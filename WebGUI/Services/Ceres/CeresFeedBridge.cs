#nullable enable
using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.IO;
using System.Net.Sockets;
using System.Text;
using System.Text.Json;
using System.Threading;
using System.Threading.Tasks;
using ChessLibrary;
using Microsoft.FSharp.Core;

namespace WebGUI.Services.Ceres
{
    /// <summary>
    /// Bridges a live Ceres tournament broadcast (CELT/1.0 over TCP) into EB's own feed pipeline.
    /// On <see cref="ConnectAsync"/> it opens a "global" subscription (tournament meta + end) plus one
    /// "thread" subscription per game slot, translates each CELT line to an EB contract event via
    /// <c>ChessLibrary.CeresWire</c> (stamped with gameId = threadId and source = host), and feeds the
    /// result to <see cref="JsonFeedService"/> — so the existing grid / feed-mode / catch-up / standings
    /// render a Ceres tournament with no view changes.
    ///
    /// Standings AND pentanomial come from the global <c>gameResult</c> stream (replayed in full on
    /// connect, so they catch up on any join); pentanomial is paired by thread. Per-thread <c>gameEnd</c>
    /// is not consumed (would double-count). A new tournament (different id/name) clears prior state.
    /// </summary>
    public sealed class CeresFeedBridge : IAsyncDisposable
    {
        private readonly JsonFeedService _feed;
        private readonly List<CeresStreamClient> _clients = new();
        private readonly ConcurrentDictionary<int, GameState> _games = new();
        private string _source = "";
        private string? _lastTournamentKey;   // (id|name) of the current tournament — to detect a new one

        private readonly object _sync = new();
        private CancellationTokenSource? _discoveryCts;
        private Task? _discoveryLoop;

        // Per-thread game context: engine names (to attribute move/interim, which carry only a side)
        // and the last FEN (the position before the next move, used to render moves as SAN).
        private sealed class GameState
        {
            public string White = "";
            public string Black = "";
            public string LastFen = "";
            public string MoveHistory = "";   // running "N. e4 c6 …" SAN list for the move list
        }

        public bool IsConnected { get; private set; }
        public string Status { get; private set; } = "Not connected";

        /// <summary>Raised when <see cref="IsConnected"/>/<see cref="Status"/> change (for UI refresh).</summary>
        public event Action? Changed;

        public CeresFeedBridge(JsonFeedService feed)
        {
            _feed = feed;
        }

        /// <summary>Connect to a Ceres publisher at host:port and start streaming all games into the feed.</summary>
        public async Task ConnectAsync(string host, int port)
        {
            await DisconnectAsync();
            _source = host;
            // NOTE: do NOT reset the feed here — connecting (incl. opening the grid via ?ceres=) must
            // preserve the catch-up so joining an in-progress tournament shows the games already played.
            // A fresh start is the explicit Reset button on the grid.
            SetStatus($"Connecting to {host}:{port}…", false);

            _discoveryCts = new CancellationTokenSource();
            // Global: tournament meta + tournamentEnd (gameResult frames are ignored — see OnLine).
            lock (_sync) { _clients.Add(StartClient(host, port, "global", -1)); }
            // Open the per-thread subscriptions once Ceres answers the probe. A connect made before
            // Ceres is up (NN still loading) would otherwise read no thread count and miss boards; this
            // retries until the publisher reports its (fixed) concurrency, then opens all subs and stops.
            _discoveryLoop = Task.Run(() => OpenThreadSubsWhenReadyAsync(host, port, _discoveryCts.Token));
        }

        // Wait — re-probing — until the publisher reports its thread count, then open one subscription
        // per game thread and stop. threadCount is the tournament's fixed concurrency (known as soon as
        // the publisher is up), so one successful probe suffices: a late-starting board's subscription
        // is already open and simply waits for its frames. No perpetual polling. While the server is
        // unreachable the loop keeps retrying and shows a "waiting" status (not a false "connected").
        private async Task OpenThreadSubsWhenReadyAsync(string host, int port, CancellationToken ct)
        {
            while (!ct.IsCancellationRequested)
            {
                int count = await GetThreadCountAsync(host, port, ct);   // 0 if the publisher isn't reachable yet
                if (count > 0)
                {
                    lock (_sync)
                    {
                        for (int i = 0; i < count; i++)
                            _clients.Add(StartClient(host, port, "thread", i));
                    }
                    SetStatus($"Connected to {host}:{port} — {count} game(s)", true);
                    return;
                }
                SetStatus($"Waiting for Ceres at {host}:{port}…", false);
                try { await Task.Delay(TimeSpan.FromSeconds(3), ct); }
                catch (OperationCanceledException) { break; }
            }
        }

        public async Task DisconnectAsync()
        {
            try { _discoveryCts?.Cancel(); } catch { }
            if (_discoveryLoop != null)
            {
                try { await _discoveryLoop; } catch { }
                _discoveryLoop = null;
            }
            try { _discoveryCts?.Dispose(); } catch { }
            _discoveryCts = null;

            CeresStreamClient[] clients;
            lock (_sync) { clients = _clients.ToArray(); _clients.Clear(); }
            foreach (var c in clients)
            {
                try { await c.DisposeAsync(); } catch { }
            }
            _games.Clear();
            if (IsConnected || Status != "Not connected")
                SetStatus("Not connected", false);
        }

        private CeresStreamClient StartClient(string host, int port, string scope, int threadId)
        {
            var c = new CeresStreamClient(host, port, scope, threadId,
                                          line => OnLine(threadId, line));
            c.Start();
            return c;
        }

        // Translate one CELT line (from the connection bound to threadId) and feed the contract event.
        private void OnLine(int threadId, string line)
        {
            string type = CeresWire.celtType(line);
            string gameId = threadId < 0 ? "" : threadId.ToString();
            string? json = null;

            switch (type)
            {
                case "tournamentInfo":
                    // A genuinely new tournament (different id/name) on the same long-lived bridge: clear
                    // the previous tournament's standings/boards so they don't carry over. The same
                    // identity on a transient reconnect is NOT reset (the snapshot rebuilds it); same-name
                    // re-runs still use the grid's Reset button.
                    try
                    {
                        using var doc = JsonDocument.Parse(line);
                        var root = doc.RootElement;
                        string tid = root.TryGetProperty("id", out var idEl) ? (idEl.GetString() ?? "") : "";
                        string tnm = root.TryGetProperty("name", out var nmEl) ? (nmEl.GetString() ?? "") : "";
                        string key = tid + "|" + tnm;
                        if (_lastTournamentKey != null && _lastTournamentKey != key)
                        {
                            _feed.Reset();
                            _games.Clear();
                        }
                        _lastTournamentKey = key;
                    }
                    catch { /* malformed meta — fall through and just map it */ }
                    json = Opt(CeresWire.mapTournamentInfo(_source, line));
                    break;

                case "gameStart":
                    var gs = CeresWire.mapGameStart(_source, gameId, line);
                    if (gs != null)
                    {
                        var t = gs.Value;
                        _games[threadId] = new GameState { White = t.Item1, Black = t.Item2, LastFen = t.Item3 };
                        json = t.Item4;
                    }
                    break;

                case "move":
                {
                    var st = _games.TryGetValue(threadId, out var g) ? g : new GameState();
                    var mv = CeresWire.mapMove(_source, gameId, st.White, st.Black, st.LastFen, st.MoveHistory, line);
                    if (mv != null) { json = mv.Value.Item1; st.LastFen = mv.Value.Item2; st.MoveHistory = mv.Value.Item3; }
                    break;
                }

                case "interim":
                {
                    var st = _games.TryGetValue(threadId, out var g) ? g : new GameState();
                    json = Opt(CeresWire.mapInterim(_source, gameId, st.White, st.Black, st.LastFen, line));
                    break;
                }

                // Per-thread "gameEnd" is intentionally NOT consumed: the global "gameResult" carries the
                // same payload plus the producing thread id, and drives BOTH standings and pentanomial
                // (paired by thread). Consuming the per-thread frame too would double-count / split pairing.
                case "gameResult":   // global: full standings + pentanomial; replayed in full on connect
                                     // for join-anytime catch-up. mapGameEnd tags gameId "r{threadId}".
                    json = Opt(CeresWire.mapGameEnd(_source, "", line));
                    break;

                case "tournamentEnd":
                    json = Opt(CeresWire.mapTournamentEnd(_source, line));
                    break;

                // "hello" / "subscribed" / "directoryResponse": control frames, nothing to do.
            }

            if (json != null)
                _feed.Ingest(json);
        }

        // Quick probe: connect, read the server's hello line, take its threadCount, disconnect.
        // Returns 0 when the publisher isn't reachable yet (so the caller can keep waiting rather than
        // assume a 1-game tournament).
        private static async Task<int> GetThreadCountAsync(string host, int port, CancellationToken ct)
        {
            try
            {
                using var client = new TcpClient { NoDelay = true };
                await client.ConnectAsync(host, port, ct);   // cancellable: a black-holed remote host won't stall teardown
                using var stream = client.GetStream();
                using var reader = new StreamReader(stream, Encoding.UTF8);
                var line = await reader.ReadLineAsync().WaitAsync(TimeSpan.FromSeconds(5), ct);
                if (!string.IsNullOrEmpty(line))
                {
                    using var doc = JsonDocument.Parse(line);
                    if (doc.RootElement.TryGetProperty("threadCount", out var tc)
                        && tc.TryGetInt32(out var n))
                        return Math.Max(1, n);
                }
            }
            catch { /* not reachable yet */ }
            return 0;
        }

        private static string? Opt(FSharpOption<string> o) => o == null ? null : o.Value;

        private void SetStatus(string status, bool connected)
        {
            Status = status;
            IsConnected = connected;
            try { Changed?.Invoke(); } catch { }
        }

        public async ValueTask DisposeAsync() => await DisconnectAsync();
    }
}
