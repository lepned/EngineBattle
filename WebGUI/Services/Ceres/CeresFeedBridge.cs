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
    /// Standings come from per-thread <c>gameEnd</c> frames; the duplicate global <c>gameResult</c>
    /// frames are ignored to avoid double-counting.
    /// </summary>
    public sealed class CeresFeedBridge : IAsyncDisposable
    {
        private readonly JsonFeedService _feed;
        private readonly List<CeresStreamClient> _clients = new();
        private readonly ConcurrentDictionary<int, (string white, string black)> _names = new();
        private string _source = "";

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
            SetStatus($"Connecting to {host}:{port}…", false);

            int threadCount = await GetThreadCountAsync(host, port);

            // Global: tournament meta + tournamentEnd (gameResult frames are ignored — see OnLine).
            _clients.Add(StartClient(host, port, "global", -1));
            // One subscription per game thread/slot.
            for (int i = 0; i < Math.Max(1, threadCount); i++)
                _clients.Add(StartClient(host, port, "thread", i));

            SetStatus($"Connected to {host}:{port} — {threadCount} game(s)", true);
        }

        public async Task DisconnectAsync()
        {
            var clients = _clients.ToArray();
            _clients.Clear();
            foreach (var c in clients)
            {
                try { await c.DisposeAsync(); } catch { }
            }
            _names.Clear();
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
                    json = Opt(CeresWire.mapTournamentInfo(_source, line));
                    break;

                case "gameStart":
                    var gs = CeresWire.mapGameStart(_source, gameId, line);
                    if (gs != null)
                    {
                        var t = gs.Value;
                        _names[threadId] = (t.Item1, t.Item2);
                        json = t.Item3;
                    }
                    break;

                case "move":
                {
                    var (w, b) = _names.TryGetValue(threadId, out var nm) ? nm : ("", "");
                    json = Opt(CeresWire.mapMove(_source, gameId, w, b, line));
                    break;
                }

                case "interim":
                {
                    var (w, b) = _names.TryGetValue(threadId, out var nm) ? nm : ("", "");
                    json = Opt(CeresWire.mapInterim(_source, gameId, w, b, line));
                    break;
                }

                case "gameEnd":
                    json = Opt(CeresWire.mapGameEnd(_source, gameId, line));
                    break;

                case "tournamentEnd":
                    json = Opt(CeresWire.mapTournamentEnd(_source, line));
                    break;

                // "gameResult" (global): ignored — the per-thread gameEnd already records the result.
                // "hello" / "subscribed" / "directoryResponse": control frames, nothing to do.
            }

            if (json != null)
                _feed.Ingest(json);
        }

        // Quick probe: connect, read the server's hello line, take its threadCount, disconnect.
        private static async Task<int> GetThreadCountAsync(string host, int port)
        {
            try
            {
                using var client = new TcpClient { NoDelay = true };
                await client.ConnectAsync(host, port);
                using var stream = client.GetStream();
                using var reader = new StreamReader(stream, Encoding.UTF8);
                var line = await reader.ReadLineAsync().WaitAsync(TimeSpan.FromSeconds(5));
                if (!string.IsNullOrEmpty(line))
                {
                    using var doc = JsonDocument.Parse(line);
                    if (doc.RootElement.TryGetProperty("threadCount", out var tc)
                        && tc.TryGetInt32(out var n))
                        return Math.Max(1, n);
                }
            }
            catch { /* fall through to default */ }
            return 1;
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
