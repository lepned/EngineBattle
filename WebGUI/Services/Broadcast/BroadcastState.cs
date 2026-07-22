#nullable enable
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;
using System.Threading.Tasks;
using ChessLibrary;

namespace WebGUI.Services.Broadcast
{
    /// <summary>One half-move of a broadcast game with the position it leads to.
    /// Eval is the lichess server evaluation from the PGN comment (e.g. "0.18" or "#4"), if any.</summary>
    public sealed record BroadcastPly(
        string San, string Fen, string FromSq, string ToSq, string Color, string Clock, string Eval);

    /// <summary>One game of a broadcast round, updated as the stream re-sends its PGN.</summary>
    public sealed class BroadcastGame
    {
        public string Key { get; init; } = "";      // Round|White|Black
        public string Round { get; set; } = "";
        public string White { get; set; } = "";
        public string Black { get; set; } = "";
        public string WhiteElo { get; set; } = "";
        public string BlackElo { get; set; } = "";
        public string Result { get; set; } = "*";
        public List<string> SanMoves { get; set; } = new();
        /// <summary>Per-ply board states from replaying the PGN; empty if replay failed.</summary>
        public List<BroadcastPly> Plies { get; set; } = new();
        public string StartFen { get; set; } = "";
        public string WhiteClock { get; set; } = "";
        public string BlackClock { get; set; } = "";
        public string RawPgn { get; set; } = "";
        /// <summary>Parsed PGN of the latest update (for board replay, e.g. PlayPgnToPly).</summary>
        public PGNTypes.PgnGame? Parsed { get; set; }
        public DateTime LastUpdateUtc { get; set; }

        public string LastMoveSan => SanMoves.Count > 0 ? SanMoves[^1] : "";
        public bool Finished => Result is "1-0" or "0-1" or "1/2-1/2";
        public string CurrentFen => Plies.Count > 0 ? Plies[^1].Fen : StartFen;
    }

    /// <summary>An entry of the official-broadcasts picker. TourId/RoundName group the
    /// rounds of one tournament for the round switcher.</summary>
    public sealed record OfficialRound(string RoundId, string Label, bool Ongoing, string TourId, string RoundName);

    /// <summary>
    /// Holds the live state of one lichess broadcast round: starts/stops the streaming
    /// client, parses each re-sent game PGN (ChessLibrary parser), diffs it against the
    /// known state and raises events. Reconnects and mid-join both work naturally because
    /// the stream always carries each game's full PGN.
    /// </summary>
    public sealed class BroadcastState : IAsyncDisposable
    {
        private readonly object _lock = new();
        private readonly Dictionary<string, BroadcastGame> _games = new();
        private LichessBroadcastClient? _client;

        public string RoundId { get; private set; } = "";
        public string Status { get; private set; } = "idle";
        public bool IsRunning { get; private set; }

        /// <summary>Raised on any change (status or game update). May fire on a background thread.</summary>
        public event Action? Changed;
        /// <summary>Raised when a game gained moves or changed result: (game, newMoveCount).</summary>
        public event Action<BroadcastGame, int>? GameUpdated;

        public IReadOnlyList<BroadcastGame> Games
        {
            get { lock (_lock) return _games.Values.OrderBy(g => g.Key, StringComparer.Ordinal).ToList(); }
        }

        /// <summary>Accepts a bare round id or any lichess broadcast URL
        /// (https://lichess.org/broadcast/{tour}/{round}/{roundId}[/...]).</summary>
        public static string? ExtractRoundId(string input)
        {
            if (string.IsNullOrWhiteSpace(input)) return null;
            var s = input.Trim();
            if (!s.Contains('/')) return s;
            var noQuery = s.Split('?', '#')[0];
            var segments = noQuery.Split('/', StringSplitOptions.RemoveEmptyEntries);
            var idx = Array.FindIndex(segments, seg => seg.Equals("broadcast", StringComparison.OrdinalIgnoreCase));
            // .../broadcast/{tourSlug}/{roundSlug}/{roundId}
            if (idx >= 0 && segments.Length > idx + 3) return segments[idx + 3];
            return segments.LastOrDefault();
        }

        public void Start(string roundIdOrUrl)
        {
            var roundId = ExtractRoundId(roundIdOrUrl);
            if (string.IsNullOrEmpty(roundId)) { SetStatus("invalid round id/url"); return; }

            _ = StopAsync().AsTask();
            lock (_lock) _games.Clear();
            RoundId = roundId;
            IsRunning = true;
            _client = new LichessBroadcastClient(roundId, OnGamePgn, SetStatus);
            _client.Start();
            SetStatus("connecting");
        }

        public async ValueTask StopAsync()
        {
            var client = _client;
            _client = null;
            IsRunning = false;
            if (client != null)
            {
                await client.DisposeAsync();
                SetStatus("stopped");
            }
        }

        private void SetStatus(string status)
        {
            Status = status;
            try { Changed?.Invoke(); } catch { }
        }

        private static readonly Regex ClkRegex = new(@"\[%clk\s+([0-9:.]+)\]", RegexOptions.Compiled);
        private static readonly Regex EvalRegex = new(@"\[%eval\s+([^\]\s]+)\]", RegexOptions.Compiled);

        private void OnGamePgn(string pgn)
        {
            PGNTypes.PgnGame parsed;
            try { parsed = FullPGNParser.parseFullPgnGame(pgn); }
            catch { return; }

            var meta = parsed.GameMetaData;
            if (string.IsNullOrEmpty(meta.White) && string.IsNullOrEmpty(meta.Black)) return;

            var key = $"{meta.Round}|{meta.White}|{meta.Black}";
            var san = parsed.Mainline.Select(m => m.San).Where(s => !string.IsNullOrEmpty(s)).ToList();

            // Clocks: last [%clk] comment per color.
            string whiteClock = "", blackClock = "";
            foreach (var move in parsed.Mainline)
            {
                if (string.IsNullOrEmpty(move.Comment)) continue;
                var m = ClkRegex.Match(move.Comment);
                if (!m.Success) continue;
                if (move.Color == "b") blackClock = m.Groups[1].Value; else whiteClock = m.Groups[1].Value;
            }

            string Tag(string k) => meta.OtherTags.FirstOrDefault(t => t.Key.Equals(k, StringComparison.OrdinalIgnoreCase))?.Value ?? "";

            // Replay the game to get per-ply FENs and from/to squares for board rendering.
            var startFen = string.IsNullOrWhiteSpace(meta.Fen) ? Chess.startPos : meta.Fen;
            var plies = new List<BroadcastPly>();
            try
            {
                var board = new Chess.Board();
                board.ResetBoardStateFromFen(startFen);
                board.LoadPGNGameWithVariations(parsed);
                int plyIdx = 0;
                foreach (var maf in board.MovesAndFenPlayed)
                {
                    if (string.IsNullOrEmpty(maf.ShortSan)) continue; // initial-position sentinel
                    var comment = plyIdx < parsed.Mainline.Count ? parsed.Mainline[plyIdx].Comment : "";
                    var clk = string.IsNullOrEmpty(comment) ? Match.Empty : ClkRegex.Match(comment);
                    var ev = string.IsNullOrEmpty(comment) ? Match.Empty : EvalRegex.Match(comment);
                    plies.Add(new BroadcastPly(
                        maf.ShortSan, maf.FenAfterMove, maf.Move.FromSq, maf.Move.ToSq, maf.Move.Color,
                        clk.Success ? clk.Groups[1].Value : "",
                        ev.Success ? ev.Groups[1].Value : ""));
                    plyIdx++;
                }
            }
            catch { plies.Clear(); /* broadcast PGN glitch — keep SAN-only state */ }

            // Clocks (and lichess server evals) are extracted above; strip the raw
            // [%clk]/[%eval] comments so move-list consumers render clean SAN.
            foreach (var move in parsed.Mainline) move.Comment = "";

            BroadcastGame game;
            int newMoves;
            bool changed;
            lock (_lock)
            {
                if (!_games.TryGetValue(key, out var existing))
                {
                    existing = new BroadcastGame { Key = key };
                    _games[key] = existing;
                }
                game = existing;

                newMoves = Math.Max(0, san.Count - game.SanMoves.Count);
                changed = newMoves > 0 || game.Result != meta.Result || game.SanMoves.Count != san.Count;

                game.Round = meta.Round;
                game.White = meta.White;
                game.Black = meta.Black;
                game.WhiteElo = Tag("WhiteElo");
                game.BlackElo = Tag("BlackElo");
                game.Result = string.IsNullOrEmpty(meta.Result) ? "*" : meta.Result;
                game.SanMoves = san;
                game.Plies = plies;
                game.Parsed = parsed;
                game.StartFen = startFen;
                game.WhiteClock = whiteClock;
                game.BlackClock = blackClock;
                game.RawPgn = pgn;
                game.LastUpdateUtc = DateTime.UtcNow;
            }

            if (changed)
            {
                try { GameUpdated?.Invoke(game, newMoves); } catch { }
                try { Changed?.Invoke(); } catch { }
            }
        }

        public async ValueTask DisposeAsync() => await StopAsync();

        private static readonly System.Net.Http.HttpClient ListHttp = CreateListHttp();

        private static System.Net.Http.HttpClient CreateListHttp()
        {
            var client = new System.Net.Http.HttpClient { Timeout = TimeSpan.FromSeconds(15) };
            client.DefaultRequestHeaders.UserAgent.ParseAdd("EngineBattle/1.0 (broadcast viewer)");
            return client;
        }

        /// <summary>Fetches the official broadcast list (ndjson) for the round picker.
        /// Returns tour+round entries, ongoing rounds first. Empty list on any failure.</summary>
        public static async Task<List<OfficialRound>> FetchOfficialRoundsAsync(int tournaments = 10)
        {
            var result = new List<OfficialRound>();
            try
            {
                var body = await ListHttp.GetStringAsync($"https://lichess.org/api/broadcast?nb={tournaments}");
                foreach (var line in body.Split('\n', StringSplitOptions.RemoveEmptyEntries))
                {
                    try
                    {
                        using var doc = System.Text.Json.JsonDocument.Parse(line);
                        var root = doc.RootElement;
                        var tour = root.GetProperty("tour");
                        var tourName = tour.GetProperty("name").GetString() ?? "";
                        var tourId = tour.TryGetProperty("id", out var tid) ? tid.GetString() ?? "" : "";
                        if (!root.TryGetProperty("rounds", out var rounds)) continue;
                        foreach (var round in rounds.EnumerateArray())
                        {
                            var id = round.GetProperty("id").GetString() ?? "";
                            var name = round.TryGetProperty("name", out var n) ? n.GetString() ?? "" : "";
                            var ongoing = round.TryGetProperty("ongoing", out var o) && o.GetBoolean();
                            var finished = round.TryGetProperty("finished", out var f) && f.GetBoolean();
                            if (id.Length == 0) continue;
                            var label = $"{tourName} — {name}" + (ongoing ? "  (live)" : finished ? "  (finished)" : "");
                            result.Add(new OfficialRound(id, label, ongoing, tourId, name));
                        }
                    }
                    catch { /* one malformed line must not kill the list */ }
                }
            }
            catch { /* offline or lichess down — picker just stays empty */ }
            return result.OrderByDescending(r => r.Ongoing).ToList();
        }
    }
}
