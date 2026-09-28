// A scriptable UCI engine for testing EngineBattle's engine wrappers (ChessLibrary/Engine/Engine.fs).
//
// It behaves like a small, well-mannered UCI engine unless told otherwise. What it does is set the
// way a real engine is configured - through UCI options it advertises (FakeInfoCount,
// FakeGoDelayMs, FakeCrashOnGo, ...), which a test puts in EngineConfig.Options exactly as an
// engine def would - or, for what must happen before any option can arrive, through command-line
// arguments. Every line it receives is appended to the file given by --log, so a test can assert
// what EngineBattle sent and in which order.
//
// It plays no chess. Moves come from a fixed legal game (Line below): after "position startpos
// moves <prefix of Line>" the reply is the next move of Line and the PV the few after it. For any
// other position the reply is the FakeBestMove option, which the test sets to a move legal there.
//
// Arguments:
//   --log PATH          append every received line to PATH (or set FAKEUCI_LOG); the first line
//                       is "#args <the arguments>"
//   --exit-on-uci       exit with code 5 on "uci" (a binary that is not an engine)
//   --no-uciok          answer "uci" with the id lines and options but never "uciok"
//   --live-stats        advertise LogLiveStats (EngineBattle then reports HasLiveStat)
//   --ansi              colour stderr lines the way Ceres does
//   --stderr-at-start N print N lines to stderr before reading anything

using System.Text;

static class FakeUciEngine
{
    // A legal game from the start position: Ruy Lopez, closed.
    static readonly string[] Line =
    [
        "e2e4", "e7e5", "g1f3", "b8c6", "f1b5", "a7a6", "b5a4", "g8f6", "e1g1", "f8e7",
        "f1e1", "b7b5", "a4b3", "d7d6", "c2c3", "e8g8", "h2h3", "c6b8", "d2d4", "b8d7",
    ];

    const string StartFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
    static readonly object OutLock = new();
    static StreamWriter? _log;
    static readonly Dictionary<string, string> Options = new(StringComparer.OrdinalIgnoreCase)
    {
        ["Hash"] = "16", ["Threads"] = "1", ["MoveOverheadMs"] = "100", ["MultiPV"] = "1",
        ["Ponder"] = "false", ["UCI_Chess960"] = "false", ["UCI_ShowWDL"] = "false",
        ["WeightsFile"] = "", ["Style"] = "Normal", ["LogLiveStats"] = "false",
        ["FakeBestMove"] = "", ["FakeInfoCount"] = "3", ["FakeGoDelayMs"] = "0",
        ["FakeInfinite"] = "false", ["FakeReadyDelayMs"] = "0", ["FakeNoReadyOk"] = "false",
        ["FakeFatalOnReady"] = "false", ["FakeExitOnReady"] = "false", ["FakeCrashOnGo"] = "false",
        ["FakeBoundLine"] = "false", ["FakeBestMoveNone"] = "false", ["FakeIgnoreQuit"] = "false",
        ["FakeStderrOnReady"] = "0", ["FakeMoveStats"] = "false", ["FakeFirstGoDelayMs"] = "0",
        ["FakeNoPv"] = "false",
    };

    static bool _liveStats;
    static bool _ansi;
    static bool _firstGoDone;
    static List<string> _movesFromStart = [];
    static bool _startpos = true;
    static CancellationTokenSource? _searchCts;
    static Task? _search;

    static int Main(string[] args)
    {
        bool exitOnUci = false, noUciOk = false;
        int stderrAtStart = 0;
        // FAKEUCI_LOG serves a test that must start the engine with no arguments at all.
        var envLog = Environment.GetEnvironmentVariable("FAKEUCI_LOG");
        if (!string.IsNullOrEmpty(envLog)) _log = new StreamWriter(envLog, append: true) { AutoFlush = true };
        for (int i = 0; i < args.Length; i++)
        {
            switch (args[i])
            {
                case "--log": _log = new StreamWriter(args[++i], append: true) { AutoFlush = true }; break;
                case "--exit-on-uci": exitOnUci = true; break;
                case "--no-uciok": noUciOk = true; break;
                case "--live-stats": _liveStats = true; break;
                case "--ansi": _ansi = true; break;
                case "--stderr-at-start": stderrAtStart = int.Parse(args[++i]); break;
            }
        }
        // First log line: the arguments the engine was started with, as "#args ...".
        _log?.WriteLine("#args " + string.Join(' ', args));
        for (int i = 1; i <= stderrAtStart; i++) Err($"startup line {i}");

        string? line;
        while ((line = Console.In.ReadLine()) != null)
        {
            _log?.WriteLine(line);
            var cmd = line.Trim();
            if (cmd.Length == 0) continue;
            var word = cmd.Split(' ', 2)[0];
            switch (word)
            {
                case "uci":
                    if (exitOnUci) return 5;
                    Uci(noUciOk);
                    break;
                case "isready": IsReady(); break;
                case "setoption": SetOption(cmd); break;
                case "ucinewgame": break;
                case "position": Position(cmd); break;
                case "go": Go(cmd); break;
                case "stop": StopSearch(); break;
                case "ponderhit": break;
                case "quit":
                    if (Bool("FakeIgnoreQuit")) break;
                    StopSearch();
                    return 0;
            }
        }
        return 0;
    }

    static void Out(string s) { lock (OutLock) { Console.Out.WriteLine(s); Console.Out.Flush(); } }
    static void Err(string s)
    {
        lock (OutLock) { Console.Error.WriteLine(_ansi ? $"\u001b[0;93m{s}\u001b[m" : s); Console.Error.Flush(); }
    }
    static bool Bool(string name) => string.Equals(Options[name], "true", StringComparison.OrdinalIgnoreCase);
    static int Int(string name) => int.TryParse(Options[name], out var v) ? v : 0;

    static void Uci(bool noUciOk)
    {
        Out("id name FakeUciEngine 1.0");
        Out("id author EngineBattle tests");
        Out("option name Hash type spin default 16 min 1 max 1024");
        Out("option name Threads type spin default 1 min 1 max 64");
        Out("option name MoveOverheadMs type spin default 100 min 0 max 5000");
        Out("option name MultiPV type spin default 1 min 1 max 8");
        Out("option name Ponder type check default false");
        Out("option name UCI_Chess960 type check default false");
        Out("option name UCI_ShowWDL type check default false");
        Out("option name WeightsFile type string default <empty>");
        Out("option name Style type combo default Normal var Solid var Normal var Risky");
        Out("option name Clear Hash type button");
        if (_liveStats) Out("option name LogLiveStats type check default false");
        Out("option name FakeBestMove type string default <empty>");
        Out("option name FakeInfoCount type spin default 3 min 0 max 1000000");
        Out("option name FakeGoDelayMs type spin default 0 min 0 max 600000");
        Out("option name FakeInfinite type check default false");
        Out("option name FakeReadyDelayMs type spin default 0 min 0 max 600000");
        Out("option name FakeNoReadyOk type check default false");
        Out("option name FakeFatalOnReady type check default false");
        Out("option name FakeExitOnReady type check default false");
        Out("option name FakeCrashOnGo type check default false");
        Out("option name FakeBoundLine type check default false");
        Out("option name FakeBestMoveNone type check default false");
        Out("option name FakeIgnoreQuit type check default false");
        Out("option name FakeStderrOnReady type spin default 0 min 0 max 100000");
        Out("option name FakeMoveStats type check default false");
        Out("option name FakeFirstGoDelayMs type spin default 0 min 0 max 600000");
        Out("option name FakeNoPv type check default false");
        if (!noUciOk) Out("uciok");
    }

    static void IsReady()
    {
        // isready is answered even while a search runs, as the UCI protocol requires.
        for (int i = 1; i <= Int("FakeStderrOnReady"); i++) Err($"stderr line {i}");
        if (Bool("FakeExitOnReady")) Environment.Exit(2);
        if (Bool("FakeFatalOnReady")) { Out("info string Cannot initialize engine: fake failure"); return; }
        if (Bool("FakeNoReadyOk")) return;
        var delay = Int("FakeReadyDelayMs");
        if (delay > 0) Thread.Sleep(delay);
        Out("readyok");
    }

    static void SetOption(string cmd)
    {
        // setoption name <name with spaces> [value <value with spaces>]
        var nameAt = cmd.IndexOf(" name ", StringComparison.Ordinal);
        if (nameAt < 0) return;
        var rest = cmd[(nameAt + 6)..];
        var valueAt = rest.IndexOf(" value ", StringComparison.Ordinal);
        var name = (valueAt < 0 ? rest : rest[..valueAt]).Trim();
        var value = valueAt < 0 ? "" : rest[(valueAt + 7)..].Trim();
        if (Options.ContainsKey(name)) Options[name] = value;
    }

    static void Position(string cmd)
    {
        var movesAt = cmd.IndexOf(" moves ", StringComparison.Ordinal);
        _movesFromStart = movesAt < 0 ? [] : cmd[(movesAt + 7)..].Split(' ', StringSplitOptions.RemoveEmptyEntries).ToList();
        // EngineBattle's analysis wrapper spells the start position out as a FEN; treat it as startpos.
        _startpos = cmd.StartsWith("position startpos", StringComparison.Ordinal)
                    || cmd.StartsWith("position fen " + StartFen, StringComparison.Ordinal);
    }

    /// The reply and the PV after it: from Line when the position is a prefix of it, else FakeBestMove.
    static (string best, string[] pv) Plan()
    {
        var fixedMove = Options["FakeBestMove"];
        if (!string.IsNullOrEmpty(fixedMove) && fixedMove != "<empty>") return (fixedMove, [fixedMove]);
        var n = _movesFromStart.Count;
        bool onLine = _startpos && n < Line.Length && _movesFromStart.SequenceEqual(Line.Take(n));
        if (!onLine) return ("0000", ["0000"]);
        var pv = Line.Skip(n).Take(3).ToArray();
        return (pv[0], pv);
    }

    static void Go(string cmd)
    {
        StopSearch();
        if (Bool("FakeCrashOnGo")) { Err("fake crash in search"); Environment.Exit(3); }
        var cts = new CancellationTokenSource();
        _searchCts = cts;
        bool infinite = Bool("FakeInfinite") || cmd.Contains(" infinite") || cmd.Contains(" ponder");
        int delay = Int("FakeGoDelayMs");
        if (!_firstGoDone) { delay += Int("FakeFirstGoDelayMs"); _firstGoDone = true; }
        _search = Task.Run(() => Search(cts.Token, infinite, delay));
    }

    static void Search(CancellationToken token, bool infinite, int delayMs)
    {
        var (best, pv) = Plan();
        int multiPv = Math.Max(1, Int("MultiPV"));
        int count = Int("FakeInfoCount");
        for (int d = 1; d <= count && !token.IsCancellationRequested; d++)
        {
            for (int k = 1; k <= multiPv; k++)
            {
                var sb = new StringBuilder();
                sb.Append($"info depth {d} seldepth {d + 2}");
                if (multiPv > 1) sb.Append($" multipv {k}");
                sb.Append($" score cp {20 + d - k} wdl 400 450 150 nodes {d * 1000} nps {d * 50000} tbhits 0 time {d}");
                if (!Bool("FakeNoPv")) sb.Append($" pv {string.Join(' ', pv)}");
                Out(sb.ToString());
            }
        }
        if (Bool("FakeBoundLine") && !token.IsCancellationRequested)
            Out($"info depth {count + 1} seldepth {count + 3} score cp 99 upperbound nodes {count * 1000 + 1} nps 1 tbhits 0 time 1 pv {pv[0]}");
        if (Bool("FakeMoveStats"))
        {
            Out($"info string {best}  (322 ) N:     900 (+ 0) (P: 61.00%) (WL:  0.10000) (D: 0.300) (M: 60.0) (Q:  0.10000) (U: 0.01000) (S:  0.11000) (V:  0.0900)");
            Out("info string d2d4  (293 ) N:     100 (+ 0) (P: 30.00%) (WL:  0.05000) (D: 0.300) (M: 60.0) (Q:  0.05000) (U: 0.02000) (S:  0.07000) (V:  0.0400)");
            Out("info string node  (  20) N:    1000 (+ 0) (P: 100.0%) (WL:  0.09000) (D: 0.300) (M: 60.0) (Q:  0.09000) (V:  0.0800)");
        }
        // Wait out the delay, or for "stop" when searching infinitely; stop always ends the wait.
        var until = DateTime.UtcNow.AddMilliseconds(delayMs);
        while (!token.IsCancellationRequested && (infinite || DateTime.UtcNow < until)) Thread.Sleep(2);
        if (Bool("FakeBestMoveNone")) { Out("bestmove (none)"); return; }
        Out(pv.Length > 1 ? $"bestmove {best} ponder {pv[1]}" : $"bestmove {best}");
    }

    static void StopSearch()
    {
        var cts = _searchCts;
        if (cts == null) return;
        cts.Cancel();
        _search?.Wait();
        _searchCts = null;
    }
}
