#nullable enable
using static ChessLibrary.Chess;

namespace WebGUI.Services;

public record OpeningInfo(string ECO, string Name);

public class OpeningExplorerService
{
    private Dictionary<ulong, OpeningInfo>? _byHash;
    private readonly ILogger<OpeningExplorerService> _logger;

    public OpeningExplorerService(IWebHostEnvironment env, ILogger<OpeningExplorerService> logger)
    {
        _logger = logger;
        Task.Run(() => Load(env.WebRootPath));
    }

    private void Load(string webRootPath)
    {
        var path = Path.Combine(webRootPath, "data", "openings.tsv");
        if (!File.Exists(path))
        {
            _logger.LogWarning("Opening data file not found at {Path}", path);
            return;
        }

        var lines = File.ReadAllLines(path);
        var dict = new Dictionary<ulong, OpeningInfo>();

        for (int i = 1; i < lines.Length; i++) // skip header
        {
            var line = lines[i];
            if (string.IsNullOrWhiteSpace(line)) continue;

            var parts = line.Split('\t');
            if (parts.Length < 3) continue;

            var eco = parts[0].Trim();
            var name = parts[1].Trim();
            var pgn = parts[2].Trim();

            try
            {
                var board = new Board();
                var tokens = pgn.Split(' ', StringSplitOptions.RemoveEmptyEntries);
                foreach (var token in tokens)
                {
                    if (token.Length > 0 && char.IsDigit(token[0]) && token.Contains('.'))
                        continue;
                    board.PlaySanMove(token);
                }

                var hash = board.PositionHash();
                dict[hash] = new OpeningInfo(eco, name);
            }
            catch (Exception ex)
            {
                _logger.LogDebug("Failed to parse opening '{Name}': {Error}", name, ex.Message);
            }
        }

        _logger.LogInformation("Loaded {Count} openings from {Path}", dict.Count, path);
        _byHash = dict;
    }

    public OpeningInfo? Lookup(ulong hash)
    {
        var dict = _byHash;
        if (dict == null)
            return null; // still loading
        if (dict.TryGetValue(hash, out var info))
            return info;
        return null;
    }

    public OpeningInfo? Lookup(Board board)
    {
        var hash = _byHash;
        if (hash == null)
            return null; // still loading

        var uciMoves = board.UciMovesPlayed;

        if (uciMoves.Count > 0)
        {
            // Replay moves on a scratch board, check hash at each step, keep deepest match
            var scratch = new Board();
            var startFen = board.StartPosition;
            if (!string.IsNullOrEmpty(startFen))
                scratch.LoadFen(startFen);
            OpeningInfo? best = null;

            // Check start position itself (e.g. pasted FEN that is a known opening)
            if (hash.TryGetValue(scratch.PositionHash(), out var startInfo))
                best = startInfo;

            foreach (var move in uciMoves)
            {
                scratch.PlayUciMove(move);
                if (hash.TryGetValue(scratch.PositionHash(), out var info))
                    best = info;
            }

            if (best != null)
                return best;
        }

        // Check current position hash directly (handles pasted FEN with no moves)
        if (hash.TryGetValue(board.PositionHash(), out var directInfo))
            return directInfo;

        return null;
    }
}
