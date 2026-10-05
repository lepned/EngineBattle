using ChessLibrary;

namespace WebGUI.Services;

/// A loaded PGN whose moves the board could not all play: warn, naming the game and the first one.
public static class PgnLoadLog
{
    public static void WarnSkipped(Chess.Board board, PGNTypes.PgnGame game, ILogger logger) =>
        WarnSkipped(board.SkippedPgnMoves, game, logger);

    public static void WarnSkipped(IReadOnlyList<string> skipped, PGNTypes.PgnGame game, ILogger logger)
    {
        if (skipped.Count == 0) return;
        var m = game.GameMetaData;
        logger.LogWarning("PGN {White} - {Black} round {Round}: illegal move {First}, {Count} move(s) not loaded",
            m.White, m.Black, m.Round, skipped[0], skipped.Count);
    }
}
