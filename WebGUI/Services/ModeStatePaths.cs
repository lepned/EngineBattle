#nullable enable
using System.IO;
using ChessLibrary;

namespace WebGUI.Services
{
    /// <summary>
    /// Where a cup, Swiss or ladder keeps its state, by ChessLibrary's StatePaths - the rule the
    /// runner uses (next to the PGN unless configured) - so no view shows a file the run does not write.
    /// </summary>
    public static class ModeStatePaths
    {
        public static string Cup(string contentRoot, TypesDef.Tournament.Tournament? tournament) =>
            StatePaths.path(contentRoot, tournament ?? TypesDef.Tournament.Tournament.Empty, StatePaths.Mode.Cup);

        public static string Swiss(string contentRoot, TypesDef.Tournament.Tournament? tournament) =>
            StatePaths.path(contentRoot, tournament ?? TypesDef.Tournament.Tournament.Empty, StatePaths.Mode.Swiss);

        public static string Ladder(string contentRoot, TypesDef.Tournament.Tournament? tournament) =>
            StatePaths.path(contentRoot, tournament ?? TypesDef.Tournament.Tournament.Empty, StatePaths.Mode.Ladder);

        /// <summary>The state file made ready before a resume popup reads it, as the run will find it:
        /// a paused tournament's shared file taken over, a state whose PGN has no games set aside.</summary>
        public static string Prepare(string contentRoot, TypesDef.Tournament.Tournament tournament, StatePaths.Mode mode, Microsoft.Extensions.Logging.ILogger logger)
        {
            var prepared = StatePaths.prepare(contentRoot, tournament, mode);
            if (prepared.Item2 != null)
                logger.LogInformation("{Text}", prepared.Item2.Value);
            return prepared.Item1;
        }

        /// <summary>wwwroot/tournament.json as written, for a view the page gives no path: a plain
        /// read (Runner.Tournament() can reload and use up the cup's one-shot path override).</summary>
        public static TypesDef.Tournament.Tournament? Configured(string contentRoot)
        {
            try
            {
                var read = Configuration.JSON.tryReadTournamentJson(Path.Combine(contentRoot, "wwwroot", "tournament.json"));
                return read.IsOk ? read.ResultValue : null;
            }
            catch
            {
                return null;
            }
        }
    }
}
