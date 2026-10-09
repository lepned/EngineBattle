using ChessLibrary;

namespace WebGUI.Services
{
    /// Makes another tournament file the current one: copied in as wwwroot/tournament.json, which
    /// everything in EngineBattle reads, after the current one is backed up beside it as
    /// tournament_yyyy-MM-dd_HHmmss.json (as the Tournament creator does when it activates a file).
    /// Used by the desktop shell's File > Open Tournament (through /api/tournament/open) and by the
    /// browser's Open tournament page; the tournament page itself stays free of pickers, since it
    /// is streamed. The tournament page reads the new file on its next visit
    /// (Runner.InvalidateTournament through TournamentService.GetConfigRunner).
    public class TournamentFileService
    {
        private readonly TournamentService tournaments;
        private readonly ILogger<TournamentFileService> logger;
        private readonly object gate = new();

        public TournamentFileService(TournamentService tournaments, ILogger<TournamentFileService> logger)
        {
            this.tournaments = tournaments;
            this.logger = logger;
        }

        public static string ActivePath => Path.Combine(AppPaths.BaseDir, "wwwroot", "tournament.json");

        /// (ok, message): ok when the file is now the current tournament; the message says what
        /// happened or why not.
        public (bool Ok, string Message) Open(string path)
        {
            if (string.IsNullOrWhiteSpace(path))
                return (false, "No file given.");
            if (tournaments.IsRunning || tournaments.IsStopping)
                return (false, "A tournament is running. Stop it before you open another one.");

            string full;
            try { full = Path.GetFullPath(path); }
            catch (Exception ex) { return (false, $"Not a valid path: {path} ({ex.Message})"); }
            if (!File.Exists(full))
                return (false, $"File not found: {full}");
            // Only a file that reads as a tournament is copied into wwwroot, which the server serves
            var read = Configuration.JSON.tryReadTournamentJson(full);
            if (read.IsError)
                return (false, $"{Path.GetFileName(full)} is not a tournament file EngineBattle can read: {read.ErrorValue}");

            var active = Path.GetFullPath(ActivePath);
            if (string.Equals(full, active, StringComparison.OrdinalIgnoreCase))
                return (true, "This is already the current tournament.");

            lock (gate)
            {
                try
                {
                    string backup = null;
                    if (File.Exists(active))
                    {
                        backup = Path.Combine(Path.GetDirectoryName(active)!, $"tournament_{DateTime.Now:yyyy-MM-dd_HHmmss}.json");
                        File.Copy(active, backup, overwrite: false);
                    }
                    File.Copy(full, active, overwrite: true);
                    logger.LogInformation("Opened tournament {File} (the previous one backed up to {Backup})", full, backup ?? "-");
                    return (true, backup == null
                        ? $"Opened {Path.GetFileName(full)}."
                        : $"Opened {Path.GetFileName(full)}. The previous tournament.json is saved as {Path.GetFileName(backup)}.");
                }
                catch (Exception ex)
                {
                    logger.LogError(ex, "Could not open tournament {File}", full);
                    return (false, $"Could not open {Path.GetFileName(full)}: {ex.Message}");
                }
            }
        }
    }
}
