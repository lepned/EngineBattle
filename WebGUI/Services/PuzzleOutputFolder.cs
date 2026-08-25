using Microsoft.Extensions.Logging;

namespace WebGUI.Services;

/// <summary>
/// Resolves the folder a puzzle or ERET run writes its result files to.
///
/// The three cases are not interchangeable and the caller has to tell them apart:
/// a usable folder, a configured folder that cannot be used (an error the user must
/// see, though the run continues), and nothing configured at all (normal, quiet).
/// Both puzzle pages had their own copy of this, differing only in a log label and
/// which field they assigned the message to.
/// </summary>
public static class PuzzleOutputFolder
{
    /// <param name="configured">The FailedPuzzlesOutputFolder value from the config.</param>
    /// <param name="label">What the run produces, for the log line ("Puzzle", "ERET").</param>
    /// <returns>
    /// Path is "" when nothing should be written. Error is "" unless the user needs to
    /// be told something; the caller owns where that message is displayed.
    /// </returns>
    public static (string Path, string Error) Prepare(string configured, string label, ILogger logger)
    {
        var status = ChessLibrary.PuzzleDataUtils.ensureOutputFolder(configured);

        if (status.IsReady)
        {
            var ready = (ChessLibrary.PuzzleDataUtils.OutputFolderStatus.Ready)status;
            logger.LogInformation("{Label} results -> {Folder}{Created}",
                label, ready.path, ready.created ? " (created)" : "");
            return (ready.path, "");
        }

        if (status.IsFailed)
        {
            var failed = (ChessLibrary.PuzzleDataUtils.OutputFolderStatus.Failed)status;
            logger.LogError("Cannot use output folder {Folder}: {Message}", failed.path, failed.message);
            return ("", $"Cannot use output folder {failed.path}: {failed.message.TrimEnd('.')}. "
                        + "The run continues, but no result files will be written.");
        }

        logger.LogWarning("No FailedPuzzlesOutputFolder set - no {Label} result files will be written.", label);
        return ("", "");
    }
}
