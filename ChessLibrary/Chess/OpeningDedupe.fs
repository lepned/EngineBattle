/// An opening file without its duplicates: an opening with the same start and moves as an earlier one
/// (the tournament's rule, Hash.computeOpeningHashFromGame) is left out; the rest is written as it was.
module ChessLibrary.OpeningDedupe

open System
open System.Collections.Generic
open System.IO
open ChessLibrary.PGNTypes

type Summary =
    { Openings: int
      Kept: int
      Removed: int
      /// Lines found more than once.
      Groups: int }

/// The openings kept, the first of each line, and the summary.
let unique (games: PgnGame seq) =
    let seen = HashSet<_>()
    let repeated = HashSet<_>()
    let kept = ResizeArray<PgnGame>()
    let mutable total = 0
    for g in games do
        total <- total + 1
        let key = ChessUtilities.Hash.computeOpeningHashFromGame g
        if seen.Add key then kept.Add g else repeated.Add key |> ignore
    kept, { Openings = total; Kept = kept.Count; Removed = total - kept.Count; Groups = repeated.Count }

/// Writes `inPath` without its duplicate openings to `outPath` (through a temporary file: a failed
/// run leaves no half file). The input is never written over.
let removeDuplicates (inPath: string) (outPath: string) : Summary =
    if String.Equals(Path.GetFullPath inPath, Path.GetFullPath outPath, StringComparison.OrdinalIgnoreCase) then
        invalidArg "outPath" "the output would write over the input"
    let kept, summary = unique (FullPGNParser.parsePgnFileWithRaw inPath)
    let full = Path.GetFullPath outPath
    let dir = Path.GetDirectoryName full
    if not (String.IsNullOrEmpty dir) then Directory.CreateDirectory dir |> ignore
    // the parser keeps each game's text with \n line endings: the file is written with \n throughout
    let temp = full + ".tmp"
    try
        File.WriteAllText(temp, kept |> Seq.map (fun g -> g.Raw.TrimEnd() + "\n\n") |> String.concat "")
        File.Move(temp, full, true)
    finally
        if File.Exists temp then File.Delete temp
    summary
