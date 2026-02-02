module ChessLibrary.EPDExtractor

open System.IO
open ChessLibrary.EPDTypes
open ChessLibrary.PGNTypes
open ChessLibrary.TestsTypes

let extractEDPDetails (input: string) = MoveParser.parseLine input

let mapEPDToPGN (epd:EPDEntry) n : PgnGame =
  let header = { Key = "Opening"; Value = epd.Id |> Option.defaultValue ""}
  let gameData =
    if header.Value = "" then
      {GameMetadata.Empty with Fen = epd.FEN}
    else
      {GameMetadata.Empty with Fen = epd.FEN; OtherTags=[header] }
  {
    GameNumber = n
    GameMetaData = gameData
    Mainline = ResizeArray()
    RootVariations = ResizeArray()
    Comments = ""
    Fen = epd.FEN
    Raw = epd.RawInput
  }

let mapChessRecordToEPD (record: ChessRecord) n : EPDEntry =
  let fen = record.FEN
  let other = { RawInput = record.ToString(); FEN = fen; BestMove = None; AvoidMove = None; Id = Some (n.ToString()); Other = record.ToString() |> Some }
  other

let readEPDs (path:string) =
  let content = File.ReadAllLines path
  seq { for line in content do
            if line.StartsWith("##") |> not then
                extractEDPDetails line }
  |> Seq.choose id

let getOpeningPGNFromEPD path =
  readEPDs path
  |> Seq.mapi (fun idx el -> mapEPDToPGN el (idx + 1))

let parseEPDFile (pgnFilePath: string): seq<PgnGame> =
  let openings = getOpeningPGNFromEPD pgnFilePath
  openings
