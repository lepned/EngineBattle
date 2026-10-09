module PgnOutputTests

open System
open System.IO
open Xunit
open ChessLibrary
open ChessLibrary.PGNTypes
open ChessLibrary.TimeControlTypes

// ---------------------------------------------------------------------------
// The PGN EngineBattle writes for a played game, as the standard wants it: escaped tag values,
// SetUp (and Variant for Chess960) with a FEN, "12... Nf6" for Black's moves, castling with O.
// ---------------------------------------------------------------------------

let private header (metadata: GameMetadata) =
  let path = Path.Combine(Path.GetTempPath(), "eb-pgnout-" + Guid.NewGuid().ToString "N" + ".pgn")
  do
    use writer = new StreamWriter(path)
    PGNWriter.writePGNHeaderSection writer metadata
    writer.Write("1. e4 *\n")
  path, File.ReadAllText path

[<Fact>]
let ``a quote or backslash in a tag value is escaped, and read back as itself`` () =
  let path, text = header { GameMetadata.Empty with Event = "Cup \"A\" \\ final"; White = "W"; Black = "B"; Result = "*" }
  Assert.Contains("[Event \"Cup \\\"A\\\" \\\\ final\"]", text)
  let game = FullPGNParser.parsePgnFile path |> Seq.head
  Assert.Equal("Cup \"A\" \\ final", game.GameMetaData.Event)

[<Fact>]
let ``a game from a FEN has SetUp, and a Chess960 one its Variant, before the FEN`` () =
  let fen = "bqrknbnr/pppppppp/8/8/8/8/PPPPPPPP/RQNBKRBN w FAhc - 0 1"
  let _, text = header { GameMetadata.Empty with Fen = fen; OtherTags = [ { Key = "Variant"; Value = "Chess960" } ] }
  Assert.Contains($"[SetUp \"1\"]\n[Variant \"Chess960\"]\n[FEN \"{fen}\"]", text.Replace("\r\n", "\n"))
  let _, standard = header { GameMetadata.Empty with Fen = "8/8/8/4k3/8/8/4P3/4K3 w - - 0 1" }
  Assert.Contains("[SetUp \"1\"]", standard)
  Assert.DoesNotContain("[Variant", standard)
  let _, noFen = header GameMetadata.Empty
  Assert.DoesNotContain("[SetUp", noFen)

[<Fact>]
let ``a played move is written 12... Nf6 for Black and castles with O`` () =
  let board = Chess.Board()
  board.LoadFen()
  board.PlaySanMove "e4"
  Assert.Equal("1. e4", board.SanMoveNumberString "e4")
  board.PlaySanMove "e5"
  Assert.Equal("1... e5", board.SanMoveNumberString "e5")
  board.LoadFen "r3k2r/8/8/8/8/8/8/R3K2R b KQkq - 0 30"
  board.PlaySanMove "0-0-0"
  Assert.Equal("30... O-O-O", board.SanMoveNumberString "0-0-0")
  board.PlaySanMove "0-0"
  Assert.Equal("31. O-O", board.SanMoveNumberString "0-0")

[<Fact>]
let ``deviation analysis reads O-O and 0-0 as one move`` () =
  let game (castle: string) =
    let path = Path.Combine(Path.GetTempPath(), "eb-pgnout-" + Guid.NewGuid().ToString "N" + ".pgn")
    File.WriteAllText(path, $"[Event \"e\"]\n[White \"A\"]\n[Black \"B\"]\n[Result \"*\"]\n\n1. e4 e5 2. Nf3 Nc6 3. Bc4 Bc5 4. {castle} *\n")
    FullPGNParser.parsePgnFile path |> Seq.head
  Assert.Equal<string list>(DeviationAnalysis.movesFromPgn (game "0-0"), DeviationAnalysis.movesFromPgn (game "O-O"))

[<Fact>]
let ``the standard's tags: Elo, time control, termination, opening, variation and ECO apart`` () =
  let meta =
    { GameMetadata.Empty with
        White = "W"; Black = "B"; Result = "1-0"; Reason = MiscTypes.ResultReason.ForfeitLimits
        OtherTags =
          [ { Key = "WhiteElo"; Value = "3500" }; { Key = "TimeControl"; Value = "60+0.6" }
            { Key = "Opening"; Value = "Sicilian" }; { Key = "Variation"; Value = "Najdorf" }; { Key = "ECO"; Value = "B90" } ] }
  let _, text = header meta
  let text = text.Replace("\r\n", "\n")
  Assert.Contains("[Result \"1-0\"]\n[WhiteElo \"3500\"]\n[TimeControl \"60+0.6\"]\n[Termination \"time forfeit\"]\n[Opening \"Sicilian\"]\n[Variation \"Najdorf\"]\n[ECO \"B90\"]\n", text)
  // a FEN is no opening's name
  let _, epd = header { GameMetadata.Empty with OpeningName = "8/8/8/4k3/8/8/4P3/4K3 w - - 0 1" }
  Assert.DoesNotContain("[Opening \"", epd)

[<Fact>]
let ``a time setting's PGN TimeControl text`` () =
  let c : TimeControlTypes.TimeConfig = { Id = 1; Fixed = TimeSpan.FromSeconds 60.0; Increment = TimeSpan.FromSeconds 0.6; NodeLimit = false; Nodes = 0; MoveTime = TimeSpan.Zero; MovesToGo = 0 }
  Assert.Equal("60+0.6", c.PgnText 0)
  Assert.Equal("300", { c with Fixed = TimeSpan.FromSeconds 300.0; Increment = TimeSpan.Zero }.PgnText 0)
  Assert.Equal("40/300+2", { c with Fixed = TimeSpan.FromSeconds 300.0; Increment = TimeSpan.FromSeconds 2.0; MovesToGo = 40 }.PgnText 0)
  Assert.Equal("40/7200", { c with Fixed = TimeSpan.FromSeconds 7200.0; Increment = TimeSpan.Zero }.PgnText 40)
  Assert.Equal("-", { c with NodeLimit = true; Nodes = 1000 }.PgnText 0)
  Assert.Equal("-", { c with MoveTime = TimeSpan.FromSeconds 1.0 }.PgnText 0)

[<Fact>]
let ``the move list of a game from a FEN counts from the FEN's move number and side`` () =
  // as Game Review sets a game up: the board and its move graph start at the game's FEN
  let board = Chess.Board()
  board.ResetBoardStateFromFen "r1bqkb1r/1ppp1ppp/p1n2n2/4p3/B3P3/5N2/PPPP1PPP/RNBQ1RK1 b kq - 0 30"
  for san in [ "Nxe4"; "d4"; "b5" ] do board.PlaySanMove san
  Assert.Equal("30... Nxe4 31. d4 b5", board.GetMoveHistoryWithVariations())
  let tokens = board.InlineTokensFromGraph() |> List.filter (fun t -> not t.IsBracket)
  Assert.Equal<(int * bool * int) list>([ 30, false, 0; 31, true, 1; 31, false, 2 ], tokens |> List.map (fun t -> t.MoveNumber, t.IsWhite, t.Ply))
  Assert.Equal("30... Nxe4", tokens.Head.DisplayText)
  // from the start position nothing changes
  board.ResetBoardStateFromFen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
  for san in [ "e4"; "e5" ] do board.PlaySanMove san
  Assert.Equal("1. e4 e5", board.GetMoveHistoryWithVariations())

[<Fact>]
let ``a castle already on the board is followed, not branched, however it was spelled`` () =
  // the book plays O-O (its spelling), the live game then sends the same move as e1g1
  let start = "r1bqk2r/pppp1ppp/2n2n2/2b1p3/2B1P3/5N2/PPPP1PPP/RNBQK2R w KQkq - 4 4"
  let board = Chess.Board()
  board.ResetBoardStateFromFen start
  board.PlaySanMove "O-O"
  board.LoadFen start
  board.PlayUciMove "e1g1"
  let tokens = board.InlineTokensFromGraph()
  Assert.DoesNotContain(tokens, fun t -> t.IsBracket)
  Assert.Equal(1, tokens.Length)
