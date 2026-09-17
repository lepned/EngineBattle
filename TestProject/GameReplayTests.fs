module GameReplayTests

open Xunit
open ChessLibrary
open ChessLibrary.PGNTypes
open ChessLibrary.TypesDef.CoreTypes
open ChessLibrary.GameReplay

// ---------------------------------------------------------------------------
// prepareGameReplay seeds each engine's replay dictionary from saved games of
// the same opening, so that with PreventMoveDeviation on it repeats its own
// earlier moves. Which colour a saved game seeds must follow that colour's
// own match: a saved game with the SAME pairing in the same colours used to
// seed White and never Black, so Black searched fresh and deviated while
// White was held.
// ---------------------------------------------------------------------------

let private engine name = { EngineConfig.Empty with Name = name }

let private savedGame (white: string) (black: string) (moves: string) =
  let pgn = sprintf "[Event \"t\"]\n[White \"%s\"]\n[Black \"%s\"]\n[Result \"*\"]\n\n%s *\n" white black moves
  let g = FullPGNParser.parseFullPgnGame pgn
  g.GameMetaData.OpeningHash <- "same-opening"
  g

let private pairingOf (white: string) (black: string) : Pairing =
  { Opening = PgnGame.Empty 1
    White = engine white
    Black = engine black
    GameNr = 1
    RoundNr = "1.1"
    OpeningHash = "same-opening" }

let private seed (pairing: Pairing) (saved: PgnGame) =
  let dicts = Map.ofList [ pairing.White.Name, ReferenceGameReplay(); pairing.Black.Name, ReferenceGameReplay() ]
  prepareGameReplay pairing dicts (ResizeArray<GameReplay>()) [| saved |] [||]
  dicts.[pairing.White.Name], dicts.[pairing.Black.Name]

let private movesOf (d: ReferenceGameReplay) =
  d.Values |> Seq.map (fun r -> r.Move) |> Seq.sort |> Seq.toList

[<Fact>]
let ``the same pairing in the same colours seeds both engines`` () =
  let white, black = seed (pairingOf "A" "B") (savedGame "A" "B" "1. e4 e5 2. Nf3 Nc6")
  Assert.Equal<string list>([ "e2e4"; "g1f3" ], movesOf white)
  Assert.Equal<string list>([ "b8c6"; "e7e5" ], movesOf black)
  Assert.All(white.Values, fun r -> Assert.Equal("A", r.Engine))
  Assert.All(black.Values, fun r -> Assert.Equal("B", r.Engine))

[<Fact>]
let ``the same White against another opponent seeds White only`` () =
  let white, black = seed (pairingOf "A" "B") (savedGame "A" "C" "1. e4 e5 2. Nf3 Nc6")
  Assert.Equal(2, white.Count)
  Assert.Empty(black)

[<Fact>]
let ``the same Black against another opponent seeds Black only`` () =
  let white, black = seed (pairingOf "A" "B") (savedGame "C" "B" "1. e4 e5 2. Nf3 Nc6")
  Assert.Empty(white)
  Assert.Equal(2, black.Count)

[<Fact>]
let ``the same pairing with colours swapped seeds nothing`` () =
  // A played White before and plays Black now: its earlier moves are not its moves here.
  let white, black = seed (pairingOf "B" "A") (savedGame "A" "B" "1. e4 e5 2. Nf3 Nc6")
  Assert.Empty(white)
  Assert.Empty(black)
