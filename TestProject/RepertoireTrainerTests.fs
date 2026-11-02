/// Repertoire trainer: the tree from a book repertoire PGN, the opponent's pick, the move check and the boxes.
module RepertoireTrainerTests

open Xunit
open ChessLibrary.RepertoireTrainer

// Black's repertoire: 1.e4 c5 is played for the trainee; White's 2.b4 gives chances away (?)
let private pgn =
    """[Event "Repertoire for Black"]

1. e4 c5 2. Nf3 {[%eval 0.30] Maia-1900 60%.} (2. Nc3 {[%eval 0.20] Maia-1900 30%.} 2... Nc6 {[%eval 0.20]} 3. Nf3 g6) (2. b4 {[%eval -0.80] 2.Nf3 is better (+0.30). Maia-1900 10%.} 2... cxb4 {[%eval -0.80]} 3. d4 e5) 2... d6 {[%eval 0.30]} (2... Nc6 {[%eval 0.35] Also good.} 3. d4) 3. d4 {[%eval 0.30] Maia-1900 90%.} 3... cxd4 {[%eval 0.30]} 4. Nxd4 Nf6 *
"""

let private tree () = match load pgn true with Ok t -> t | Error e -> failwith e
let private sans (s: State) = s.Played |> List.rev |> List.map (fun n -> n.San) |> String.concat " "

[<Fact>]
let ``the PGN becomes a tree: side, shares, the opponent's mistake and our accepted alternative`` () =
    let t = tree ()
    Assert.False t.White
    let second = t.Root.Head.Next.Head.Next
    Assert.Equal<string list>([ "Nf3"; "Nc3"; "b4" ], second |> List.map (fun n -> n.San))
    Assert.Equal<float option list>([ Some 60.0; Some 30.0; Some 10.0 ], second |> List.map (fun n -> n.Share))
    Assert.Equal<string list>([ ""; ""; "?" ], second |> List.map (fun n -> n.Mistake))
    Assert.True second.[2].HasTrap
    Assert.Equal<bool list>([ false; true ], second.Head.Next |> List.map (fun n -> n.AlsoGood))

[<Fact>]
let ``the opening is played for the trainee and the opponent's move is picked by share`` () =
    let t = tree ()
    // shares 60/30/10: the roll lands in Nf3, Nc3 or b4
    Assert.Equal("e4 c5 Nf3", sans (start t emptyProgress AllLines 0.1))
    Assert.Equal("e4 c5 Nc3", sans (start t emptyProgress AllLines 0.7))
    Assert.Equal("e4 c5 b4", sans (start t emptyProgress AllLines 0.95))
    // traps only: whatever the roll, the line with the mistake
    Assert.Equal("e4 c5 b4", sans (start t emptyProgress Traps 0.1))

[<Fact>]
let ``the repertoire's move is right, an accepted alternative too (its own move played), another move is wrong`` () =
    let t = tree ()
    let s = start t emptyProgress AllLines 0.1
    let right = step s (Play("d7d6", 0.0))
    Assert.True(right.Feedback.IsCorrect)
    Assert.Equal("e4 c5 Nf3 d6 d4", sans right)
    let also = step s (Play("b8c6", 0.0))
    match also.Feedback with
    | AlsoGood(played, main) -> Assert.Equal(("Nc6", "d6"), (played.San, main.San))
    | f -> failwith $"{f}"
    Assert.Equal("e4 c5 Nf3 d6 d4", sans also)
    let wrong = step s (Play("g7g6", 0.0))
    Assert.True(wrong.Feedback.IsWrong)
    Assert.Equal("e4 c5 Nf3", sans wrong)
    Assert.Equal(1, wrong.Wrongs)

[<Fact>]
let ``a miss puts the position in box 1; found later in the same line it is not promoted`` () =
    let t = tree ()
    let s = start t emptyProgress AllLines 0.1
    let key = s.Fen.Split(' ') |> Array.truncate 4 |> String.concat " "
    let found = step s (Play("d7d6", 0.0))
    Assert.Equal({ Box = 2; DueAt = 1 + interval 2; Misses = 0 }, found.Progress.Cards.[key])
    let missed = step (step s (Play("g7g6", 0.0))) (Play("d7d6", 0.0))
    Assert.Equal({ Box = 1; DueAt = 1; Misses = 1 }, missed.Progress.Cards.[key])
    Assert.Equal((0, 1), (missed.Right, missed.Wrongs))

[<Fact>]
let ``the line ends where the engine's continuation starts, which is played out; Due finds nothing at first`` () =
    let t = tree ()
    let s = start t emptyProgress AllLines 0.1
    let s = step s (Play("d7d6", 0.0))
    let s = step s (Play("c5d4", 0.0))
    Assert.True s.Over
    Assert.Equal("e4 c5 Nf3 d6 d4 cxd4 Nxd4 Nf6", sans s)
    Assert.Equal(1, s.Progress.Lines)
    Assert.Equal(NothingToTrain, (start t emptyProgress Due 0.1).Feedback)

[<Fact>]
let ``the progress file reads back what was saved`` () =
    let path = System.IO.Path.GetTempFileName()
    try
        let p = { Lines = 7; Cards = Map.ofList [ "k1", { Box = 3; DueAt = 12; Misses = 2 } ] }
        saveProgress path p
        Assert.Equal(p, loadProgress path)
        Assert.Equal(emptyProgress, loadProgress (path + ".missing"))
    finally
        System.IO.File.Delete path

[<Fact>]
let ``a game without comments is no repertoire, and castling matches the board's UCI`` () =
    Assert.True((load "[Event \"x\"]\n\n1. e4 e5 2. Nf3 *\n" true).IsError)
    let castling = "[Event \"Repertoire for White\"]\n\n1. e4 e5 2. Nf3 Nc6 3. Bc4 Bc5 4. O-O {[%eval 0.20]} *\n"
    let t = match load castling true with Ok t -> t | Error e -> failwith e
    let rec last (n: Node) = match n.Next with [] -> n | c :: _ -> last c
    let castle = last t.Root.Head
    let board = ChessLibrary.Chess.Board()
    board.LoadFen castle.Fen
    Assert.Contains(castle.Uci, board.GetLegalMoves() |> Seq.map fst)

[<Fact>]
let ``the comment shown is the answered move's, never the next one's`` () =
    let t = tree ()
    let s = start t emptyProgress AllLines 0.1
    Assert.True s.Answered.IsNone
    let right = step s (Play("d7d6", 0.0))
    // 2.Nf3 was answered; 3.d4 is on the board and not yet answered
    Assert.Equal(Some "Nf3", right.Answered |> Option.map (fun n -> n.San))
    Assert.Equal(Some "d4", right.Opponent |> Option.map (fun n -> n.San))
