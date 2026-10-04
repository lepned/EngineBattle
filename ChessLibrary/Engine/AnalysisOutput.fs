namespace ChessLibrary

open System
open ChessLibrary.EngineProtocol
open ChessLibrary.BoardUtils
open ChessLibrary.RuntimeUtilities
open EngineTypes
open MiscTypes
open MoveTypes
open PositionTypes

/// What the analysis wrapper makes of an engine's output, as a function: the state after one line
/// and what that line produced - EngineUpdates for the caller and the odd console message - in
/// the order they are to happen. The wrapper runs `step` on its reader thread and carries out the
/// effects; a test can run it over recorded engine output with no process at all.
///
/// The only thing it needs from outside is the position being searched (IPosition): the side to
/// move for the score, SAN for a PV, and the facts about a bestmove. The wrapper answers those from
/// its board under its lock, and caches the SAN conversions.
module internal AnalysisOutput =

  /// What a bestmove's move is in the searched position.
  type BestMoveFacts =
    { ShortSan: string
      MoveNumber: int
      WhiteToMove: bool
      /// QUIRK (pinned): the position BEFORE the move, reported as FEN and FenAfterMove.
      Fen: string
      PiecesLeft: int
      IsCastling: bool }

  type IPosition =
    abstract WhiteToMove : bool
    /// SAN for a UCI PV, for MultiPV index `mpv`.
    abstract SanPv : mpv: int * lan: string -> string
    /// None when the move is illegal in the position.
    abstract BestMoveFacts : move: string -> BestMoveFacts option
    /// Fills in SANMove for each move of a stats set.
    abstract AddShortSan : moves: ResizeArray<NNValues> -> unit
    /// The board, for the illegal-move message.
    abstract Describe : unit -> string
    /// False in checkmate and stalemate.
    abstract HasLegalMove : unit -> bool

  /// The IPosition of a board, every read under `sync` (the lock the board's writer holds too).
  /// `whiteToMove` is the side to move as cached outside the lock; `sanPv` converts a UCI PV for a
  /// MultiPV index, so the caller can cache the conversions.
  let boardPosition (board: Chess.Board) (sync: obj) (whiteToMove: unit -> bool) (sanPv: int -> string -> string) =
    let mutable board = board
    { new IPosition with
        member _.WhiteToMove = whiteToMove ()
        member _.SanPv(mpv, lan) = sanPv mpv lan
        member _.BestMoveFacts move =
          lock sync (fun () ->
            match tryGetMoveAndSanFromUci &board move with
            | Some (tmove, shortSan) ->
                let mutable pos = board.Position
                Some { ShortSan = shortSan
                       MoveNumber = board.MoveNumber()
                       WhiteToMove = board.Position.STM = 0uy
                       Fen = BoardHelper.posToFen board.Position
                       PiecesLeft = PositionOps.numberOfPieces &pos
                       IsCastling = tmove.MoveType &&& TPieceType.CASTLE <> TPieceType.EMPTY }
            | None -> None)
        member _.AddShortSan moves = lock sync (fun () -> makeShortSan moves &board)
        member _.Describe () =
          lock sync (fun () -> $"Board state: {board.FEN()} {board.CurrentFEN} {board.Position.Ply} ")
        member _.HasLegalMove () = lock sync (fun () -> board.AnyLegalMove()) }

  /// Which kind of output the lines are part of.
  type Mode =
    | Idle
    /// "option ..." lines collected until uciok (newest first).
    | Options of string list
    /// Verbose move stats collected until the "node" line that closes the set (newest first).
    | MoveStats of NNValues list
    | Search
    | BestMovePending

  type State =
    { Mode: Mode
      Nodes: int64
      /// Evals of the search in progress, newest first; emptied by its bestmove.
      Evals: EvalType list
      /// The final eval of every search so far, newest first.
      AllEvals: EvalType list
      Depth: int
      /// SAN and UCI form of the latest complete MultiPV-1 variation.
      Pv: string
      PvLong: string }

    static member Initial =
      { Mode = Idle; Nodes = 0L; Evals = []; AllEvals = []; Depth = 0; Pv = ""; PvLong = "" }

  /// A new position: the previous search's variation must not be published for it. Without this,
  /// a search that never prints a pv - value head runs, instant tablebase or mate returns - would
  /// report the previous position's line, and the GUI's "fill an empty PV from bestmove" fallback
  /// would never fire.
  let withoutPv (state: State) = { state with Pv = ""; PvLong = "" }

  type Effect =
    | Update of EngineUpdate
    | Print of string
    | Debug of string

  let private startsWith (prefix: string) (line: string) = line.StartsWith(prefix, StringComparison.Ordinal)

  let private bestMove (name: string) (pos: IPosition) (state: State) (line: string) =
    // Every bestmove line produces Done, so a waiting caller completes. No move ("(none)", the
    // UCI null move "0000", a bare "bestmove") is right only where there is none; elsewhere it
    // is an illegal move.
    let tokens = line.Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries)
    let doneFirst = Update (Done name)
    let noMove = tokens.Length < 2 || tokens.[1] = "(none)" || tokens.[1] = "0000"
    if noMove && not (pos.HasLegalMove ()) then
      state, [ doneFirst; Debug (sprintf "%s: no move in a finished position: '%s'" name line) ]
    else
      let move = if tokens.Length < 2 then "" else tokens.[1]
      let ponder =
        match tokens |> Array.tryFindIndex ((=) "ponder") with
        | Some i when i + 1 < tokens.Length -> tokens.[i + 1]
        | _ -> ""
      match pos.BestMoveFacts move with
      | Some facts ->
          let eval =
            match state.Evals, state.AllEvals with
            | e :: _, _ -> e
            | [], e :: _ -> e
            | [], [] -> EvalType.CP 0.0
          let pv =
            if String.IsNullOrEmpty state.Pv then
              let prefix = if facts.WhiteToMove then sprintf "%d." facts.MoveNumber else sprintf "%d..." facts.MoveNumber
              prefix + facts.ShortSan
            else state.Pv
          let moveDetail =
            { LongSan = move
              FromSq = move.[0..1]
              ToSq = move.[2..3]
              Color = "w"
              IsCastling = facts.IsCastling
              Comments = String.Empty }
          let info =
            { Player = name
              Move = move
              Ponder = ponder
              Eval = eval
              TimeLeft = TimeSpan.Zero
              MoveTime = TimeSpan.Zero
              NPS = 0.0 // not tracked on the bestmove path; Status updates carry the live NPS
              Nodes = state.Nodes
              FEN = facts.Fen
              PV = pv
              LongPV = pv
              MoveAndFen = { Move = moveDetail; ShortSan = facts.ShortSan; FenAfterMove = facts.Fen }
              MoveHistory = ""
              Move50 = 0
              R3 = 1
              PiecesLeft = facts.PiecesLeft
              AdjDrawML = 10 }
          { state with AllEvals = eval :: state.AllEvals; Evals = []; Depth = 0 }, [ doneFirst; Update (BestMove info) ]
      | None ->
          state,
          [ doneFirst
            Print $"{name} played an illegal move here: {line} "
            Print (pos.Describe ()) ]

  let private searchInfo (name: string) (pos: IPosition) (state: State) (line: string) =
    match Regex.getEssentialDataWithEPS line pos.WhiteToMove with
    | Some (d, eval, nodes, nps, eps, pvLine, tbHits, wdl, sd, mPv) ->
        let mPv = if mPv = 0 then 1 else mPv
        // A fail-high/low line carries a PV cut to the root move; let it through and the last
        // complete variation is lost - permanently, if the search stops right there. Score, depth
        // and node counts are real and flow on unchanged.
        let isBound = Regex.isBoundLine line
        let pv, pvLong =
          if not (String.IsNullOrEmpty pvLine) && mPv = 1 && not isBound then pos.SanPv(1, pvLine), pvLine
          else state.Pv, state.PvLong
        let state =
          { state with
              Nodes = nodes
              Depth = max state.Depth d
              Evals = eval :: state.Evals
              Pv = pv
              PvLong = pvLong }
        let status =
          { PlayerName = name
            Eval = eval
            Depth = d
            SD = sd
            Nodes = nodes
            NPS = float nps
            EPS = float eps
            TBhits = tbHits
            WDL = if wdl.IsSome then WDLType.HasValue wdl.Value else WDLType.NotFound
            PV = if mPv = 1 then pv else pos.SanPv(mPv, pvLine)
            PVLongSAN = if mPv = 1 && isBound then pvLong else pvLine
            MultiPV = mPv }
        // The raw line goes along with the parsed status: engine-specific extras survive to GUI
        // consumers (the CandidateMoves copy output, for one).
        state, [ Update (Status status); Update (Info (name, line)) ]
    | None -> state, []

  /// The mode a line switches to, before anything is produced for it.
  let private nextMode (name: string) (mode: Mode) (line: string) =
    if startsWith "readyok" line then Search
    elif startsWith "option" line then
      match mode with
      | Options lines -> Options (line :: lines)
      | _ -> Options [ line ]
    elif startsWith "bestmove" line then BestMovePending
    elif startsWith "info string" line && line.Contains "N:" then
      match mode with
      | MoveStats moves ->
          let nn = Regex.getInfoStringData name line
          // One node in a policy test ("go nodes 1"): the top move carries the node's Q.
          let moves =
            if startsWith "info string node" line && nn.Nodes = 1 then
              // In arrival order, so a tie goes to the earlier move, as it always has.
              let top = List.rev moves |> List.maxBy (fun e -> e.P)
              moves |> List.map (fun m -> if obj.ReferenceEquals(m, top) then { m with Q = nn.Q } else m)
            else moves
          MoveStats (nn :: moves)
      | _ ->
          if startsWith "info string node" line then MoveStats []
          else MoveStats [ Regex.getInfoStringData name line ]
    elif startsWith "info" line then Search
    else mode

  /// A line of the UCI protocol; anything else is the engine talking to its user - and so is an
  /// `info string` without move stats (Ceres's dump-fen, Stockfish's NNUE note).
  let private isProtocol (line: string) =
    let line = line.TrimStart()
    if startsWith "info string" line then line.Contains "N:"
    else
      line = ""
      || [ "info"; "bestmove"; "option"; "id "; "uciok"; "readyok"; "copyprotection"; "registration" ]
         |> List.exists (fun keyword -> startsWith keyword line)

  /// One line of engine output after the handshake: the new state and what the line produced,
  /// in order.
  let step (name: string) (pos: IPosition) (state: State) (line: string) : State * Effect list =
    // the answer to a dump command (Ceres's dump-move-stats, dump-info...) or an error: shown, as
    // the engine meant it for its user; it changes nothing in the parse
    if not (isProtocol line) then state, [ Print (sprintf "%s: %s" name line) ] else
    let state = { state with Mode = nextMode name state.Mode line }
    match state.Mode with
    | MoveStats moves when startsWith "info string node" line ->
        // QUIRK (pinned): the closing "node" line is in the set too, as a pseudo-move.
        let set = ResizeArray<NNValues>(List.rev moves)
        pos.AddShortSan set
        { state with Mode = Idle }, [ Update (NNSeq set) ]
    | Search -> searchInfo name pos state line
    | BestMovePending ->
        let state, effects = bestMove name pos state line
        { state with Mode = Idle }, effects
    | Options lines when startsWith "uciok" line ->
        { state with Mode = Idle }, [ Update (UCIInfo (ResizeArray<string>(List.rev lines))) ]
    | _ -> state, []
