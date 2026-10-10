/// The piece-value commands: contextual values in one position (values position), a fit over many
/// positions (values fit), and results by material imbalance (pgn imbalance).
module Commands.Values

open System
open System.IO
open ChessLibrary
open ChessLibrary.TypesDef
open RuntimeUtilities
open MathNet.Numerics.LinearAlgebra
open Cli.Groups

// ---------- Contextual piece valuation (leave-one-out net eval) ----------
// "What is a piece worth to the net in THIS position" = how much the eval drops
// when the piece is removed, framed from the piece owner's perspective.
// All board manipulation is done on the FEN string (representation-agnostic);
// square index convention: 0 = a1, 7 = h1, 56 = a8, 63 = h8.

type PieceContribution =
    { Sq: int
      Piece: char
      IsWhite: bool
      Illegal: bool
      DeltaLogit: float option    // owner perspective, logit(score) — the de-saturated metric
      DeltaCp: float option       // owner perspective, centipawns (engine; saturates near rails)
      DeltaScore: float option }  // owner perspective, White-expected-score % points (saturates)

let pvSquareName (sq: int) =
    sprintf "%c%d" (char (int 'a' + sq % 8)) (sq / 8 + 1)

let pvFields (fen: string) =
    fen.Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries)

/// Parse the FEN board field into 64 chars ('.' = empty), indexed a1=0 .. h8=63.
let pvParseFenBoard (fen: string) : char[] =
    let boardField = (pvFields fen).[0]
    let squares = Array.create 64 '.'
    let mutable rank = 7
    let mutable file = 0
    for c in boardField do
        if c = '/' then rank <- rank - 1; file <- 0
        elif Char.IsDigit c then file <- file + (int c - int '0')
        else
            if rank >= 0 && rank < 8 && file < 8 then squares.[rank * 8 + file] <- c
            file <- file + 1
    squares

/// Serialize a 64-char board array back into a FEN board field.
let pvBoardToField (squares: char[]) : string =
    let sb = System.Text.StringBuilder()
    for rank in 7 .. -1 .. 0 do
        let mutable empty = 0
        for file in 0 .. 7 do
            let c = squares.[rank * 8 + file]
            if c = '.' then empty <- empty + 1
            else
                if empty > 0 then sb.Append(string empty) |> ignore; empty <- 0
                sb.Append(c) |> ignore
        if empty > 0 then sb.Append(string empty) |> ignore
        if rank > 0 then sb.Append('/') |> ignore
    sb.ToString()

/// Build a FEN with the piece on `sq` removed. Clears en passant and strips
/// standard castling rights tied to a removed home-square rook. Keeps side to move.
let pvFenWithoutPiece (fen: string) (sq: int) : string =
    let fields = pvFields fen
    let squares = pvParseFenBoard fen
    let removed = squares.[sq]
    squares.[sq] <- '.'
    let boardField = pvBoardToField squares
    let stm = if fields.Length > 1 then fields.[1] else "w"
    let castling =
        let c = if fields.Length > 2 then fields.[2] else "-"
        if c = "-" then "-"
        else
            let dropped =
                match sq, removed with
                | 0, 'R' -> "Q" | 7, 'R' -> "K"
                | 56, 'r' -> "q" | 63, 'r' -> "k"
                | _ -> ""
            let filtered = String(c.ToCharArray() |> Array.filter (fun ch -> dropped.IndexOf(ch) < 0))
            if filtered = "" then "-" else filtered
    let half = if fields.Length > 4 then fields.[4] else "0"
    let full = if fields.Length > 5 then fields.[5] else "1"
    sprintf "%s %s %s - %s %s" boardField stm castling half full

/// True if the side NOT to move is in check (i.e. the position is illegal /
/// off-distribution for net eval — usually because a removed piece exposed a check).
let pvOpponentInCheck (fen: string) : bool =
    let fields = pvFields fen
    let stm = if fields.Length > 1 then fields.[1] else "w"
    let swapped = if stm = "w" then "b" else "w"
    let castling = if fields.Length > 2 then fields.[2] else "-"
    let half = if fields.Length > 4 then fields.[4] else "0"
    let full = if fields.Length > 5 then fields.[5] else "1"
    let swappedFen = sprintf "%s %s %s - %s %s" fields.[0] swapped castling half full
    let mutable pos = RuntimeUtilities.BoardHelper.getPosFromFen (Some swappedFen)
    MoveGeneration.InCheck &pos <> 0UL

/// analyzePosition already returns the eval in White's perspective; convert to cp.
let pvEvalToWhiteCp (e: ChessLibrary.MiscTypes.EvalType) : float option =
    match e with
    | ChessLibrary.MiscTypes.EvalType.CP p -> Some (p * 100.0)
    | ChessLibrary.MiscTypes.EvalType.Mate m -> Some (float (sign m) * 30000.0)
    | _ -> None

/// WDL is reported from the side-to-move's perspective; convert to White expected score %.
let pvWdlToWhiteScore (stmWhite: bool) (wdl: ChessLibrary.EngineTypes.WDL option) : float option =
    match wdl with
    | Some w ->
        let total = w.Win + w.Draw + w.Loss
        if total <= 0.0 then None
        else
            let win, draw = if stmWhite then w.Win, w.Draw else w.Loss, w.Draw
            Some ((win + 0.5 * draw) / total * 100.0)
    | None -> None

/// White expected-score in logit (log-odds) space. Win% saturates at the rails;
/// the logit stays finite and monotone (the value head emits soft WDL), which
/// restores resolution between, e.g., removing a queen vs a rook.
let pvWdlToWhiteLogit (stmWhite: bool) (wdl: ChessLibrary.EngineTypes.WDL option) : float option =
    match pvWdlToWhiteScore stmWhite wdl with
    | Some pct ->
        let eps = 1e-4
        let s = max eps (min (1.0 - eps) (pct / 100.0))
        Some (log (s / (1.0 - s)))
    | None -> None

/// What each piece is worth to the engine in this position: the eval drop when it is removed.
let runPieceValues (config: TypesDef.CoreTypes.EngineConfig) (fen: string) (lim: Limit) =
  do
      let searchDesc = lim.Text

      let fields = pvFields fen
      let stmWhite = fields.Length < 2 || fields.[1] = "w"

      printfn ""
      printfn "Engine:   %s" config.Name
      printfn "FEN:      %s" fen
      printfn "Side:     %s to move" (if stmWhite then "White" else "Black")
      printfn "Search:   %s" searchDesc
      printfn ""
      printfn "Contextual piece value = how much the eval drops when each piece is removed,"
      printfn "framed from the piece owner's perspective. Higher = more valuable in THIS position."
      printfn ""

      let engine = Engines.start config
      try
          // Baseline eval
          let baseR = Engines.search engine fen [] lim false
          let baseCp = pvEvalToWhiteCp baseR.Eval
          let baseScore = pvWdlToWhiteScore stmWhite baseR.Wdl
          let baseLogit = pvWdlToWhiteLogit stmWhite baseR.Wdl
          printfn "Baseline eval (White's view): %s%s"
              (match baseCp with Some c -> sprintf "%+.0f cp" c | None -> "n/a")
              (match baseScore with Some s -> sprintf "  |  White score %.1f%%" s | None -> "")

          // Decided-position guard: per-piece values are ill-posed once the game
          // is effectively over (any change saturates), so warn the user.
          match baseLogit with
          | Some l when abs l > 2.0 ->
              printfn "⚠️  Position is already decided (logit %+.1f ≈ score %.0f%%) — individual"
                  l (match baseScore with Some s -> s | None -> 0.0)
              printfn "    piece values are ill-posed here; use a position with |eval| < ~2 pawns."
          | _ -> ()

          let squares = pvParseFenBoard fen
          let pieces =
              [ for sq in 0 .. 63 do
                  let c = squares.[sq]
                  if c <> '.' && c <> 'K' && c <> 'k' then yield sq, c ]

          printf "Evaluating %d pieces " pieces.Length
          let results =
              [ for (sq, c) in pieces do
                  let isWhite = Char.IsUpper c
                  let modFen = pvFenWithoutPiece fen sq
                  if pvOpponentInCheck modFen then
                      printf "."
                      yield { Sq = sq; Piece = c; IsWhite = isWhite; Illegal = true
                              DeltaLogit = None; DeltaCp = None; DeltaScore = None }
                  else
                      let r = Engines.search engine modFen [] lim false
                      let remCp = pvEvalToWhiteCp r.Eval
                      let remScore = pvWdlToWhiteScore stmWhite r.Wdl
                      let remLogit = pvWdlToWhiteLogit stmWhite r.Wdl
                      let sgn = if isWhite then 1.0 else -1.0
                      let delta baseV remV =
                          match baseV, remV with
                          | Some b, Some rm -> Some (sgn * (b - rm))
                          | _ -> None
                      printf "."
                      yield { Sq = sq; Piece = c; IsWhite = isWhite; Illegal = false
                              DeltaLogit = delta baseLogit remLogit
                              DeltaCp = delta baseCp remCp
                              DeltaScore = delta baseScore remScore } ]
          printfn " done.\n"

          let pieceWord (c: char) =
              let name =
                  match Char.ToUpper c with
                  | 'P' -> "pawn" | 'N' -> "knight" | 'B' -> "bishop"
                  | 'R' -> "rook" | 'Q' -> "queen" | _ -> "?"
              sprintf "%s %s" (if Char.IsUpper c then "White" else "Black") name

          // ---- Ranked table (Δlogit is the de-saturated headline metric) ----
          printfn "%-6s  %-14s  %9s  %10s  %10s  %s" "Square" "Piece" "Δlogit" "Δcp" "Δscore%" "Note"
          printfn "%s" (String.replicate 80 "-")
          results
          |> List.sortByDescending (fun r -> match r.DeltaLogit with Some v -> v | None -> System.Double.NegativeInfinity)
          |> List.iter (fun r ->
              let lgStr = match r.DeltaLogit with Some v -> sprintf "%+.2f" v | None -> "—"
              let cpStr = match r.DeltaCp with Some v -> sprintf "%+.0f" v | None -> "—"
              let scStr = match r.DeltaScore with Some v -> sprintf "%+.1f" v | None -> "—"
              let note = if r.Illegal then "off-distribution (removal exposes check)" else ""
              printfn "%-6s  %-14s  %9s  %10s  %10s  %s"
                  (pvSquareName r.Sq) (pieceWord r.Piece) lgStr cpStr scStr note)
          printfn ""

          // ---- Heatmap board (value in pawns, owner's perspective) ----
          let bySq = results |> List.map (fun r -> r.Sq, r) |> Map.ofList
          let cell (sq: int) =
              let c = squares.[sq]
              if c = '.' then "      "
              elif c = 'K' || c = 'k' then sprintf "%c  .  " c
              else
                  match Map.tryFind sq bySq with
                  | Some r when r.Illegal -> sprintf "%c  x  " c
                  | Some { DeltaLogit = Some v } ->
                      let lg = max -9.9 (min 9.9 v)
                      sprintf "%c%+5.1f" c lg
                  | _ -> sprintf "%c  ?  " c
          printfn "Board (piece + contextual value as Δlogit, owner's perspective):"
          printfn ""
          for rank in 7 .. -1 .. 0 do
              let cells = [ for file in 0 .. 7 -> cell (rank * 8 + file) ]
              printfn " %d | %s" (rank + 1) (String.concat " " cells)
          printfn "     %s" (String.concat " " [ for f in 0 .. 7 -> sprintf "%-6c" (char (int 'a' + f)) ])
          printfn ""
          printfn "  (K/k = king, 'x' = removal exposes check, '?' = no eval)"
          printfn ""

          // ---- Per-type summary (average Δlogit by piece type) ----
          printfn "Average contextual value by piece type (Δlogit), static order P<N≈B<R<Q:"
          printfn "%-8s  %10s  %5s" "Type" "Avg Δlogit" "Count"
          printfn "%s" (String.replicate 28 "-")
          for t in [ 'P'; 'N'; 'B'; 'R'; 'Q' ] do
              let vals =
                  results
                  |> List.filter (fun r -> not r.Illegal && Char.ToUpper r.Piece = t)
                  |> List.choose (fun r -> r.DeltaLogit)
              if not vals.IsEmpty then
                  let avg = List.average vals
                  let label = match t with 'P' -> "Pawn" | 'N' -> "Knight" | 'B' -> "Bishop" | 'R' -> "Rook" | 'Q' -> "Queen" | _ -> "?"
                  printfn "%-8s  %10.2f  %5d" label avg vals.Length
          printfn ""
      finally
          Engines.quit engine

// ---------- Multi-position piece-value regression (AlphaZero-style) ----------
// Regress the net's eval (logit of White expected score) against material imbalance
// across many positions. The coefficients are the net's implied piece values.
// Uses L1 (IRLS) instead of L2 — the PNAS "Acquisition of Chess Knowledge" paper
// found L2 systematically underestimates piece weights. Small ridge keeps the
// 7x7 normal-equation solve stable when a material feature has little variance.

/// White-minus-black piece counts: [ΔP; ΔN; ΔB; ΔR; ΔQ].
let pvMaterialDiff (squares: char[]) : float[] =
    let cnt c = squares |> Array.sumBy (fun x -> if x = c then 1.0 else 0.0)
    [| cnt 'P' - cnt 'p'; cnt 'N' - cnt 'n'; cnt 'B' - cnt 'b'; cnt 'R' - cnt 'r'; cnt 'Q' - cnt 'q' |]

let pvTotalPieces (squares: char[]) : int =
    squares |> Array.sumBy (fun x -> if x = '.' then 0 else 1)

/// Weighted least squares via ridge-stabilised normal equations (feature count is tiny).
let pvWls (xRows: float[][]) (y: float[]) (wts: float[]) (ridge: float) : float[] =
    let p = xRows.[0].Length
    let n = xRows.Length
    let xtx = Array2D.zeroCreate p p
    let xty = Array.zeroCreate p
    for i in 0 .. n - 1 do
        let wi = wts.[i]
        let xi = xRows.[i]
        for a in 0 .. p - 1 do
            xty.[a] <- xty.[a] + wi * xi.[a] * y.[i]
            for b in 0 .. p - 1 do
                xtx.[a, b] <- xtx.[a, b] + wi * xi.[a] * xi.[b]
    for a in 0 .. p - 1 do xtx.[a, a] <- xtx.[a, a] + ridge
    let m = DenseMatrix.init p p (fun a b -> xtx.[a, b])
    let v = DenseVector.init p (fun a -> xty.[a])
    (m.Solve(v)).ToArray()

/// L1 regression by iteratively reweighted least squares (robust to outliers/saturation).
let pvFitL1 (xRows: float[][]) (y: float[]) : float[] =
    let n = xRows.Length
    let ridge = 1e-6
    let predict (beta: float[]) (x: float[]) = Array.fold2 (fun acc b xi -> acc + b * xi) 0.0 beta x
    let mutable beta = pvWls xRows y (Array.create n 1.0) ridge
    for _iter in 1 .. 25 do
        let wts = Array.init n (fun i -> 1.0 / max (abs (y.[i] - predict beta xRows.[i])) 1e-3)
        beta <- pvWls xRows y wts ridge
    beta

/// DeepMind's model: fit g(d) = tanh(β·d) to game outcomes z ∈ {-1,0,+1} by
/// Gauss-Newton least squares (Levenberg-Marquardt damped). Returns β.
let pvFitTanh (xRows: float[][]) (z: float[]) : float[] =
    let p = xRows.[0].Length
    let n = xRows.Length
    let lambda = 1e-3
    let beta = Array.zeroCreate p
    let dot (b: float[]) (x: float[]) = Array.fold2 (fun a bi xi -> a + bi * xi) 0.0 b x
    for _iter in 1 .. 100 do
        let jtj = Array2D.zeroCreate p p
        let rhs = Array.zeroCreate p     // = Σ s_i x_i r_i  (the −Jᵀr of GN)
        for i in 0 .. n - 1 do
            let xi = xRows.[i]
            let t = tanh (dot beta xi)
            let s = 1.0 - t * t          // tanh'(η) = sech²(η)
            let ri = z.[i] - t
            for a in 0 .. p - 1 do
                rhs.[a] <- rhs.[a] + s * xi.[a] * ri
                for b in 0 .. p - 1 do
                    jtj.[a, b] <- jtj.[a, b] + (s * xi.[a]) * (s * xi.[b])
        for a in 0 .. p - 1 do jtj.[a, a] <- jtj.[a, a] + lambda
        let m = DenseMatrix.init p p (fun a b -> jtj.[a, b])
        let v = DenseVector.init p (fun a -> rhs.[a])
        let step = (m.Solve(v)).ToArray()
        for a in 0 .. p - 1 do beta.[a] <- beta.[a] + step.[a]
    beta

/// Game-clustered (block) bootstrap of the four normalised piece values [N;B;R;Q].
/// Resamples whole GAMES with replacement — positions within a game are highly
/// correlated (consecutive plies share nearly the same material), so resampling
/// individual positions would treat them as independent and understate the intervals.
/// `estimate` refits on a resampled row set and returns [|N;B;R;Q|] (pawn=1), or None
/// if that draw is degenerate. Returns the (2.5%, 97.5%) percentile band per piece.
/// Returns per-piece CIs [|N;B;R;Q|] plus the CI of the Bishop−Knight gap. The gap CI is
/// computed per resample (B and N share the pawn-normalisation and the same games, so they
/// co-move — the paired difference is far tighter than the overlap of the marginals implies).
let pvBootstrapCI
      (rows: (float[] * float * int * int)[])
      (estimate: (float[] * float * int * int)[] -> float[] option)
      (b: int) : ((float * float)[] * (float * float * float)) option =
    let games = rows |> Array.groupBy (fun (_, _, _, gid) -> gid) |> Array.map snd
    let ng = games.Length
    if ng < 2 then None
    else
        let rng = System.Random(12345)   // fixed seed → reproducible intervals
        let samples = ResizeArray<float[]>()
        for _ in 1 .. b do
            let draw = Array.init ng (fun _ -> games.[rng.Next ng]) |> Array.concat
            match estimate draw with
            | Some v -> samples.Add v
            | None -> ()
        if samples.Count < max 10 (b / 2) then None
        else
            let pct (q: float) (xs: float[]) =
                let s = Array.sort xs
                s.[max 0 (min (s.Length - 1) (int (q * float (s.Length - 1) + 0.5)))]
            let perPiece =
                [| for i in 0 .. 3 ->
                     let col = samples |> Seq.map (fun v -> v.[i]) |> Array.ofSeq
                     (pct 0.025 col, pct 0.975 col) |]
            let gap = samples |> Seq.map (fun v -> v.[1] - v.[0]) |> Array.ofSeq   // Bishop − Knight
            Some (perPiece, (pct 0.025 gap, pct 0.5 gap, pct 0.975 gap))

/// Outcome-regression rows: every position past the opening, labelled with its
/// game's final result from White's perspective (+1 / 0 / -1). No engine needed.
let pvCollectOutcomeRows (path: string) (sample: int) : (float[] * float * int * int)[] =
    let rows = ResizeArray<float[] * float * int * int>()
    for gi, g in ChessLibrary.FullPGNParser.parsePgnFile path |> Seq.indexed do
        let zWhite =
            match g.GameMetaData.Result with
            | "1-0" -> Some 1.0
            | "0-1" -> Some -1.0
            | "1/2-1/2" -> Some 0.0
            | _ -> None
        match zWhite with
        | None -> ()
        | Some z ->
            try
                let board = ChessLibrary.Chess.Board()
                let startFen =
                    if String.IsNullOrWhiteSpace g.GameMetaData.Fen then ChessLibrary.MiscTypes.startPosition
                    else g.GameMetaData.Fen
                board.LoadFen startFen
                let mutable ply = 0
                for pm in g.Mainline do
                    board.PlaySanMove pm.San
                    ply <- ply + 1
                    if ply >= 16 then
                        let f = board.FEN()
                        let squares = pvParseFenBoard f
                        let md = pvMaterialDiff squares
                        // DeepMind model: features are material imbalance only — NO intercept,
                        // NO tempo. Balanced positions (d=0) then drop out of the fit naturally.
                        rows.Add([| md.[0]; md.[1]; md.[2]; md.[3]; md.[4] |], z, pvTotalPieces squares, gi)
            with _ -> ()
    let arr = rows.ToArray()
    if arr.Length <= sample then arr
    else
        let stride = float arr.Length / float sample
        [| for i in 0 .. sample - 1 -> arr.[int (float i * stride)] |]

/// Read the engine's eval (wv=, in pawns) from a PGN move comment using EB's
/// own annotation parser. Returns None when the comment has no wv (e.g. book moves).
let pvParseWv (comment: string) : float option =
    if String.IsNullOrWhiteSpace comment || not (comment.Contains "wv=") then None
    else
        let stat = ChessLibrary.EngineTypes.Annotation.getEngineStatData "" false comment
        Some stat.wv

/// Rows from PGN embedded evals: for each position past the opening, the engine's
/// in-game eval (wv, flipped to White's perspective, |wv|<4 to stay linear) vs
/// material imbalance. No engine process needed — full-search evals straight from the PGN.
let pvCollectPgnEvalRows (path: string) (sample: int) (player: string option) : (float[] * float * int * int)[] =
    let rows = ResizeArray<float[] * float * int * int>()
    for gi, g in ChessLibrary.FullPGNParser.parsePgnFile path |> Seq.indexed do
        try
            let white = g.GameMetaData.White
            let black = g.GameMetaData.Black
            let board = ChessLibrary.Chess.Board()
            let startFen =
                if String.IsNullOrWhiteSpace g.GameMetaData.Fen then ChessLibrary.MiscTypes.startPosition
                else g.GameMetaData.Fen
            board.LoadFen startFen
            let mutable ply = 0
            for pm in g.Mainline do
                // The wv on move m is the engine's eval of the position BEFORE m,
                // from the mover's perspective → flip to White and pair with that position.
                if ply >= 16 then
                    match pvParseWv pm.Comment with
                    // wv is already stored White-relative and in pawns (EB writes it via
                    // getEssentialDataWithEPS, which normalises to White) — do NOT flip.
                    // Continuous pawn target needs an intercept + tempo term, so include them.
                    // Keep decisive positions too (that's where material→eval is cleanest);
                    // only exclude mate sentinels (wv = ±200 from the parser).
                    | Some wv when abs wv < 20.0 ->
                        // Odd total piece count ⟹ side counts differ by an odd number ⟹
                        // guaranteed material imbalance. Use the board's native count.
                        let mutable pos = board.Position
                        let total = PositionTypes.PositionOps.numberOfPieces &pos
                        // --player filter: keep only plies where the named net was on move,
                        // so every wv is that single net's own eval (single-net assessment).
                        let onMove = if pos.STM = 0uy then white else black
                        let playerOk =
                            match player with
                            | None -> true
                            | Some pn -> onMove.IndexOf(pn, StringComparison.OrdinalIgnoreCase) >= 0
                        if total % 2 = 1 && playerOk then
                            let squares = pvParseFenBoard (board.FEN())
                            let md = pvMaterialDiff squares
                            let stm = if pos.STM = 0uy then 1.0 else -1.0
                            rows.Add([| 1.0; md.[0]; md.[1]; md.[2]; md.[3]; md.[4]; stm |], wv, total, gi)
                    | _ -> ()
                board.PlaySanMove pm.San
                ply <- ply + 1
        with _ -> ()
    let arr = rows.ToArray()
    if arr.Length <= sample then arr
    else
        let stride = float arr.Length / float sample
        [| for i in 0 .. sample - 1 -> arr.[int (float i * stride)] |]

// ---- Combination / contextual material-imbalance measurement ----
// Matches positions by ABSOLUTE per-side material (not just the difference), so e.g.
// BN-vs-BB (bishop pair) is distinguished from bare B-vs-N. A signature gives an exact
// count for each INVOLVED piece type on each side; UNINVOLVED types must be equal across
// sides (which forces pawns level by construction). Type order: [P;N;B;R;Q].
let pvSideCounts (squares: char[]) : int[] * int[] =
    let w = Array.zeroCreate 5
    let b = Array.zeroCreate 5
    let idx c = match c with 'P' -> 0 | 'N' -> 1 | 'B' -> 2 | 'R' -> 3 | 'Q' -> 4 | _ -> -1
    for c in squares do
        match c with
        | 'P' | 'N' | 'B' | 'R' | 'Q' -> let i = idx c in w.[i] <- w.[i] + 1
        | 'p' | 'n' | 'b' | 'r' | 'q' -> let i = idx (System.Char.ToUpper c) in b.[i] <- b.[i] + 1
        | _ -> ()
    w, b

// Rows: (whiteCounts[5], blackCounts[5], z White-relative, wv option)
let pvComboRows (path: string) : (int[] * int[] * float * float option)[] =
    let rows = ResizeArray<int[] * int[] * float * float option>()
    for g in ChessLibrary.FullPGNParser.parsePgnFile path do
        let z = match g.GameMetaData.Result with "1-0" -> Some 1.0 | "0-1" -> Some -1.0 | "1/2-1/2" -> Some 0.0 | _ -> None
        match z with
        | None -> ()
        | Some zz ->
            try
                let board = ChessLibrary.Chess.Board()
                let startFen =
                    if String.IsNullOrWhiteSpace g.GameMetaData.Fen then ChessLibrary.MiscTypes.startPosition
                    else g.GameMetaData.Fen
                board.LoadFen startFen
                let mutable ply = 0
                for pm in g.Mainline do
                    board.PlaySanMove pm.San
                    ply <- ply + 1
                    if ply >= 16 then
                        let w, b = pvSideCounts (pvParseFenBoard (board.FEN()))
                        rows.Add(w, b, zz, pvParseWv pm.Comment)
            with _ -> ()
    rows.ToArray()

// Does (w,b) match the signature with the A-side as White? Some = exact, None/None = equal.
let pvSigMatch (wr: int option[]) (br: int option[]) (w: int[]) (b: int[]) : bool =
    let mutable good = true
    for t in 0 .. 4 do
        match wr.[t], br.[t] with
        | Some wv, Some bv -> if w.[t] <> wv || b.[t] <> bv then good <- false
        | None, None -> if w.[t] <> b.[t] then good <- false
        | _ -> good <- false
    good

// +1 = White is the A-side, -1 = Black is the A-side (mirror), 0 = no match
let pvComboOrient (wr: int option[]) (br: int option[]) (w: int[]) (b: int[]) : int =
    if pvSigMatch wr br w b then 1
    elif pvSigMatch wr br b w then -1
    else 0

/// Results by material imbalance (B vs N, the bishop pair, ...) over a PGN's games; `path` exists.
let runPvCombo (path: string) =
  do
      printfn "\nContextual material imbalances on:\n  %s\n" path
      printf "Collecting positions ..."
      let rows = pvComboRows path
      printfn " %d positions past ply 16.\n" rows.Length

      // build signature from involved (typeIndex, whiteCount, blackCount); P=0 N=1 B=2 R=3 Q=4
      let sigOf (involved: (int * int * int) list) =
          let wr = Array.create 5 None
          let br = Array.create 5 None
          for (t, wv, bv) in involved do wr.[t] <- Some wv; br.[t] <- Some bv
          wr, br
      let combos =
          [ "B vs N",     sigOf [ (2,1,0); (1,0,1) ]                 // bare, context-free baseline
            "R vs N",     sigOf [ (3,1,0); (1,0,1) ]
            "R vs B",     sigOf [ (3,1,0); (2,0,1) ]
            "BN vs BB",   sigOf [ (2,1,2); (1,1,0) ]                 // bishop pair: A=B+N, B=B+B
            "BB vs NN",   sigOf [ (2,2,0); (1,0,2) ]                 // pair vs two knights
            "BR vs BN",   sigOf [ (2,1,1); (3,1,0); (1,0,1) ]       // R vs N with a bishop each
            "RN vs RB",   sigOf [ (3,1,1); (1,1,0); (2,0,1) ]       // N vs B with a rook each
            "QN vs QB",   sigOf [ (4,1,1); (1,1,0); (2,0,1) ] ]     // N vs B with a queen each

      printfn "  %-12s %8s %10s %9s" "matchup" "n" "outcome" "eval(pw)"
      printfn "  %-12s %8s %10s %9s" "(A vs B)" "" "(+=A)" "(+=A)"
      for (label, (wr, br)) in combos do
          let acc = ResizeArray<float * float option>()
          for (w, b, z, wv) in rows do
              match pvComboOrient wr br w b with
              | 1 -> acc.Add(z, wv)
              | -1 -> acc.Add(-z, wv |> Option.map (fun v -> -v))
              | _ -> ()
          if acc.Count >= 20 then
              let mz = acc |> Seq.averageBy fst
              let evs = acc |> Seq.choose snd |> Array.ofSeq
              let me = if evs.Length > 0 then Array.average evs else nan
              printfn "  %-12s %8d %+10.3f %+9.2f" label acc.Count mz me
          else
              printfn "  %-12s %8d %10s %9s" label acc.Count "(few)" ""
      printfn ""

/// Side to move is in check (a tactical position — exclude from a quiet fit).
let pvInCheck (fen: string) : bool =
    let mutable pos = RuntimeUtilities.BoardHelper.getPosFromFen (Some fen)
    MoveGeneration.InCheck &pos <> 0UL

let pvSquareIndexOf (s: string) : int =
    let file = int (Char.ToLower s.[0]) - int 'a'
    let rank = int s.[1] - int '1'
    if file >= 0 && file < 8 && rank >= 0 && rank < 8 then rank * 8 + file else -1

/// Best move lands on an occupied square → a capture is on the board (tactical proxy).
/// Misses en passant, which is rare enough to ignore for a quiet filter.
let pvBestIsCapture (squares: char[]) (bestmove: string) : bool =
    if bestmove.Length >= 4 then
        let toSq = pvSquareIndexOf (bestmove.Substring(2, 2))
        toSq >= 0 && squares.[toSq] <> '.'
    else false

/// Replay each game's mainline SAN, collecting the FEN after every ply past the
/// opening (ply ≥ 16), then stride-sample for spread across games and phases.
let pvCollectFensFromPgn (path: string) (sample: int) : string[] =
    let candidates = ResizeArray<string>()
    for g in ChessLibrary.FullPGNParser.parsePgnFile path do
        try
            let board = ChessLibrary.Chess.Board()
            let startFen =
                if String.IsNullOrWhiteSpace g.GameMetaData.Fen then ChessLibrary.MiscTypes.startPosition
                else g.GameMetaData.Fen
            board.LoadFen startFen
            let mutable ply = 0
            for pm in g.Mainline do
                board.PlaySanMove pm.San
                ply <- ply + 1
                if ply >= 16 then candidates.Add(board.FEN())
        with _ -> ()
    let arr = candidates.ToArray()
    if arr.Length <= sample then arr
    else
        let stride = float arr.Length / float sample
        [| for i in 0 .. sample - 1 -> arr.[int (float i * stride)] |]

/// What `values fit` works from.
type FitParams =
    { /// The engine whose evals are fitted (--from eval only).
      Engine: TypesDef.CoreTypes.EngineConfig option
      /// An EPD or PGN file that exists.
      Positions: string
      Sample: int option
      Nodes: int option
      ByPhase: bool
      Player: string option
      Bootstrap: int option }

let runPieceValueFitEval (p: FitParams) =
  do
      let config = p.Engine.Value
      let nodes = p.Nodes |> Option.defaultValue 1
      let path = p.Positions
      let isPgn = path.EndsWith(".pgn", StringComparison.OrdinalIgnoreCase)
      let sampleN = p.Sample |> Option.defaultValue (if isPgn then 500 else Int32.MaxValue)
      let fens =
          if isPgn then
              pvCollectFensFromPgn path sampleN
          else
              EPDExtractor.readEPDs path
              |> Seq.map (fun e -> e.FEN)
              |> Seq.filter (fun f -> not (String.IsNullOrWhiteSpace f))
              |> Seq.truncate sampleN
              |> Seq.toArray

      printfn ""
      printfn "Engine:    %s" config.Name
      printfn "Positions: %s  (%s)" path (if isPgn then "PGN, quiet-filtered" else "EPD")
      printfn "Sampled:   %d   Search: nodes %d   Target: logit(White score)" fens.Length nodes
      printfn ""

      let engine = Engines.start config
      try
          printf "Evaluating %d positions " fens.Length
          let mutable nCheck = 0
          let mutable nCapture = 0
          let mutable nSat = 0
          let data =
              [| for idx in 0 .. fens.Length - 1 do
                   let fen = fens.[idx]
                   try
                       // Normalise short EPD FENs to 6 fields for the engine.
                       let fs = pvFields fen
                       let fen = if fs.Length = 4 then fen + " 0 1" else fen
                       let squares = pvParseFenBoard fen
                       let stmWhite = fs.Length < 2 || fs.[1] = "w"
                       if (idx + 1) % 20 = 0 then printf "."
                       if pvInCheck fen then
                           nCheck <- nCheck + 1   // quiet filter: side to move in check
                       else
                           let r = Engines.search engine fen [] (Nodes nodes) false
                           match pvWdlToWhiteLogit stmWhite r.Wdl with
                           | Some y when abs y < 4.0 ->   // keep the linear (unsaturated) regime
                               if pvBestIsCapture squares r.Bestmove then
                                   nCapture <- nCapture + 1   // quiet filter: best move is a capture
                               else
                                   let md = pvMaterialDiff squares
                                   let stm = if stmWhite then 1.0 else -1.0
                                   let feat = [| 1.0; md.[0]; md.[1]; md.[2]; md.[3]; md.[4]; stm |]
                                   yield feat, y, pvTotalPieces squares
                           | Some _ -> nSat <- nSat + 1    // saturated (|logit| ≥ 4)
                           | None -> ()
                   with
                   | :? CustomException.EngineStartupException as ex -> raise ex   // dead engine: stop the batch (no reraise inside a comprehension)
                   | _ -> () |]
          printfn " %d usable  (dropped: %d in-check, %d capture-best, %d saturated).\n"
              data.Length nCheck nCapture nSat

          let names = [| "Pawn"; "Knight"; "Bishop"; "Rook"; "Queen" |]
          let fitAndReport (label: string) (rows: (float[] * float * int)[]) =
              if rows.Length < 12 then
                  printfn "── %s ──  only %d positions — too few to fit.\n" label rows.Length
              else
                  let X = rows |> Array.map (fun (f, _, _) -> f)
                  let y = rows |> Array.map (fun (_, v, _) -> v)
                  let variances =
                      [| for c in 1 .. 5 ->
                           let col = X |> Array.map (fun f -> f.[c])
                           let mean = Array.average col
                           col |> Array.averageBy (fun v -> (v - mean) * (v - mean)) |]
                  let beta = pvFitL1 X y
                  let bp = beta.[1]
                  let preds = X |> Array.map (fun f -> Array.fold2 (fun a b x -> a + b * x) 0.0 beta f)
                  let resid = Array.map2 (fun yy pp -> yy - pp) y preds
                  let mad = resid |> Array.averageBy abs
                  let ybar = Array.average y
                  let ssTot = y |> Array.sumBy (fun v -> (v - ybar) * (v - ybar))
                  let ssRes = resid |> Array.sumBy (fun v -> v * v)
                  let r2 = if ssTot > 1e-9 then 1.0 - ssRes / ssTot else 0.0
                  let valStr i = if abs bp > 1e-9 then sprintf "%.2f" (beta.[i] / bp) else "n/a"
                  printfn "── %s (n=%d) ──" label rows.Length
                  printfn "  Material variance: %s"
                      (String.concat "  " (Array.map2 (fun (nm: string) v -> sprintf "%s %.2f" nm v) names variances))
                  if abs bp < 1e-6 || (variances |> Array.exists (fun v -> v < 0.02)) then
                      printfn "  ⚠️  Near-zero material variance — these positions are ~balanced, so the"
                      printfn "      coefficients below are unreliable. Use a material-imbalanced position set."
                  printfn "  βP: %.3f logits/pawn   tempo: %+.2f pawns   R²: %.3f   MAD: %.3f"
                      bp (if abs bp > 1e-9 then beta.[6] / bp else 0.0) r2 mad
                  printfn "  Implied values (pawn = 1.00):"
                  printfn "    Pawn 1.00   Knight %s   Bishop %s   Rook %s   Queen %s"
                      (valStr 2) (valStr 3) (valStr 4) (valStr 5)
                  printfn ""

          fitAndReport "All positions" data
          if p.ByPhase then
              fitAndReport "Middlegame (>16 pieces)" (data |> Array.filter (fun (_, _, tp) -> tp > 16))
              fitAndReport "Endgame (<=16 pieces)"   (data |> Array.filter (fun (_, _, tp) -> tp <= 16))
      finally
          Engines.quit engine

/// DeepMind-style fit: regress game OUTCOMES (not engine eval) on material imbalance
/// through a tanh link. No engine needed — the target is the PGN result.
let runPieceValueFitOutcome (p: FitParams) =
  do
      let path = p.Positions
      if not (path.EndsWith(".pgn", StringComparison.OrdinalIgnoreCase)) then
          failwith "--outcome requires a PGN file (it uses game results as the regression target)"
      let sampleN = p.Sample |> Option.defaultValue 20000
      printfn ""
      printfn "Positions: %s  (game-outcome regression — DeepMind tanh model)" path
      printfn "Target:    game result z in {-1,0,+1}, White's perspective   (engine arg ignored)"
      printfn ""
      printf "Collecting positions from games ..."
      let data = pvCollectOutcomeRows path sampleN
      printfn " %d positions.\n" data.Length

      let names = [| "Pawn"; "Knight"; "Bishop"; "Rook"; "Queen" |]
      let fit (label: string) (rows: (float[] * float * int * int)[]) =
          if rows.Length < 50 then
              printfn "── %s ──  only %d positions — too few to fit.\n" label rows.Length
          else
              let X = rows |> Array.map (fun (f, _, _, _) -> f)
              let z = rows |> Array.map (fun (_, v, _, _) -> v)
              // features are [ΔP;ΔN;ΔB;ΔR;ΔQ] (cols 0..4), no intercept
              let variances =
                  [| for c in 0 .. 4 ->
                       let col = X |> Array.map (fun f -> f.[c])
                       let m = Array.average col
                       col |> Array.averageBy (fun v -> (v - m) * (v - m)) |]
              let beta = pvFitTanh X z
              let bp = beta.[0]
              let preds = X |> Array.map (fun f -> tanh (Array.fold2 (fun a b x -> a + b * x) 0.0 beta f))
              let mse = Array.map2 (fun zz pp -> (zz - pp) * (zz - pp)) z preds |> Array.average
              let imbalanced = Array.zip z preds |> Array.filter (fun (zz, _) -> zz <> 0.0)
              let signAcc =
                  if imbalanced.Length = 0 then 0.0
                  else imbalanced |> Array.averageBy (fun (zz, pp) -> if sign zz = sign pp then 1.0 else 0.0)
              let valStr i = if abs bp > 1e-9 then sprintf "%.2f" (beta.[i] / bp) else "n/a"
              printfn "── %s (n=%d) ──" label rows.Length
              printfn "  Material variance: %s"
                  (String.concat "  " (Array.map2 (fun (nm: string) v -> sprintf "%s %.2f" nm v) names variances))
              if abs bp < 1e-9 || (variances |> Array.exists (fun v -> v < 0.02)) then
                  printfn "  ⚠️  Near-zero material variance — coefficients unreliable for this split."
              printfn "  βP: %.3f (tanh units/pawn)   MSE: %.3f   sign-acc on decisive: %.1f%%"
                  bp mse (signAcc * 100.0)
              printfn "  Implied values (pawn = 1.00):"
              printfn "    Pawn 1.00   Knight %s   Bishop %s   Rook %s   Queen %s"
                  (valStr 1) (valStr 2) (valStr 3) (valStr 4)
              match p.Bootstrap with
              | Some b when b > 0 ->
                  let estimate (rs: (float[] * float * int * int)[]) =
                      let bx = pvFitTanh (rs |> Array.map (fun (f, _, _, _) -> f)) (rs |> Array.map (fun (_, v, _, _) -> v))
                      if abs bx.[0] < 1e-9 then None
                      else Some [| bx.[1] / bx.[0]; bx.[2] / bx.[0]; bx.[3] / bx.[0]; bx.[4] / bx.[0] |]
                  match pvBootstrapCI rows estimate b with
                  | Some (ci, (glo, gmed, ghi)) ->
                      let s (lo, hi) = sprintf "[%.2f, %.2f]" lo hi
                      printfn "  95%% CI (game-clustered bootstrap, B=%d):" b
                      printfn "    Knight %s   Bishop %s   Rook %s   Queen %s"
                          (s ci.[0]) (s ci.[1]) (s ci.[2]) (s ci.[3])
                      printfn "    Bishop−Knight gap: %+.2f  95%% CI [%+.2f, %+.2f]  (%s zero)"
                          gmed glo ghi (if glo > 0.0 then "excludes" else "includes")
                  | None -> printfn "  (bootstrap: too few games to resample)"
              | _ -> ()
              printfn ""
      fit "All positions" data
      if p.ByPhase then
          fit "Middlegame (>16 pieces)" (data |> Array.filter (fun (_, _, tp, _) -> tp > 16))
          fit "Endgame (<=16 pieces)"   (data |> Array.filter (fun (_, _, tp, _) -> tp <= 16))

/// Regress the PGN's embedded eval (wv, in pawns) on material imbalance — linear,
/// material-only (no intercept), L1. Coefficients are piece values in pawns directly.
/// A continuous bounded target avoids the queen blow-up the tanh-on-outcome model had.
let runPieceValueFitPgnEval (p: FitParams) =
  do
      let path = p.Positions
      if not (path.EndsWith(".pgn", StringComparison.OrdinalIgnoreCase)) then
          failwith "--pgneval requires a PGN file with wv= eval annotations"
      let sampleN = p.Sample |> Option.defaultValue 20000
      printfn ""
      printfn "Positions: %s  (regression on embedded engine eval wv=, in pawns)" path
      printfn "Target:    in-game eval, White's perspective   (engine arg ignored)"
      match p.Player with
      | Some pn -> printfn "Player:    only plies where '%s' was on move (single-net eval values)" pn
      | None -> printfn "Player:    all moves (blended across both engines)"
      printfn ""
      printf "Collecting positions with evals from games ..."
      let data = pvCollectPgnEvalRows path sampleN p.Player
      printfn " %d positions.\n" data.Length

      let names = [| "Pawn"; "Knight"; "Bishop"; "Rook"; "Queen" |]
      let fit (label: string) (rows: (float[] * float * int * int)[]) =
          if rows.Length < 50 then
              printfn "── %s ──  only %d positions — too few to fit.\n" label rows.Length
          else
              let X = rows |> Array.map (fun (f, _, _, _) -> f)
              let y = rows |> Array.map (fun (_, v, _, _) -> v)
              // features: [intercept; ΔP; ΔN; ΔB; ΔR; ΔQ; stm] — material in cols 1..5
              let variances =
                  [| for c in 1 .. 5 ->
                       let col = X |> Array.map (fun f -> f.[c])
                       let m = Array.average col
                       col |> Array.averageBy (fun v -> (v - m) * (v - m)) |]
              let beta = pvFitL1 X y      // linear L1; material coefficients are pawn values
              let bp = beta.[1]
              let preds = X |> Array.map (fun f -> Array.fold2 (fun a b x -> a + b * x) 0.0 beta f)
              let resid = Array.map2 (fun yy pp -> yy - pp) y preds
              let mad = resid |> Array.averageBy abs
              let ybar = Array.average y
              let ssTot = y |> Array.sumBy (fun v -> (v - ybar) * (v - ybar))
              let ssRes = resid |> Array.sumBy (fun v -> v * v)
              let r2 = if ssTot > 1e-9 then 1.0 - ssRes / ssTot else 0.0
              let nrm i = if abs bp > 1e-9 then sprintf "%.2f" (beta.[i] / bp) else "n/a"
              printfn "── %s (n=%d) ──" label rows.Length
              printfn "  Material variance: %s"
                  (String.concat "  " (Array.map2 (fun (nm: string) v -> sprintf "%s %.2f" nm v) names variances))
              if variances |> Array.exists (fun v -> v < 0.02) then
                  printfn "  ⚠️  Near-zero material variance — coefficients unreliable for this split."
              printfn "  Raw coefficients (eval-pawns per piece):"
              printfn "    Pawn %.2f   Knight %.2f   Bishop %.2f   Rook %.2f   Queen %.2f"
                  beta.[1] beta.[2] beta.[3] beta.[4] beta.[5]
              printfn "  Normalised (pawn = 1.00):  Knight %s   Bishop %s   Rook %s   Queen %s"
                  (nrm 2) (nrm 3) (nrm 4) (nrm 5)
              printfn "  intercept: %+.2f   tempo: %+.2f   βP: %.2f   R²: %.3f   MAD: %.3f pawns"
                  beta.[0] beta.[6] bp r2 mad
              match p.Bootstrap with
              | Some b when b > 0 ->
                  let estimate (rs: (float[] * float * int * int)[]) =
                      let bx = pvFitL1 (rs |> Array.map (fun (f, _, _, _) -> f)) (rs |> Array.map (fun (_, v, _, _) -> v))
                      if abs bx.[1] < 1e-9 then None
                      else Some [| bx.[2] / bx.[1]; bx.[3] / bx.[1]; bx.[4] / bx.[1]; bx.[5] / bx.[1] |]
                  match pvBootstrapCI rows estimate b with
                  | Some (ci, (glo, gmed, ghi)) ->
                      let s (lo, hi) = sprintf "[%.2f, %.2f]" lo hi
                      printfn "  95%% CI (game-clustered bootstrap, B=%d):" b
                      printfn "    Knight %s   Bishop %s   Rook %s   Queen %s"
                          (s ci.[0]) (s ci.[1]) (s ci.[2]) (s ci.[3])
                      printfn "    Bishop−Knight gap: %+.2f  95%% CI [%+.2f, %+.2f]  (%s zero)"
                          gmed glo ghi (if glo > 0.0 then "excludes" else "includes")
                  | None -> printfn "  (bootstrap: too few games to resample)"
              | _ -> ()
              printfn ""
      fit "All positions" data
      if p.ByPhase then
          fit "Middlegame (>16 pieces)" (data |> Array.filter (fun (_, _, tp, _) -> tp > 16))
          fit "Endgame (<=16 pieces)"   (data |> Array.filter (fun (_, _, tp, _) -> tp <= 16))

// ---------------------------------------------------------------- commands

open Cli.Spec
open Cli.Parse
open Cli.App

let positionSpec =
    { command [ "values"; "position" ] "Engines and positions" "Piece values from an engine's evaluations in one position" with
        Description =
          Some "Removes each piece in turn and measures how much the eval drops, from the piece owner's view: a ranked table, the board, and the average per piece type. The default search is one node, a pure network eval."
        Args = [ arg "engine" engineArgHelp; fenArg false $"{fenHelp} (default startpos)" ]
        Options = [ moves ] @ limitOptions 1 @ [ uci; args ]
        OneOf = [ limitOneOf ]
        Examples =
          [ "eb-cli values position lc0.json \"r1bqkbnr/pppp1ppp/2n5/4p3/4P3/5N2/PPPP1PPP/RNBQKB1R w KQkq - 2 3\"", "What each piece is worth to the net"
            "eb-cli values position ceres.json --moves \"e4 e5 Nf3 Nc6\" --nodes 100", "After four moves, with a 100-node search" ] }

let position (p: Parsed) =
    let config = engine p "engine" "uci"
    let start = fen p "fen"
    runPieceValues config (after start (movesFrom p start)) (limit p 1)
    Cli.Output.ExitOk

let fitSpec =
    { command [ "values"; "fit" ] "Engines and positions" "Piece values fitted over many positions" with
        Description =
          Some "Regresses a target against the material imbalance of many positions; the coefficients are the implied piece values. --from eval fits the engine's evaluations (needs --engine), outcome the games' results, pgn-eval the evals in the PGN's comments."
        Args = [ arg "positions" "an EPD or PGN file" ]
        Options =
          [ value "from" "eval|outcome|pgn-eval" "What is fitted" |> withDefault "eval"
            value "engine" "ENGINE" engineArgHelp
            value "sample" "N" "Use this many positions (default: all of an EPD, 500 of a PGN for eval, 20000 for outcome and pgn-eval)"
            value "nodes" "N" "Nodes per position for --from eval" |> withDefault "1"
            switch "by-phase" "Also fit middlegame and endgame separately"
            value "player" "NAME" "Only the games of this player (pgn-eval)"
            value "bootstrap" "N" "Confidence intervals from N bootstrap samples (outcome, pgn-eval)"
            uci
            args ]
        Examples =
          [ "eb-cli values fit quiet.epd --engine lc0.json", "The net's piece values over an EPD"
            "eb-cli values fit games.pgn --from outcome --by-phase", "Values implied by the results" ] }

let fit (p: Parsed) =
    let path = inputFile "positions file" (argOf p "positions").Value
    let engineGiven = valueOf p "engine"
    let from = defaultArg (valueOf p "from") "eval"
    let positive name = intOf p name |> Option.map (fun n -> if n > 0 then n else raise (Usage $"--{name} must be above 0, got {n}"))
    let fitParams =
        { Engine = engineGiven |> Option.map (fun given -> engineFrom p given "uci")
          Positions = path
          Sample = positive "sample"
          Nodes = positive "nodes"
          ByPhase = has p "by-phase"
          Player = valueOf p "player"
          Bootstrap = positive "bootstrap" }
    match from with
    | "eval" ->
        if engineGiven.IsNone then raise (Usage "--from eval needs --engine")
        runPieceValueFitEval fitParams
    | "outcome" | "pgn-eval" when not (path.EndsWith(".pgn", StringComparison.OrdinalIgnoreCase)) ->
        raise (Usage $"--from {from} needs a PGN")
    | "outcome" -> runPieceValueFitOutcome fitParams
    | "pgn-eval" -> runPieceValueFitPgnEval fitParams
    | other -> raise (Usage $"--from expects eval, outcome or pgn-eval, got '{other}'")
    Cli.Output.ExitOk

let imbalanceSpec =
    { command [ "pgn"; "imbalance" ] "PGN files" "Results by material imbalance" with
        Description = Some "How the games went when one side had, for instance, a bishop for a knight, the bishop pair, or a rook for a minor piece: the score and the PGN evals per matchup, from the positions past ply 16."
        Args = [ arg "pgn" "the games" ]
        Examples = [ "eb-cli pgn imbalance games.pgn", "The matchups and how they scored" ] }

let imbalance (p: Parsed) =
    runPvCombo (inputFile "PGN" (argOf p "pgn").Value)
    Cli.Output.ExitOk

let entries: Entry list =
    [ { Spec = positionSpec; Run = position }
      { Spec = fitSpec; Run = fit }
      { Spec = imbalanceSpec; Run = imbalance } ]

