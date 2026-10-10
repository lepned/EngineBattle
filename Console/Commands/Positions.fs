/// analyze, compare, position, tablebase: one engine search, two engines side by side, the facts of
/// a position, the tablebase moves of a position.
module Commands.Positions

open System
open Cli.Spec
open Cli.Parse
open Cli.Output
open Cli.Groups
open Cli.App

[<Literal>]
let Group = "Engines and positions"

let private fmtN (n: int64) = n.ToString("N0", Globalization.CultureInfo.InvariantCulture)

let private elapsedText (t: TimeSpan) =
    if t.TotalSeconds < 1.0 then $"{t.Milliseconds} ms"
    elif t.TotalMinutes < 1.0 then $"{t.TotalSeconds:F1} s"
    else $"{int t.TotalMinutes}m {t.Seconds:D2}s"

// public: System.Text.Json writes only public properties
type LimitJson =
    { Nodes: Nullable<int>
      MovetimeMs: Nullable<int>
      Depth: Nullable<int> }

let private limitJson (l: Limit) =
    match l with
    | Nodes n -> { Nodes = Nullable n; MovetimeMs = Nullable(); Depth = Nullable() }
    | MoveTime ms -> { Nodes = Nullable(); MovetimeMs = Nullable ms; Depth = Nullable() }
    | Depth d -> { Nodes = Nullable(); MovetimeMs = Nullable(); Depth = Nullable d }

/// White to move in the position after the moves.
let private whiteToMove (fen: string) (moves: string list) = (fen.Split(' ').[1] = "w") = (moves.Length % 2 = 0)

/// The engine's WDL (side to move) from White's view, like the eval.
let private wdlWhite (whiteMoves: bool) (r: Engines.SearchResult) =
    r.Wdl |> Option.map (fun w -> if whiteMoves then w.Win, w.Draw, w.Loss else w.Loss, w.Draw, w.Win)

let private say format = Printf.kprintf (fun (s: string) -> (resultWriter ()).WriteLine s) format

let private searchJson (whiteMoves: bool) (r: Engines.SearchResult) =
    {| Depth = r.Depth
       SelDepth = r.SelDepth
       Eval = Engines.evalJson r.Eval
       Nodes = r.Nodes
       Nps = r.Nps
       Eps = r.Eps
       TimeMs = int64 r.Time.TotalMilliseconds
       TbHits = r.TbHits
       Wdl = (match wdlWhite whiteMoves r with Some(w, d, l) -> box {| Win = w; Draw = d; Loss = l |} | None -> null)
       Bestmove = r.Bestmove
       Pv = r.Pv
       SanPv = r.SanPv |}

// ---------------------------------------------------------------- analyze

let analyzeSpec =
    { command [ "analyze" ] Group "Analyze a position with an engine" with
        Description =
          Some "Searches the position once: the engine's info lines as they come on stderr, the summary on stdout (eval and WDL from White's view). A checkmate or stalemate is reported without a search. With --json, only the result, on stdout."
        Args = [ arg "engine" engineArgHelp; fenArg false $"{fenHelp} (default startpos)" ]
        Options = [ moves ] @ limitOptions 1_000_000 @ [ uci; args; switch "list-options" "List the engine's UCI options and their defaults instead of searching"; json ]
        OneOf = [ limitOneOf ]
        Examples =
          [ "eb-cli analyze Stockfish19", "Search the start position, 1,000,000 nodes"
            "eb-cli analyze lc0.json \"8/8/8/8/8/2k5/2Q5/K7 b - - 0 1\" --movetime 2000", "Two seconds on a position"
            "eb-cli analyze sf.exe --moves \"e4 e5 Nf3\" --depth 20 --uci Threads=8", "After three moves, to depth 20, with 8 threads"
            "eb-cli analyze lc0.json --list-options", "The engine's UCI options" ] }

let analyze (p: Parsed) =
    let config = engine p "engine" "uci"
    let fenText = fen p "fen"
    let played = movesFrom p fenText
    let lim = limit p 1_000_000
    let jsonMode = has p "json"
    let whiteMoves = whiteToMove fenText played
    let status = (ChessLibrary.BoardUtils.getPositionStatus (after fenText played)).Status
    let terminal = status = "checkmate" || status = "stalemate"
    // the library's notices (options set, engine shut down) go to stderr; the summary is stdout's
    withResultsOnStdout (fun () ->
    if not jsonMode then
        say "Engine: %s" config.Name
        say "FEN:    %s" fenText
        if not played.IsEmpty then say "Moves:  %s" (String.concat " " played)
    if terminal && not (has p "list-options") then
        // nothing to search: what the position is, not an engine failure
        if jsonMode then
            let result = {| Engine = config.Name; Fen = fenText; Moves = played; Status = status; Limit = limitJson lim; Search = null |}
            printJson (envelope (name p.Command) true (box result) [])
        else
            say ""
            say "No search: the position is %s" status
        ExitOk
    else
    let a = Engines.start config
    try
        if has p "list-options" then
            let options = a.Engine.GetAllDefaultOptions() |> Seq.map (fun kv -> kv.Key, string kv.Value) |> List.ofSeq
            if jsonMode then
                printJson (envelope (name p.Command) true (box {| Engine = config.Name; Options = dict options |}) [])
            else
                say ""
                for k, v in options do
                    say "  %-30s  %s" k v
            ExitOk
        else
            if not jsonMode then
                say "Search: %s" lim.Text
                say ""
            let r = Engines.search a fenText played lim (not jsonMode)
            let ok = r.Depth > 0 || r.Bestmove <> ""
            if jsonMode then
                let result = {| Engine = config.Name; Fen = fenText; Moves = played; Status = status; Limit = limitJson lim; Search = searchJson whiteMoves r |}
                let errors = if ok then [] else [ { Code = "no-search"; Message = "the engine sent no search information" } ]
                printJson (envelope (name p.Command) ok (box result) errors)
            elif ok then
                say ""
                say "Depth:    %d (seldepth %d)" r.Depth r.SelDepth
                say "Eval:     %s" (r.Eval.ToString())
                say "Nodes:    %s" (fmtN r.Nodes)
                let eps = if r.Eps > 0.0 then $" ({ChessLibrary.GameAnalysis.Formatting.formatEPS r.Eps})" else ""
                say "NPS:      %s%s" (ChessLibrary.GameAnalysis.Formatting.formatNPS (float r.Nps)) eps
                say "Time:     %s" (elapsedText r.Time)
                say "TB hits:  %d" r.TbHits
                match wdlWhite whiteMoves r with
                | Some(w, d, l) -> say "WDL:      %d-%d-%d (White's view)" (int w) (int d) (int l)
                | None -> ()
                say "Bestmove: %s" r.Bestmove
                if not (String.IsNullOrWhiteSpace r.Pv) then
                    say "PV:       %s" r.Pv
                    say "PV (SAN): %s" r.SanPv
            else
                error "the engine sent no search information"
            if ok then ExitOk else ExitFailed
    finally
        Engines.quit a)

// ---------------------------------------------------------------- compare

let compareSpec =
    { command [ "compare" ] Group "Compare two engines on one or more positions" with
        Description =
          Some "Both engines search each position with the same limit; a table of evals, speed and best moves, then a summary. The position is the start position unless --fen or --positions says otherwise."
        Args = [ arg "engine1" engineArgHelp; arg "engine2" engineArgHelp ]
        Options =
          [ value "fen" "\"FEN\"" "One position (quoted)"
            value "positions" "FILE" "An EPD file: every position in it" ]
          @ limitOptions 1_000_000
          @ [ value "uci1" "NAME=VALUE" "Set a UCI option of engine 1" |> repeatable
              value "uci2" "NAME=VALUE" "Set a UCI option of engine 2" |> repeatable
              value "threshold" "PAWNS" "Show only positions where the evals differ by at least this many pawns, e.g. 0.5 (the summary counts all)"
              json ]
        OneOf = [ limitOneOf; [ "fen"; "positions" ] ]
        Examples =
          [ "eb-cli compare lc0_a.json lc0_b.json --nodes 1000", "Speed and eval on the start position"
            "eb-cli compare sf.json lc0.json --positions suite.epd --movetime 500 --threshold 0.5", "Only the positions where they disagree by half a pawn or more" ] }

let compare (p: Parsed) =
    let config1 = engine p "engine1" "uci1"
    let config2 = engine p "engine2" "uci2"
    let positions =
        match valueOf p "positions", valueOf p "fen" with
        | Some file, _ ->
            ChessLibrary.EPDExtractor.readEPDs (inputFile "positions file" file) |> Seq.map (fun e -> e.FEN) |> Array.ofSeq
        | None, Some f ->
            let v = ChessLibrary.BoardUtils.validateFen f
            if f.Equals("startpos", StringComparison.OrdinalIgnoreCase) then [| startFen |]
            elif v.IsValid then [| ChessLibrary.BoardUtils.normalizeFen f |]
            else raise (Usage $"""--fen: not a valid FEN: {String.Join("; ", v.Errors)}""")
        | None, None -> [| startFen |]
    if positions.Length = 0 then raise (Usage "the positions file has no positions")
    let lim = limit p 1_000_000
    let threshold = floatOf p "threshold"
    let jsonMode = has p "json"
    withResultsOnStdout (fun () ->
    let e1 = Engines.start config1
    let e2 =
        try Engines.start config2
        with _ ->
            Engines.quit e1
            reraise ()
    try
        if not jsonMode then
            say "E1 = %s" config1.Name
            say "E2 = %s" config2.Name
            say "Positions: %d, search: %s" positions.Length lim.Text
            say ""
        let fenW = positions |> Array.map String.length |> Array.max |> max 3
        if not jsonMode then
            say "%-4s  %-*s  %8s  %8s  %8s  %10s  %10s  %6s  %s" "#" fenW "FEN" "Eval E1" "Eval E2" "Diff" "NPS E1" "NPS E2" "Ratio" "Move"
            say "%s" (String.replicate (fenW + 76) "-")
        let rows = Collections.Generic.List<obj>()
        let mutable failed = false
        // with the position's number: a failed position is not in here
        let results = Collections.Generic.List<int * Engines.SearchResult * Engines.SearchResult * float option>()
        let mutable stop = false
        for i in 0 .. positions.Length - 1 do
            if not stop then
                let fenText = positions.[i]
                try
                    let r1 = Engines.search e1 fenText [] lim false
                    let r2 = Engines.search e2 fenText [] lim false
                    let diff =
                        match Engines.pawnsOf r1.Eval, Engines.pawnsOf r2.Eval with
                        | Some a, Some b -> Some(abs (a - b))
                        | _ -> None
                    results.Add((i + 1, r1, r2, diff))
                    let white = whiteToMove fenText []
                    rows.Add(box {| Index = i + 1; Fen = fenText; E1 = searchJson white r1; E2 = searchJson white r2; EvalDiff = Option.toNullable diff; Error = (null: string) |})
                    let show = match threshold, diff with Some t, Some d -> d >= t | _ -> true
                    if show && not jsonMode then
                        let moveText = if r1.Bestmove = r2.Bestmove then r1.Bestmove else $"{r1.Bestmove}/{r2.Bestmove}"
                        let ratio = if r1.Nps > 0L && r2.Nps > 0L then $"{float r1.Nps / float r2.Nps:F1}x" else "N/A"
                        say "%-4d  %-*s  %8s  %8s  %8s  %10s  %10s  %6s  %s" (i + 1) fenW fenText (r1.Eval.ToString()) (r2.Eval.ToString())
                            (match diff with Some d -> $"{d:F2}" | None -> "N/A")
                            (ChessLibrary.GameAnalysis.Formatting.formatNPS (float r1.Nps))
                            (ChessLibrary.GameAnalysis.Formatting.formatNPS (float r2.Nps)) ratio moveText
                with
                | :? ChessLibrary.CustomException.EngineStartupException as ex ->
                    // a refused or dead engine does not recover on the next position
                    error $"position {i + 1}: {ex.Message}"
                    rows.Add(box {| Index = i + 1; Fen = fenText; E1 = null; E2 = null; EvalDiff = Nullable<float>(); Error = ex.Message |})
                    failed <- true
                    stop <- true
                | ex ->
                    error $"position {i + 1}: {ex.Message}"
                    rows.Add(box {| Index = i + 1; Fen = fenText; E1 = null; E2 = null; EvalDiff = Nullable<float>(); Error = ex.Message |})
                    failed <- true
        let n = results.Count
        let avg (f: Engines.SearchResult * Engines.SearchResult * float option -> float option) =
            let xs = results |> Seq.choose (fun (_, a, b, d) -> f (a, b, d)) |> Array.ofSeq
            if xs.Length = 0 then None else Some(Array.average xs)
        let agree = results |> Seq.filter (fun (_, a, b, _) -> a.Bestmove = b.Bestmove) |> Seq.length
        let diffs = results |> Seq.choose (fun (i, _, _, d) -> d |> Option.map (fun d -> i, d)) |> Array.ofSeq
        let nps1 = avg (fun (a, _, _) -> if a.Nps > 0L then Some(float a.Nps) else None)
        let nps2 = avg (fun (_, b, _) -> if b.Nps > 0L then Some(float b.Nps) else None)
        let eps1 = avg (fun (a, _, _) -> if a.Eps > 0.0 then Some a.Eps else None)
        let eps2 = avg (fun (_, b, _) -> if b.Eps > 0.0 then Some b.Eps else None)
        let ratio a b = match a, b with Some x, Some y when y > 0.0 -> Some(x / y) | _ -> None
        if jsonMode then
            let summary =
                {| Positions = n
                   MoveAgreement = agree
                   AvgEvalDiff = (if diffs.Length = 0 then Nullable() else Nullable(diffs |> Array.averageBy snd))
                   MaxEvalDiff = (if diffs.Length = 0 then Nullable() else Nullable(diffs |> Array.maxBy snd |> snd))
                   AvgNps1 = Option.toNullable nps1
                   AvgNps2 = Option.toNullable nps2
                   NpsRatio = Option.toNullable (ratio nps1 nps2)
                   AvgEps1 = Option.toNullable eps1
                   AvgEps2 = Option.toNullable eps2
                   EpsRatio = Option.toNullable (ratio eps1 eps2) |}
            let result = {| Engine1 = config1.Name; Engine2 = config2.Name; Limit = limitJson lim; Positions = rows; Summary = summary |}
            let errors = if failed then [ { Code = "position-failed"; Message = "a position could not be searched (see positions[].error)" } ] else []
            printJson (envelope (name p.Command) (not failed) (box result) errors)
        elif n > 0 then
            say ""
            say "Summary (%d positions)" n
            say "Move agreement: %d/%d (%.1f%%)" agree n (100.0 * float agree / float n)
            if diffs.Length > 0 then
                let worstPosition, worst = diffs |> Array.maxBy snd
                say "Avg eval diff:  %.2f pawns" (diffs |> Array.averageBy snd)
                say "Max eval diff:  %.2f pawns (position %d)" worst worstPosition
            match nps1, nps2, ratio nps1 nps2 with
            | Some a, Some b, Some r ->
                say "Avg NPS:        %s vs %s (ratio %.2fx)" (ChessLibrary.GameAnalysis.Formatting.formatNPS a) (ChessLibrary.GameAnalysis.Formatting.formatNPS b) r
            | _ -> ()
            // EPS = network evaluations per second, for engines that report it (Ceres)
            match eps1, eps2, ratio eps1 eps2 with
            | Some a, Some b, Some r ->
                say "Avg EPS:        %s vs %s (ratio %.2fx)" (ChessLibrary.GameAnalysis.Formatting.formatEPS a) (ChessLibrary.GameAnalysis.Formatting.formatEPS b) r
            | _ -> ()
        if failed then ExitFailed else ExitOk
    finally
        Engines.quit e1
        Engines.quit e2)

// ---------------------------------------------------------------- position

let positionSpec =
    { command [ "position" ] Group "Facts about a position: status, legal moves, attacks, pins, insights" with
        Description =
          Some "Prints the position's status and legal moves; --json gives everything (attackers, pins, hanging pieces, forks and the other insights) for tools and validators. Edits change the position first, in the order set-piece, remove, side, castling, ep."
        Args = [ fenArg false fenHelp ]
        Options =
          [ moves
            value "square" "SQ" "Also the attackers, attacks, pin ray and safe squares of this square"
            value "pv" "\"LINE\"" "Write this line (SAN or UCI) in SAN"
            value "set-piece" "SQ=P" "Put a piece on a square (e4=Q, e4=n)" |> repeatable
            value "remove" "SQ" "Take the piece off a square" |> repeatable
            value "side" "w|b" "Set the side to move"
            value "castling" "RIGHTS" "Set the castling rights (KQkq, -)"
            value "ep" "SQ" "Set the en passant square"
            switch "epd" "Print the position as an EPD line instead"
            value "op" "NAME=VALUE" "An EPD operation for --epd (bm=e4, id=\"test 1\")" |> repeatable
            value "svg" "FILE" "Also draw the position with its insights as an SVG"
            force
            value "positions" "FILE" "Every position of an EPD file instead of one FEN"
            json ]
        Examples =
          [ "eb-cli position startpos", "Status and legal moves"
            "eb-cli position \"r3k2r/8/8/8/8/8/8/R3K2R w KQkq - 0 1\" --square e1 --json", "Everything, with the king's square"
            "eb-cli position startpos --set-piece e4=Q --remove d2 --epd --op id=edited", "An edited position as an EPD line"
            "eb-cli position --positions suite.epd --json", "Every position of a file" ] }

/// A square as a1..h8, or a usage error that names the option.
let private square (option: string) (sq: string) =
    if sq.Length = 2 && sq.[0] >= 'a' && sq.[0] <= 'h' && sq.[1] >= '1' && sq.[1] <= '8' then sq
    else raise (Usage $"--{option}: '{sq}' is not a square (a1-h8)")

let private applyEdits (p: Parsed) (fenText: string) =
    let mutable f = fenText
    for e in valuesOf p "set-piece" do
        match e.Split('=') with
        | [| sq; piece |] when piece.Length = 1 && "PNBRQKpnbrqk".Contains piece.[0] ->
            f <- ChessLibrary.BoardUtils.setPieceAt f (square "set-piece" sq) piece.[0]
        | _ -> raise (Usage $"--set-piece expects SQ=P with P one of PNBRQK (white) or pnbrqk (black), got '{e}'")
    for sq in valuesOf p "remove" do
        f <- ChessLibrary.BoardUtils.removePieceAt f (square "remove" sq)
    let setField (i: int) (v: string) =
        let parts = f.Split(' ')
        parts.[i] <- v
        f <- String.Join(" ", parts)
    match valueOf p "side" with
    | Some s when s = "w" || s = "b" ->
        setField 1 s
        // an en passant square the new side's pawns cannot use goes
        let parts = f.Split(' ')
        if parts.[3] <> "-" && not (ChessLibrary.BoardUtils.epCandidates f |> Array.contains parts.[3]) then setField 3 "-"
    | Some s -> raise (Usage $"--side expects w or b, got '{s}'")
    | None -> ()
    valueOf p "castling" |> Option.iter (setField 2)
    valueOf p "ep" |> Option.iter (setField 3)
    let v = ChessLibrary.BoardUtils.validateFen f
    if v.IsValid then f else raise (Usage $"""the edited position is not valid: {String.Join("; ", v.Errors)}""")

/// An EPD operation typed by convention, as python-chess types epd() operands.
let private epdOp (text: string) =
    match text.IndexOf '=' with
    | i when i > 0 ->
        let name, v = text.Substring(0, i), text.Substring(i + 1)
        let op =
            match name.ToLowerInvariant() with
            | "bm" | "am" -> ChessLibrary.BoardUtils.EpdMoves(v.Split(' ', StringSplitOptions.RemoveEmptyEntries))
            | "sm" | "pm" -> ChessLibrary.BoardUtils.EpdMove v
            | "pv" -> ChessLibrary.BoardUtils.EpdPv v
            | _ ->
                match Int32.TryParse(v, Globalization.NumberStyles.Integer, Globalization.CultureInfo.InvariantCulture) with
                | true, n -> ChessLibrary.BoardUtils.EpdInt n
                | _ ->
                    match Double.TryParse(v, Globalization.NumberStyles.Float, Globalization.CultureInfo.InvariantCulture) with
                    | true, x -> ChessLibrary.BoardUtils.EpdFloat x
                    | _ -> ChessLibrary.BoardUtils.EpdStr v
        name, op
    | _ -> raise (Usage $"--op expects NAME=VALUE, got '{text}'")

let private facts (fenText: string) (square: string option) (pvLine: string option) =
    let status = ChessLibrary.BoardUtils.getPositionStatus fenText
    let pv, sanPv =
        match pvLine with
        | None -> null, null
        | Some line ->
            match ChessLibrary.BoardUtils.pvToUci fenText line with
            | Some uci ->
                let board = ChessLibrary.Chess.Board()
                board.LoadFen fenText
                let buffer = Array.init 256 (fun _ -> Unchecked.defaultof<ChessLibrary.MoveTypes.TMove>)
                uci, ChessLibrary.BoardUtils.getShortSanPVFromLongSanPVFast buffer &board uci
            | None -> raise (Usage $"--pv: '{line}' is not a legal line from {fenText}")
    let squareFacts =
        square
        |> Option.map (fun sq ->
            let atts = ChessLibrary.BoardUtils.attackersOf fenText sq
            box
                {| Square = sq
                   AttackersWhite = atts.White
                   AttackersBlack = atts.Black
                   Attacks = ChessLibrary.BoardUtils.attacksFrom fenText sq
                   IsPinned = ChessLibrary.BoardUtils.isPinned fenText sq
                   PinRay = ChessLibrary.BoardUtils.pinRay fenText sq
                   SafeDestinations = ChessLibrary.BoardUtils.getSafeDestinations fenText sq |})
        |> Option.toObj
    {| Fen = fenText
       SideToMove = fenText.Split(' ').[1]
       Status = status.Status
       InsufficientMaterial = status.InsufficientMaterial
       FiftyMoveDraw = status.FiftyMoveDraw
       SeventyFiveMoveDraw = status.SeventyFiveMoveDraw
       LegalMoves = ChessLibrary.BoardUtils.legalMovesOf fenText
       Insights = ChessLibrary.BoardUtils.getPositionInsights fenText
       Pv = pv
       SanPv = sanPv
       SquareFacts = squareFacts |}

let position (p: Parsed) =
    let jsonMode = has p "json"
    match valueOf p "positions", argOf p "fen" with
    | Some _, Some _ -> raise (Usage "give a FEN or --positions, not both")
    | Some file, None ->
        let single = [ "moves"; "square"; "pv"; "set-piece"; "remove"; "side"; "castling"; "ep"; "epd"; "op"; "svg"; "force" ] |> List.filter (has p)
        if not single.IsEmpty then
            raise (Usage $"""--positions takes none of {String.Join(", ", single |> List.map (fun o -> "--" + o))} (they are for one position)""")
        let path = inputFile "positions file" file
        let all = ChessLibrary.EPDExtractor.readEPDs path |> Array.ofSeq
        let valid, invalid =
            all |> Array.partition (fun e -> (ChessLibrary.BoardUtils.validateFen e.FEN).IsValid)
        for e in invalid do
            warn $"skipping an invalid FEN: {e.FEN}"
        if jsonMode then
            let result = valid |> Array.map (fun e -> facts (ChessLibrary.BoardUtils.normalizeFen e.FEN) None None)
            printJson (envelope (name p.Command) true (box result) [])
        else
            for e in valid do
                let f = ChessLibrary.BoardUtils.normalizeFen e.FEN
                printfn "%s  %s, %d legal moves" f ((ChessLibrary.BoardUtils.getPositionStatus f).Status) (ChessLibrary.BoardUtils.legalMovesOf f).Length
        ExitOk
    | None, _ ->
        let start = fen p "fen"
        let fenText = applyEdits p (after start (movesFrom p start))
        if not (List.isEmpty (valuesOf p "op")) && not (has p "epd") then raise (Usage "--op is for --epd")
        // every check first: a usage error leaves no svg behind
        let svgPath = valueOf p "svg" |> Option.map (outputFile p)
        let epd = if has p "epd" then Some(ChessLibrary.BoardUtils.epdOf fenText (valuesOf p "op" |> List.map epdOp)) else None
        let f = facts fenText (valueOf p "square" |> Option.map (square "square")) (valueOf p "pv")
        svgPath
        |> Option.iter (fun path ->
            IO.File.WriteAllText(path, ChessLibrary.BoardSvg.renderWithInsights fenText)
            info $"svg written to {path}")
        match epd with
        | Some line -> printfn "%s" line
        | None ->
            if jsonMode then
                printJson (envelope (name p.Command) true (box f) [])
            else
                printfn "FEN:     %s" f.Fen
                printfn "To move: %s" (if f.SideToMove = "w" then "White" else "Black")
                printfn "Status:  %s%s" f.Status (if f.InsufficientMaterial then " (insufficient material)" else "")
                let sans = f.LegalMoves |> Array.map (fun m -> m.San)
                printfn "Moves:   %d  %s" sans.Length (String.Join(" ", sans))
                if not (isNull f.SanPv) then printfn "PV:      %s" f.SanPv
                match valueOf p "square" with
                | Some sq ->
                    let atts = ChessLibrary.BoardUtils.attackersOf fenText sq
                    let squares (s: string[]) = if s.Length = 0 then "none" else String.Join(" ", s)
                    printfn "%s: attacked by White %s, by Black %s; pinned: %s" sq (squares atts.White) (squares atts.Black) (if ChessLibrary.BoardUtils.isPinned fenText sq then "yes" else "no")
                | None -> ()
                info "(--json for attacks, pins and the position's insights)"
        ExitOk

// ---------------------------------------------------------------- tablebase

let tablebaseSpec =
    { command [ "tablebase" ] Group "Every move of a position from the Syzygy tables" with
        Description =
          Some "Wins, cursed wins, draws, blessed losses and losses, best first, with DTZ counted from the position. The halfmove clock counts unless --ignore-clock. The tables: --syzygy, else EB_SYZYGY_PATH, else syzygyPath in eb-cli.json."
        Args = [ fenArg true fenHelp ]
        Options = [ syzygy; switch "ignore-clock" "Read the halfmove clock as 0: what the position is worth if the clock starts now"; json ]
        Examples =
          [ "eb-cli tablebase 8/8/8/8/8/2k5/2Q5/K7 b - - 0 1 --syzygy D:/syzygy", "Black to move: the one draw first, then the losses"
            "eb-cli tablebase \"8/8/8/8/8/2k5/8/K6Q w - - 99 1\" --json", "A cursed win, as JSON" ] }

let tablebase (p: Parsed) =
    let fenText = fen p "fen"
    let path = syzygyPath p
    let ignoreClock = has p "ignore-clock"
    let r = ChessLibrary.TablebaseLookup.lookupWith (not ignoreClock) path fenText
    let answered =
        match r.Status with
        | ChessLibrary.TablebaseLookup.Answered _
        | ChessLibrary.TablebaseLookup.Checkmate
        | ChessLibrary.TablebaseLookup.Stalemate -> true
        | _ -> false
    // no folder, or one that is not there: the command was not given what it needs
    let usage =
        match r.Status with
        | ChessLibrary.TablebaseLookup.NoTablebaseFolder
        | ChessLibrary.TablebaseLookup.FoldersMissing _ -> true
        | _ -> false
    let text =
        match r.Status with
        | ChessLibrary.TablebaseLookup.NoTablebaseFolder ->
            $"""{ChessLibrary.TablebaseLookup.statusText path r}: give --syzygy <folder>, set EB_SYZYGY_PATH, or put {{ "syzygyPath": "<folder>" }} in {CliSettings.path ()}"""
        | _ -> ChessLibrary.TablebaseLookup.statusText path r
    if has p "json" then
        let category, dtz =
            match r.Status with
            | ChessLibrary.TablebaseLookup.Answered(c, d) -> ChessLibrary.TablebaseLookup.categoryText c, Option.toNullable d
            | _ -> null, Nullable()
        let result =
            {| Fen = r.Fen
               Answered = answered
               Status = text
               Category = category
               Dtz = dtz
               HasDtz = r.HasDtz
               Uncertain = r.Uncertain
               ClockNote = ChessLibrary.TablebaseLookup.clockNote r |> Option.toObj
               WdlNote = ChessLibrary.TablebaseLookup.wdlOnlyNote r |> Option.toObj
               Moves =
                 r.Moves
                 |> Array.map (fun m ->
                     {| Uci = m.Uci
                        San = m.San
                        Category = ChessLibrary.TablebaseLookup.categoryText m.Category
                        Dtz = Option.toNullable m.Dtz
                        IsZeroing = m.IsZeroing
                        IsCapture = m.IsCapture
                        GivesCheck = m.GivesCheck
                        IsCheckmate = m.IsCheckmate
                        IsStalemate = m.IsStalemate
                        Uncertain = m.Uncertain |}) |}
        let errors = if answered then [] else [ { Code = (if usage then "usage" else "no-answer"); Message = text } ]
        printJson (envelope (name p.Command) answered (box result) errors)
    else
        printfn "%s" r.Fen
        if answered then printfn "%s" text else error text
        let mutable group = None
        for m in r.Moves do
            if group <> Some m.Category then
                group <- Some m.Category
                printfn ""
                printfn "%s" (ChessLibrary.TablebaseLookup.categoryText m.Category)
            printfn "  %-8s %s" m.San (ChessLibrary.TablebaseLookup.moveNote m)
        for note in [ ChessLibrary.TablebaseLookup.clockNote r; ChessLibrary.TablebaseLookup.wdlOnlyNote r ] do
            note |> Option.iter (fun n -> printfn ""; printfn "%s" n)
        if answered && r.Moves.Length > 0 then
            printfn ""
            printfn "%s" (if ignoreClock then "The halfmove clock is read as 0 (--ignore-clock); DTZ is counted from this position" else "The halfmove clock counts; DTZ is counted from this position")
    if answered then ExitOk elif usage then ExitUsage else ExitFailed

let entries: Entry list =
    [ { Spec = analyzeSpec; Run = analyze }
      { Spec = compareSpec; Run = compare }
      { Spec = positionSpec; Run = position }
      { Spec = tablebaseSpec; Run = tablebase } ]
