module ChessLibrary.PGNComment

open System

/// PGN comment commands — the `[%key value]` sequences that live inside `{ }` comments.
///
/// Not part of the PGN standard itself (which treats comment bodies as free text), but a
/// widely followed convention described in the PGN Specification Supplement: `%clk`,
/// `%emt` and `%egt` for clocks, `%eval` from lichess, and `%csl`/`%cal` for coloured
/// squares and arrows. Because they sit inside ordinary comments any conformant reader
/// still parses the file — it just shows them as text, which is exactly what EB used to
/// do and what this module exists to stop.
///
/// Parsing is non-destructive: callers keep the original comment (so a game still writes
/// back unchanged) and use these views only for display.

/// A key followed by "=", as every engine telemetry dialect writes its fields — short and
/// lowercase in a move annotation ("wv=", "mt=", "tl="), long and mixed-case in EB's
/// tournament header ("Rounds=", "Threads=").
///
/// What must never match is chess notation ending in "=": a promotion, quiet or capturing
/// ("e8=Q", "exd8=Q"), or the Informant equality symbol on any move ("Rc8="). All of those
/// end in a rank digit, and no telemetry key does, so requiring the character before "=" to
/// be a letter separates them. A key that happens to end in a digit is the acceptable loss
/// ("UCI_Chess960="): the threshold is three keys, and a header carrying it carries dozens.
let private telemetryKeyRegex =
  Text.RegularExpressions.Regex(@"(?<![A-Za-z0-9])[A-Za-z](?:[A-Za-z0-9_]*[A-Za-z_])?=",
                                Text.RegularExpressions.RegexOptions.Compiled)

/// A leading "eval/depth" pair, as Fritz and Arena write it ("+0.34/12 0:02").
///
/// Either an explicit sign or two decimals is required, because a score out of a number of
/// games has the same shape: "2.5/9 was enough to win the event" is prose, "+0.34/12" and
/// "0.00/29" are not. A written result ("1/2-1/2") has no decimal point at all.
let private evalDepthPrefixRegex =
  Text.RegularExpressions.Regex(@"^\s*(?:[+-]?M\d+|[+-]\d+\.\d+|\d+\.\d{2,})/\d+",
                                Text.RegularExpressions.RegexOptions.Compiled)

/// A mate token as engines write it in comments: "M5", "-M5", "+M3". Anchored so it cannot
/// match the M of an ordinary word.
let private mateTokenRegex =
  Text.RegularExpressions.Regex(@"(?<![A-Za-z0-9])[+-]?M(\d+)", Text.RegularExpressions.RegexOptions.Compiled)

/// A parsed comment: the commands in the order they appeared, plus whatever text was left.
type ParsedComment =
  { Commands: (string * string) list
    Text: string }
  static member Empty = { Commands = []; Text = "" }

/// Splits a comment into its `[%key value]` commands and the remaining free text.
/// Surrounding braces are tolerated. A bracket that does not open a well-formed command
/// (no `%`, or never closed) is left in the text rather than swallowed — malformed input
/// should degrade to "shown as written", never to silently dropped prose.
let parse (comment: string) : ParsedComment =
  if String.IsNullOrWhiteSpace comment then ParsedComment.Empty
  else
    let s = comment.Trim().Trim('{', '}')
    let commands = ResizeArray<string * string>()
    let text = Text.StringBuilder()
    let mutable i = 0
    while i < s.Length do
      // A command starts with "[%"; anything else is text.
      if s.[i] = '[' && i + 1 < s.Length && s.[i + 1] = '%' then
        // The closing bracket must be the first one: searching past an intervening '[' would
        // let an unterminated command swallow the prose up to some unrelated bracket, e.g.
        // "[%eval 0.3 forgot bracket [see diagram] here" losing everything but "here".
        let close = s.IndexOfAny([| ']'; '[' |], i + 2)
        let body = if close < 0 then "" else s.Substring(i + 2, close - i - 2).Trim()
        let sep = if body = "" then -1 else body.IndexOfAny([| ' '; '\t' |])
        let key = if sep < 0 then body else body.Substring(0, sep)
        if close < 0 || s.[close] = '[' || key = "" then
          // Not a well-formed command — "[" is ordinary text, and the scan resumes after it
          // so a real command later in the comment is still found.
          text.Append(s.[i]) |> ignore
          i <- i + 1
        else
          let value = if sep < 0 then "" else body.Substring(sep + 1).Trim()
          commands.Add(key, value)
          i <- close + 1
      else
        text.Append(s.[i]) |> ignore
        i <- i + 1
    // Collapse the whitespace the removed commands left behind.
    let cleaned =
      text.ToString().Split([| ' '; '\t'; '\n'; '\r' |], StringSplitOptions.RemoveEmptyEntries)
      |> String.concat " "
    { Commands = List.ofSeq commands; Text = cleaned }

/// The comment with its commands removed — what a move list should show. Empty when the
/// comment carried nothing but commands.
let displayText (comment: string) : string = (parse comment).Text

/// The prose of a comment: its display text, unless that text is engine telemetry rather
/// than something written for a reader. Engines have written per-move data into ordinary
/// comments since long before the `[%…]` convention, in several dialects — EB's own
/// `wv=1.06, mt=12875, …`, cutechess and CCC's `+0.54/32 35.500s, tl=148.500s`, Banksia,
/// Ceres — and none of it belongs in a move list as an annotation.
///
/// The test is to hand the text to the engine-annotation parser and see whether it
/// recognises anything. That is sharper than guessing from punctuation, and it does not
/// fire on prose: lichess comments carrying evals, arrows and move numbers all come back
/// empty (`PGNCommentTests`).
let proseText (comment: string) : string =
  let text = displayText comment
  if String.IsNullOrWhiteSpace text then ""
  else
    let stat = EngineTypes.Annotation.getEngineStatData "" false text
    let isEngineAnnotation =
      stat.wv <> 0.0 || stat.d <> 0 || stat.n <> 0L || stat.mt <> 0L || stat.tl <> 0L
    // Backstops for machine data the parser reports nothing for — a comment whose values
    // all happen to be zero, or a dialect it does not know.
    //
    // Each has to be narrow enough not to catch prose. Telemetry keys are short and purely
    // alphabetic ("wv=", "mt=", "tl="), which a promotion never is, so counting `\w+=` would
    // have suppressed "34.e8=Q wins, since 34...f1=Q 35.Qxf1 g1=Q loses". EB's opening-book
    // marker is anchored on its comma, so it does not swallow "Book line ends here…". And a
    // leading eval/depth pair (Fritz and Arena write "+0.34/12") requires the decimal point,
    // so a bare result like "1/2-1/2 was agreed" survives.
    let telemetryKeys = telemetryKeyRegex.Matches(text).Count
    let isBookNote = text.StartsWith("book,", StringComparison.OrdinalIgnoreCase)
    let isEvalDepth = evalDepthPrefixRegex.IsMatch text
    if isEngineAnnotation || telemetryKeys >= 3 || isBookNote || isEvalDepth then "" else text

/// Formats a pawn score for a move list: one decimal, with an explicit sign except at zero.
///
/// The third format section is load-bearing. With only "+0.0;-0.0", .NET applies the
/// negative section to a value like -0.04 and then emits the sign of negative zero on top
/// of it, giving "-+0.0" — which every lichess export triggers, since near-zero evals are
/// everywhere. The zero section also spares a dead-equal position a misleading "+0.0".
let formatPawns (v: float) =
  v.ToString("+0.0;-0.0;0.0", Globalization.CultureInfo.InvariantCulture)

/// Formats a `[%eval …]` value the way a move list shows engine evals: pawn scores to one
/// decimal with an explicit sign, mate scores verbatim (`#4`, `#-3`). Lichess writes the
/// value from White's point of view, which is also EB's `wv` convention, so the two mix
/// without a sign flip.
let formatEval (raw: string) : string option =
  if String.IsNullOrWhiteSpace raw then None
  else
    let v = raw.Trim()
    if v.StartsWith "#" then Some v
    else
      match Double.TryParse(v, Globalization.NumberStyles.Float, Globalization.CultureInfo.InvariantCulture) with
      | true, d -> Some(formatPawns d)
      | _ -> None

/// Every value written for a command key, in order (case-insensitive).
///
/// A key can legitimately appear more than once: consecutive comments on one move are
/// appended into a single string, so a move annotated twice arrives as
/// "[%csl Gd4] [%csl Re5][%cal Ge2e4]". Reading only the first would drop Re5 silently.
let tryFindAll (key: string) (parsed: ParsedComment) : string list =
  parsed.Commands
  |> List.filter (fun (k, _) -> String.Equals(k, key, StringComparison.OrdinalIgnoreCase))
  |> List.map snd

/// The first value for a command key (case-insensitive), if present.
let tryFind (key: string) (parsed: ParsedComment) : string option =
  parsed.Commands
  |> List.tryFind (fun (k, _) -> String.Equals(k, key, StringComparison.OrdinalIgnoreCase))
  |> Option.map snd

/// A move-list tooltip: the comment's text, followed by its clock commands.
///
/// The display text deliberately drops every `[%…]` command, but the clocks are the one
/// group that carries information a reader wants and that has nowhere else to appear —
/// `%eval` has the move list's own column. `%clk` is the time left after the move, `%emt`
/// and `%egt` the time the move and the game had taken.
let tooltipText (comment: string) : string =
  let parsed = parse comment
  let clocks =
    [ "clk", "clock"; "emt", "moved in"; "egt", "elapsed" ]
    |> List.choose (fun (key, label) ->
         tryFind key parsed |> Option.map (fun v -> sprintf "%s %s" label v))
  match parsed.Text, clocks with
  | "", [] -> ""
  | "", cs -> String.Join(", ", cs)
  | text, [] -> text
  | text, cs -> text + " — " + String.Join(", ", cs)

/// True when the comment carries at least one command.
let hasCommands (comment: string) : bool = not (parse comment).Commands.IsEmpty

/// The mate distance written in an engine annotation ("wv=M5", "+M3/32 12.5s"), if any.
///
/// The annotation parser collapses every mate to a ±200.0 sentinel and loses the distance,
/// so a move list that formats `wv` numerically shows "+200.0" where it means mate in five.
/// Recovering the number costs one regex over the original comment.
let tryMateDistance (comment: string) : int option =
  if String.IsNullOrWhiteSpace comment then None
  else
    let m = mateTokenRegex.Match comment
    if m.Success then Some(int m.Groups.[1].Value) else None

/// A shape a comment asks the board to draw. `To` is empty for a highlighted square.
///
/// `Color` is the single letter the convention uses — G, R, Y or B — left as written rather
/// than resolved to a value here, because what those letters should look like depends on the
/// board theme, which is a rendering concern.
type CommentShape =
  { Color: char
    From: string
    To: string }
  // Null counts as "no destination": a shape built outside this module may leave it unset,
  // and calling that an arrow would emit a `[%cal]` with only one square in it.
  member x.IsArrow = not (String.IsNullOrEmpty x.To)

let private isSquare (s: string) =
  s.Length = 2 && s.[0] >= 'a' && s.[0] <= 'h' && s.[1] >= '1' && s.[1] <= '8'

/// The shapes drawn by a comment's `[%csl …]` (highlighted squares) and `[%cal …]` (arrows)
/// commands, in the order written — squares first, as lichess emits them.
///
/// Both take a comma-separated list of a colour letter followed by one square ("Gd4") or two
/// ("Ge2e4"). A token that is malformed is skipped on its own; the rest of the list still
/// draws, since half an annotation is better than none and these are hand-authored.
let shapes (comment: string) : CommentShape list =
  let parsed = parse comment
  let read (key: string) (expected: int) =
    tryFindAll key parsed
    |> List.collect (fun value ->
      value.Split(',')
      |> Array.toList
      |> List.choose (fun token ->
           let t = token.Trim()
           if t.Length <> expected + 1 then None
           else
             let color = Char.ToUpperInvariant t.[0]
             if color <> 'G' && color <> 'R' && color <> 'Y' && color <> 'B' then None
             else
               let squares = t.Substring(1).ToLowerInvariant()
               let a = squares.Substring(0, 2)
               let b = if expected = 4 then squares.Substring(2, 2) else ""
               if isSquare a && (b = "" || isSquare b) then Some { Color = color; From = a; To = b }
               else None))
  read "csl" 2 @ read "cal" 4

/// Writes shapes back as `[%csl …][%cal …]`, in lichess's own emission style: squares
/// before arrows, the two commands adjacent with no space between them, and neither
/// command emitted when it would be empty. Returns "" when there is nothing to draw.
///
/// Squares are validated on the way out as well as in — a shape carrying a square name that
/// is not on the board is dropped rather than written, since a file we emit is a file some
/// other tool has to read.
let formatShapes (shapes: CommentShape seq) : string =
  let square (s: string) = if isNull s then "" else s.Trim().ToLowerInvariant()
  let valid =
    if isNull (box shapes) then []
    else
      shapes
      |> Seq.filter (fun s ->
           let color = Char.ToUpperInvariant s.Color
           (color = 'G' || color = 'R' || color = 'Y' || color = 'B')
           && isSquare (square s.From)
           && (square s.To = "" || isSquare (square s.To)))
      |> Seq.toList
  let write (token: CommentShape) =
    sprintf "%c%s%s" (Char.ToUpperInvariant token.Color) (square token.From) (square token.To)
  let command name items =
    match items with
    | [] -> ""
    | _ -> sprintf "[%%%s %s]" name (items |> List.map write |> String.concat ",")
  command "csl" (valid |> List.filter (fun s -> not s.IsArrow))
  + command "cal" (valid |> List.filter (fun s -> s.IsArrow))

/// A complete comment section for a move: the prose and the shapes, each in its own `{ }`
/// block, which is how lichess writes them and therefore what round-trips cleanly. Either
/// half is omitted when empty; both empty gives "".
let formatComment (prose: string) (shapes: CommentShape seq) : string =
  let commands = formatShapes shapes
  // Braces would end the comment early and spill the rest into the movetext, where a reader
  // would try to parse it as moves.
  let prose = if isNull prose then "" else prose.Replace("{", "(").Replace("}", ")")
  let hasProse = not (String.IsNullOrWhiteSpace prose)
  match hasProse, commands with
  | false, "" -> ""
  | true, "" -> sprintf "{ %s }" (prose.Trim())
  | false, c -> sprintf "{ %s }" c
  | true, c -> sprintf "{ %s } { %s }" (prose.Trim()) c
