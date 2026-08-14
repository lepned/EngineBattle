namespace ChessLibrary

/// Self-contained SVG board renderer for diagnostics and tooling (console `query --svg`).
/// Draws the position with the embedded Cburnett pieces (SvgPieceDefs) inside a dark
/// frame with outside coordinates, plus overlay arrows/discs in absolute square names.
/// The insight shape mapping (colors, arrow-vs-line, opacities, muted side-to-move
/// hanging) MIRRORS WebGUI/Components/Layout/ChessboardLayout/InsightShapes.cs — keep
/// the two in sync when either changes. All numbers are written culture-invariant.
module BoardSvg =
  open System
  open System.Text
  open System.Globalization

  /// Arrow between square centers; Head = "arrow" (with a triangular head) or "line".
  type SvgArrow =
    { From: string
      To: string
      Color: string
      Opacity: float
      Width: float
      Head: string }

  /// Disc on a square; Size = diameter as a fraction of the square.
  type SvgCircle =
    { Square: string
      Color: string
      Opacity: float
      Size: float }

  let private arrow (fromSq: string) (toSq: string) color opacity =
    { From = fromSq; To = toSq; Color = color; Opacity = opacity; Width = 1.0; Head = "arrow" }

  let private line (fromSq: string) (toSq: string) color opacity =
    { From = fromSq; To = toSq; Color = color; Opacity = opacity; Width = 0.8; Head = "line" }

  let private disc (square: string) color opacity size =
    { Square = square; Color = color; Opacity = opacity; Size = size }

  // Colors mirrored from InsightShapes.cs.
  let private pinColor = "#E67E22"
  let private checkColor = "#D63031"
  let private escapeColor = "#27AE60"
  let private hangingColor = "#F1C40F"
  let private forkColor = "#9B59B6"
  let private skewerColor = "#00A8CC"
  let private overloadColor = "#D63C8C"
  let private discoveredColor = "#16A085"
  let private removableColor = "#C0692B"

  let private bothSides (ins: BoardUtils.PositionInsights) = [ ins.White; ins.Black ]

  /// Overlay arrows for the insight layers — mirrors InsightShapes.Arrows.
  let insightArrows (ins: BoardUtils.PositionInsights) (pinsChecks: bool) (hanging: bool) (tactics: bool) : SvgArrow list =
    [ for side in bothSides ins do
        if side.King <> "" then
          if pinsChecks then
            for pin in side.Pins do
              yield arrow pin.Attacker pin.Pinned pinColor 0.75
              yield line pin.Pinned pin.King pinColor 0.3
            for checker in side.Checkers do
              yield arrow checker side.King checkColor 0.6
          if hanging then
            // Mover's own hanging pieces are warnings (muted); the opponent's are
            // capturable right now (full strength) — same nuance as the GUI.
            let mutedOpacity = if side.IsSideToMove then 0.28 else 0.5
            for h in side.HangingPieces do
              for attacker in h.Attackers do
                yield line attacker h.Square hangingColor mutedOpacity
          if tactics then
            for f in side.Forks do
              for target in f.Targets do
                yield arrow f.Forker target forkColor 0.6
            for s in side.Skewers do
              yield arrow s.Attacker s.Front skewerColor 0.65
              yield line s.Front s.Back skewerColor 0.3
            for o in side.OverloadedDefenders do
              for defended in o.Defends do
                yield line o.Defender defended overloadColor 0.45
            for d in side.DiscoveredAttacks do
              yield line d.Slider d.Target discoveredColor (if d.IsCheck then 0.65 else 0.4)
            for r in side.RemovableDefenders do
              for defended in r.Defends do
                yield line r.Defender defended removableColor 0.4 ]

  /// Overlay discs for the insight layers — mirrors InsightShapes.Circles.
  let insightCircles (ins: BoardUtils.PositionInsights) (pinsChecks: bool) (dangerZone: bool) (hanging: bool) (tactics: bool) : SvgCircle list =
    [ for side in bothSides ins do
        if side.King <> "" then
          if pinsChecks then
            for pin in side.Pins do
              yield disc pin.Pinned pinColor 0.35 0.95
            for checker in side.Checkers do
              yield disc checker checkColor 0.45 0.9
            for block in side.CheckBlockSquares do
              yield disc block checkColor 0.18 0.45
            if side.InCheck then
              yield disc side.King checkColor 0.35 0.95
          if dangerZone then
            for danger in side.KingDangerSquares do
              yield disc danger checkColor 0.25 0.95
            for escape in side.KingEscapeSquares do
              yield disc escape escapeColor 0.5 0.4
          if hanging then
            let discOpacity = if side.IsSideToMove then 0.18 else 0.35
            for h in side.HangingPieces do
              yield disc h.Square hangingColor discOpacity 0.95
          if tactics then
            for f in side.Forks do
              yield disc f.Forker forkColor 0.35 0.95
            for o in side.OverloadedDefenders do
              yield disc o.Defender overloadColor 0.35 0.95
            for d in side.DiscoveredAttacks do
              yield disc d.Blocker discoveredColor 0.35 0.95
            for r in side.RemovableDefenders do
              yield disc r.Defender removableColor 0.35 0.95 ]

  // ── Rendering ────────────────────────────────────────────────────────────
  // Geometry: 45px squares (the pieces' native Cburnett size — no scaling), a dark
  // frame margin hosting the coordinates (the GUI's outside-coordinates look), and
  // arrow/head proportions matching EbChessboard's overlay (0.17/0.33/0.45 of a
  // square; the "line" head stops 0.18 short of the destination center).

  let private squarePx = 45.0
  let private marginPx = 20.0
  let private lightSquare = "#F0D9B5"
  let private darkSquare = "#B58863"
  let private frameColor = "#3B3733"

  let private inv (v: float) = v.ToString("0.###", CultureInfo.InvariantCulture)

  let private xmlEscape (s: string) =
    s.Replace("&", "&amp;").Replace("<", "&lt;").Replace(">", "&gt;").Replace("\"", "&quot;")

  /// Center of a square in pixel coordinates, honoring orientation.
  let private center (flipped: bool) (sq: string) =
    let file = int sq.[0] - int 'a'
    let rank = int sq.[1] - int '1'
    let col = if flipped then 7 - file else file
    let row = if flipped then rank else 7 - rank
    (marginPx + (float col + 0.5) * squarePx, marginPx + (float row + 0.5) * squarePx)

  /// Renders the position with the given overlay to a self-contained SVG document.
  /// Throws ArgumentException on an invalid FEN (gate on validateFen upstream for
  /// structured errors). The caption (typically the FEN) is drawn under the board.
  let render (fen: string) (flipped: bool) (arrows: SvgArrow seq) (circles: SvgCircle seq) (caption: string option) : string =
    let v = BoardUtils.validateFen fen
    if not v.IsValid then invalidArg "fen" (String.concat "; " v.Errors)
    let board = BoardUtils.boardOfFen fen
    let side = marginPx * 2.0 + squarePx * 8.0
    let captionH = match caption with Some _ -> 18.0 | None -> 0.0
    let sb = StringBuilder()
    let app (s: string) = sb.Append(s) |> ignore

    app (sprintf "<svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" viewBox=\"0 0 %s %s\" width=\"%s\" height=\"%s\">"
           (inv side) (inv (side + captionH)) (inv side) (inv (side + captionH)))

    // Frame and squares
    app (sprintf "<rect width=\"%s\" height=\"%s\" fill=\"%s\"/>" (inv side) (inv (side + captionH)) frameColor)
    for row in 0 .. 7 do
      for col in 0 .. 7 do
        let file = if flipped then 7 - col else col
        let rank = if flipped then row else 7 - row
        let isLight = (file + rank) % 2 = 1
        app (sprintf "<rect x=\"%s\" y=\"%s\" width=\"45\" height=\"45\" fill=\"%s\"/>"
               (inv (marginPx + float col * squarePx)) (inv (marginPx + float row * squarePx))
               (if isLight then lightSquare else darkSquare))

    // Outside coordinates in the frame (left ranks, bottom files)
    for i in 0 .. 7 do
      let rankLabel = if flipped then i + 1 else 8 - i
      let fileLabel = char (int 'a' + (if flipped then 7 - i else i))
      app (sprintf "<text x=\"%s\" y=\"%s\" font-family=\"sans-serif\" font-size=\"10\" font-weight=\"600\" fill=\"#FFFFFF\" fill-opacity=\"0.75\" text-anchor=\"middle\" dominant-baseline=\"central\">%d</text>"
             (inv (marginPx / 2.0)) (inv (marginPx + (float i + 0.5) * squarePx)) rankLabel)
      app (sprintf "<text x=\"%s\" y=\"%s\" font-family=\"sans-serif\" font-size=\"10\" font-weight=\"600\" fill=\"#FFFFFF\" fill-opacity=\"0.75\" text-anchor=\"middle\" dominant-baseline=\"central\">%c</text>"
             (inv (marginPx + (float i + 0.5) * squarePx)) (inv (marginPx + 8.0 * squarePx + marginPx / 2.0)) fileLabel)

    // Piece defs (only the pieces actually on the board) and their uses
    let present = board |> Array.filter (fun c -> c <> '\000') |> Array.distinct
    if present.Length > 0 then
      app "<defs>"
      for c in present do
        match SvgPieceDefs.defs.TryFind c with
        | Some def ->
            // Unique ids per color+piece: 'K' -> wK, 'k' -> bK (matches the def markup).
            app def
        | None -> ()
      app "</defs>"
      for idx in 0 .. 63 do
        let c = board.[idx]
        if c <> '\000' && SvgPieceDefs.defs.ContainsKey c then
          let file = idx % 8
          let rank = idx / 8
          let col = if flipped then 7 - file else file
          let row = if flipped then rank else 7 - rank
          let defId = (if Char.IsUpper c then "w" else "b") + string (Char.ToUpperInvariant c)
          app (sprintf "<use xlink:href=\"#%s\" x=\"%s\" y=\"%s\"/>"
                 defId (inv (marginPx + float col * squarePx)) (inv (marginPx + float row * squarePx)))

    // Overlay: discs then arrows, above the pieces (same stacking as the GUI overlay)
    for cir in circles do
      let (cx, cy) = center flipped cir.Square
      app (sprintf "<circle cx=\"%s\" cy=\"%s\" r=\"%s\" fill=\"%s\" opacity=\"%s\"/>"
             (inv cx) (inv cy) (inv (cir.Size / 2.0 * squarePx)) cir.Color (inv cir.Opacity))
    for a in arrows do
      if a.From <> a.To && a.From <> "" && a.To <> "" then
        let (x1, y1) = center flipped a.From
        let (x2, y2) = center flipped a.To
        let dx, dy = x2 - x1, y2 - y1
        let len = sqrt (dx * dx + dy * dy)
        let ux, uy = dx / len, dy / len
        let strokeW = 0.17 * a.Width * squarePx
        if a.Head = "line" then
          let ex, ey = x2 - ux * 0.18 * squarePx, y2 - uy * 0.18 * squarePx
          app (sprintf "<line x1=\"%s\" y1=\"%s\" x2=\"%s\" y2=\"%s\" stroke=\"%s\" stroke-width=\"%s\" stroke-linecap=\"round\" opacity=\"%s\"/>"
                 (inv x1) (inv y1) (inv ex) (inv ey) a.Color (inv strokeW) (inv a.Opacity))
        else
          let headLen = 0.33 * squarePx
          let headW = 0.45 * squarePx
          let bx, by = x2 - ux * headLen, y2 - uy * headLen
          let px, py = -uy, ux
          app (sprintf "<g opacity=\"%s\"><line x1=\"%s\" y1=\"%s\" x2=\"%s\" y2=\"%s\" stroke=\"%s\" stroke-width=\"%s\" stroke-linecap=\"round\"/><polygon points=\"%s,%s %s,%s %s,%s\" fill=\"%s\"/></g>"
                 (inv a.Opacity) (inv x1) (inv y1) (inv bx) (inv by) a.Color (inv strokeW)
                 (inv x2) (inv y2)
                 (inv (bx + px * headW / 2.0)) (inv (by + py * headW / 2.0))
                 (inv (bx - px * headW / 2.0)) (inv (by - py * headW / 2.0))
                 a.Color)

    match caption with
    | Some text ->
        app (sprintf "<text x=\"%s\" y=\"%s\" font-family=\"monospace\" font-size=\"9\" fill=\"#FFFFFF\" fill-opacity=\"0.7\" text-anchor=\"middle\">%s</text>"
               (inv (side / 2.0)) (inv (side + captionH - 6.0)) (xmlEscape text))
    | None -> ()

    app "</svg>"
    sb.ToString()

  /// Convenience for diagnostics: the position with the full tactics-family overlay
  /// (pins/checks + hanging + tactics — the GUI's "Tactics" bundle; king danger is
  /// visually dominant and stays opt-out) and the FEN as caption.
  let renderWithInsights (fen: string) : string =
    let fen = BoardUtils.normalizeFen fen
    let ins = BoardUtils.getPositionInsights fen
    render fen false (insightArrows ins true true true) (insightCircles ins true false true true) (Some fen)
