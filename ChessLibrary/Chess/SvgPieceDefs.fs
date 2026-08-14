namespace ChessLibrary

/// Cburnett chess piece vector art (wikipedia set, CC BY-SA — see
/// WebGUI/wwwroot/pieces/wikipedia/LICENSE.md), embedded as 45x45 SVG groups for
/// the self-contained board renderer in BoardSvg. GENERATED from those files —
/// regenerate rather than hand-edit if the piece set changes.
module SvgPieceDefs =

  let private wK = """<g id="wK" fill="none" fill-rule="evenodd" stroke="#000"><g stroke-linecap="round" stroke-width="1.5"><path d="M22.5 11.63V6"/><path d="M20 8h5"/></g><path d="M22.5 25s4.5-7.5 3-10.5c0 0-1-2.5-3-2.5s-3 2.5-3 2.5c-1.5 3 3 10.5 3 10.5" stroke-width="1.5" fill="#fff"/><g stroke-width="1.5" stroke-linecap="round" stroke-linejoin="round"><path d="M12.5 37c5.5 3.5 14.5 3.5 20 0v-7s9-4.5 6-10.5c-4-6.5-13.5-3.5-16 4V27v-3.5c-2.5-7.5-12-10.5-16-4-3 6 6 10.5 6 10.5v7" fill="#fff"/><path d="M12.5 30c5.5-3 14.5-3 20 0m-20 3.5c5.5-3 14.5-3 20 0m-20 3.5c5.5-3 14.5-3 20 0"/></g></g>"""

  let private wQ = """<g id="wQ" fill="#fff" stroke="#000" stroke-linejoin="round" stroke-width="1.5"><path d="M9 26c8.5-1.5 21-1.5 27 0l2.5-12.5L31 25l-.3-14.1-5.2 13.6-3-14.5-3 14.5-5.2-13.6L14 25 6.5 13.5 9 26z"/><path d="M9 26c0 2 1.5 2 2.5 4 1 1.5 1 1 .5 3.5-1.5 1-1 2.5-1 2.5-1.5 1.5 0 2.5 0 2.5 6.5 1 16.5 1 23 0 0 0 1.5-1 0-2.5 0 0 .5-1.5-1-2.5-.5-2.5-.5-2 .5-3.5 1-2 2.5-2 2.5-4-8.5-1.5-18.5-1.5-27 0z"/><path d="M11.5 30c3.5-1 18.5-1 22 0M12 33.5c6-1 15-1 21 0" fill="none"/><circle cx="6" cy="12" r="2"/><circle cx="14" cy="9" r="2"/><circle cx="22.5" cy="8" r="2"/><circle cx="31" cy="9" r="2"/><circle cx="39" cy="12" r="2"/></g>"""

  let private wR = """<g id="wR" fill="#fff" fill-rule="evenodd" stroke="#000"><g stroke-linejoin="round" stroke-width="1.5"><path d="M9 39h27v-3H9v3z"/><path d="M12 36v-4h21v4H12z"/><path d="M11 14V9h4v2h5V9h5v2h5V9h4v5" stroke-linecap="butt"/><path d="M34 14l-3 3H14l-3-3" stroke-linecap="round"/></g><path d="M31 17v12.5H14V17" stroke-linecap="butt" stroke-linejoin="miter" stroke-width="1.5"/><g stroke-linecap="round"><path d="M31 29.5l1.5 2.5h-20l1.5-2.5" stroke-linejoin="round" stroke-width="1.5"/><path d="M11 14h23" fill="none" stroke-linejoin="miter" stroke-width="1.5"/></g></g>"""

  let private wB = """<g id="wB" fill="none" fill-rule="evenodd" stroke="#000"><g fill="#fff" stroke-linejoin="round" stroke-width="1.5"><path d="M9 36c3.39-.97 10.11.43 13.5-2 3.39 2.43 10.11 1.03 13.5 2 0 0 1.65.54 3 2-.68.97-1.65.99-3 .5-3.39-.97-10.11.46-13.5-1-3.39 1.46-10.11.03-13.5 1-1.35.49-2.32.47-3-.5 1.35-1.46 3-2 3-2zm6-4c2.5 2.5 12.5 2.5 15 0 .5-1.5 0-2 0-2 0-2.5-2.5-4-2.5-4 5.5-1.5 6-11.5-5-15.5-11 4-10.5 14-5 15.5 0 0-2.5 1.5-2.5 4 0 0-.5.5 0 2z"/><path d="M25 8a2.5 2.5 0 1 1-5 0 2.5 2.5 0 1 1 5 0z"/></g><path d="M17.5 26h10M15 30h15m-7.5-14.5v5M20 18h5" stroke-linecap="round" stroke-width="1.5"/></g>"""

  let private wN = """<g id="wN" fill-rule="evenodd" stroke="#000"><g fill="#fff" stroke-linecap="round" stroke-linejoin="round" stroke-width="1.5"><path d="M22 10c10.5 1 16.5 8 16 29H15c0-9 10-6.5 8-21"/><path d="M24 18c.38 2.91-5.55 7.37-8 9-3 2-2.82 4.34-5 4-1.042-.94 1.41-3.04 0-3-1 0 .19 1.23-1 2-1 0-4.003 1-4-4 0-2 6-12 6-12s1.89-1.9 2-3.5c-.73-.994-.5-2-.5-3 1-1 3 2.5 3 2.5h2s.78-1.992 2.5-3c1 0 1 3 1 3"/></g><path d="M9.5 25.5a.5.5 0 1 1-1 0 .5.5 0 1 1 1 0zm5.433-9.75c-.414.717-.944 1.187-1.183 1.049s-.097-.832.317-1.549.944-1.187 1.183-1.049.097.832-.317 1.549z" stroke-linejoin="round" stroke-width="1.5"/></g>"""

  let private wP = """<g id="wP"><path d="M22.5 9a4 4 0 0 0-4 4c0 .89.29 1.71.78 2.38C17.33 16.5 16 18.59 16 21c0 2.03.94 3.84 2.41 5.03-3 1.06-7.41 5.55-7.41 13.47h23c0-7.92-4.41-12.41-7.41-13.47 1.47-1.19 2.41-3 2.41-5.03 0-2.41-1.33-4.5-3.28-5.62.49-.67.78-1.49.78-2.38a4 4 0 0 0-4-4z" fill="#fff" stroke="#000" stroke-width="1.5"/></g>"""

  let private bK = """<g id="bK" fill="none" fill-rule="evenodd" stroke="#000"><g stroke-linejoin="miter" stroke-width="1.5"><path d="M22.5 11.63V6" stroke-linecap="round"/><path d="M22.5 25s4.5-7.5 3-10.5c0 0-1-2.5-3-2.5s-3 2.5-3 2.5c-1.5 3 3 10.5 3 10.5" stroke-linecap="butt" fill="#000"/></g><g stroke-linecap="round"><path d="M12.5 37c5.5 3.5 14.5 3.5 20 0v-7s9-4.5 6-10.5c-4-6.5-13.5-3.5-16 4V27v-3.5c-2.5-7.5-12-10.5-16-4-3 6 6 10.5 6 10.5v7" fill="#000" stroke-linejoin="round" stroke-width="1.5"/><path d="M20 8h5" stroke-linejoin="miter" stroke-width="1.5"/><path d="M32 29.5s8.5-4 6.03-9.65C34.15 14 25 18 22.5 24.5v2.1-2.1C20 18 10.85 14 6.97 19.85 4.5 25.5 13 29.5 13 29.5m-.5.5c5.5-3 14.5-3 20 0m-20 3.5c5.5-3 14.5-3 20 0m-20 3.5c5.5-3 14.5-3 20 0" stroke-width="1.5" stroke-linejoin="round" stroke="#fff"/></g></g>"""

  let private bQ = """<g id="bQ" stroke="#000"><g stroke-linejoin="round" stroke-width="1.5"><path d="M9 26c8.5-1.5 21-1.5 27 0l2.5-12.5L31 25l-.3-14.1-5.2 13.6-3-14.5-3 14.5-5.2-13.6L14 25 6.5 13.5 9 26z"/><path d="M9 26c0 2 1.5 2 2.5 4 1 1.5 1 1 .5 3.5-1.5 1-1 2.5-1 2.5-1.5 1.5 0 2.5 0 2.5 6.5 1 16.5 1 23 0 0 0 1.5-1 0-2.5 0 0 .5-1.5-1-2.5-.5-2.5-.5-2 .5-3.5 1-2 2.5-2 2.5-4-8.5-1.5-18.5-1.5-27 0z"/><path d="M11.5 30c3.5-1 18.5-1 22 0M12 33.5c6-1 15-1 21 0" stroke-linecap="round"/><circle cx="6" cy="12" r="2"/><circle cx="14" cy="9" r="2"/><circle cx="22.5" cy="8" r="2"/><circle cx="31" cy="9" r="2"/><circle cx="39" cy="12" r="2"/><path d="M11 38.5a35 35 1 0 0 23 0" stroke-linecap="butt" fill="none"/></g><path d="M11 29a35 35 1 0 1 23 0m-21.5 2.5h20m-21 3a35 35 1 0 0 22 0m-23 3a35 35 1 0 0 24 0" fill="none" stroke="#fff" stroke-linecap="round" stroke-linejoin="round" stroke-width="1.5"/></g>"""

  let private bR = """<g id="bR" fill-rule="evenodd" stroke="#000"><g stroke-linejoin="round" stroke-width="1.5"><path d="M9 39h27v-3H9v3zm3.5-7l1.5-2.5h17l1.5 2.5h-20z"/><path d="M12 36v-4h21v4H12z"/></g><path d="M14 29.5v-13h17v13H14z" stroke-linejoin="miter" stroke-width="1.5"/><g stroke-linejoin="round"><path d="M14 16.5L11 14h23l-3 2.5H14z" stroke-width="1.5"/><path d="M11 14V9h4v2h5V9h5v2h5V9h4v5H11z" stroke-width="1.5"/></g><path d="M12 35.5h21m-20-4h19m-18-2h17m-17-13h17M11 14h23" stroke-linejoin="miter" fill="none" stroke="#fff" stroke-width="1" stroke-linecap="round"/></g>"""

  let private bB = """<g id="bB" fill="none" fill-rule="evenodd" stroke="#000"><g fill="#000" stroke-linejoin="round" stroke-width="1.5"><path d="M9 36c3.39-.97 10.11.43 13.5-2 3.39 2.43 10.11 1.03 13.5 2 0 0 1.65.54 3 2-.68.97-1.65.99-3 .5-3.39-.97-10.11.46-13.5-1-3.39 1.46-10.11.03-13.5 1-1.35.49-2.32.47-3-.5 1.35-1.46 3-2 3-2zm6-4c2.5 2.5 12.5 2.5 15 0 .5-1.5 0-2 0-2 0-2.5-2.5-4-2.5-4 5.5-1.5 6-11.5-5-15.5-11 4-10.5 14-5 15.5 0 0-2.5 1.5-2.5 4 0 0-.5.5 0 2z"/><path d="M25 8a2.5 2.5 0 1 1-5 0 2.5 2.5 0 1 1 5 0z"/></g><path d="M17.5 26h10M15 30h15m-7.5-14.5v5M20 18h5" stroke="#fff" stroke-linecap="round" stroke-width="1.5"/></g>"""

  let private bN = """<g id="bN" fill-rule="evenodd" stroke="#000"><g fill="#000" stroke-linecap="round" stroke-linejoin="round" stroke-width="1.5"><path d="M22 10c10.5 1 16.5 8 16 29H15c0-9 10-6.5 8-21"/><path d="M24 18c.38 2.91-5.55 7.37-8 9-3 2-2.82 4.34-5 4-1.042-.94 1.41-3.04 0-3-1 0 .19 1.23-1 2-1 0-4.003 1-4-4 0-2 6-12 6-12s1.89-1.9 2-3.5c-.73-.994-.5-2-.5-3 1-1 3 2.5 3 2.5h2s.78-1.992 2.5-3c1 0 1 3 1 3"/></g><g fill="#fff"><path d="M9.5 25.5a.5.5 0 1 1-1 0 .5.5 0 1 1 1 0zm5.433-9.75c-.414.717-.944 1.187-1.183 1.049s-.097-.832.317-1.549.944-1.187 1.183-1.049.097.832-.317 1.549z" stroke-linejoin="round" stroke-width="1.5" stroke="#fff"/><path d="M24.55 10.4l-.45 1.45.5.15c3.15 1 5.65 2.49 7.9 6.75S35.75 29.06 35.25 39l-.05.5h2.25l.05-.5c.5-10.06-.88-16.85-3.25-21.34s-5.79-6.64-9.19-7.16l-.51-.1z" stroke="none"/></g></g>"""

  let private bP = """<g id="bP"><path d="M22.5 9a4 4 0 0 0-4 4c0 .89.29 1.71.78 2.38C17.33 16.5 16 18.59 16 21c0 2.03.94 3.84 2.41 5.03-3 1.06-7.41 5.55-7.41 13.47h23c0-7.92-4.41-12.41-7.41-13.47 1.47-1.19 2.41-3 2.41-5.03 0-2.41-1.33-4.5-3.28-5.62.49-.67.78-1.49.78-2.38a4 4 0 0 0-4-4z" stroke="#000" stroke-width="1.5"/></g>"""

  /// Piece char ('K'..'p') -> 45x45 <g id=...> definition for an SVG <defs> block.
  let defs : Map<char, string> =
    Map.ofList [
      'K', wK
      'Q', wQ
      'R', wR
      'B', wB
      'N', wN
      'P', wP
      'k', bK
      'q', bQ
      'r', bR
      'b', bB
      'n', bN
      'p', bP
    ]
