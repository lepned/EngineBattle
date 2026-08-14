module ChessLibrary.MaterialRules

open PositionTypes

/// The single material-draw rule for the whole codebase: the FIDE "dead position"
/// test — checkmate is impossible by ANY legal sequence of moves, cooperative or not.
/// Verified equal to python-chess `is_insufficient_material()` across the standard
/// endings (see the table test in TestProject/PositionQueryTests.fs).
///
/// Dead: bare kings; king + a single minor; kings + any number of bishops that all
/// stand on one square color. NOT dead (mate remains possible with help, even though
/// neither side can force it): K+N vs K+N, K+B vs K+N, opposite-color bishops,
/// K+N+N vs K — tournament adjudication used to call these draws, but they are
/// caught by the eval-based draw rule and the 50-move backstop instead.
///
/// Operates on a QBB-frame position: the vertical frame flip inverts square colors
/// but preserves the all-bishops-on-one-color property, so the parity test is safe
/// regardless of side to move.
let isDeadPosition (pos: Position inref) =
  let heavy = PositionOps.pawns &pos ||| PositionOps.rooks &pos ||| PositionOps.queens &pos
  if heavy <> 0UL then false
  else
    let knights = PositionOps.knights &pos
    let bishops = PositionOps.bishops &pos
    let minors =
      System.Numerics.BitOperations.PopCount knights + System.Numerics.BitOperations.PopCount bishops
    if minors <= 1 then true
    elif knights <> 0UL then false
    else
      let dark = 0xAA55AA55AA55AA55UL
      bishops &&& dark = 0UL || bishops &&& ~~~dark = 0UL
