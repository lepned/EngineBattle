namespace ChessLibrary.Match

open System
open System.Globalization

/// Numbers as the reference prints them: its `fmt::format("{:.2f}", x)` and friends, so a line from
/// EngineBattle's match mode reads character for character like the reference's.
///
/// .NET's "F" formats the exact binary value and rounds an exact tie half to even, as fmt does
/// (both print 12.125 as "12.12", 0.375 as "0.38"; checked against the reference's bundled fmt). They
/// differ only on non-finite values, which fmt writes as C does: "inf", "-inf", and "nan" or
/// "-nan" by the sign bit. Every NaN here prints "-nan", which is what the reference's x86 builds
/// print in every case measured (0.0/0.0 has the sign bit set on x86, and its release build's
/// Elo error comes out negative too). Following the sign bit instead would make the text depend
/// on the machine: .NET on Linux gets a positive NaN from glibc's log10 where Windows gets a
/// negative one, and ARM's 0.0/0.0 is positive.
module MatchFormat =

  let private inv = CultureInfo.InvariantCulture

  let private nonFinite (x: float) =
    if Double.IsNaN x then Some "-nan"
    elif Double.IsPositiveInfinity x then Some "inf"
    elif Double.IsNegativeInfinity x then Some "-inf"
    else None

  /// `{:.Nf}`
  let fixedPoint (decimals: int) (x: float) =
    match nonFinite x with
    | Some s -> s
    | None -> x.ToString("F" + string decimals, inv)

  let private sciExponent (e: int) = (if e < 0 then "e-" else "e+") + (abs e).ToString("00", inv)

  /// `{}` of a double: the shortest digits that read back as the same value, fixed notation for
  /// decimal exponents -4 .. 15 (as fmt), else "1.5e+16".
  let shortest (x: float) =
    match nonFinite x with
    | Some s -> s
    | None ->
      if x = 0.0 then (if Double.IsNegative x then "-0" else "0")
      else
        let r = x.ToString("R", inv)                    // shortest round-trip digits
        let neg = r.StartsWith "-"
        let r = if neg then r.Substring 1 else r
        // split into digits and a decimal exponent: value = 0.d1d2d3... * 10^point
        let mant, exp10 =
          match r.IndexOf 'E' with
          | -1 -> r, 0
          | i -> r.Substring(0, i), int (r.Substring(i + 1))
        let intPart, frac = match mant.IndexOf '.' with | -1 -> mant, "" | i -> mant.Substring(0, i), mant.Substring(i + 1)
        let digits0 = intPart + frac
        let lead = digits0.Length - digits0.TrimStart('0').Length
        let digits = digits0.Trim('0')
        let point = intPart.Length + exp10 - lead        // digits.[0] sits at 10^(point-1)
        let e = point - 1
        let body =
          if e < -4 || e >= 16 then
            let m = if digits.Length > 1 then digits.Substring(0, 1) + "." + digits.Substring 1 else digits
            m + sciExponent e
          elif point <= 0 then "0." + String('0', -point) + digits
          elif point >= digits.Length then digits + String('0', point - digits.Length)
          else digits.Substring(0, point) + "." + digits.Substring point
        (if neg then "-" else "") + body

  /// `{:.2g}` (C's %.2g): two significant digits, trailing zeros dropped, scientific when the
  /// exponent is below -4 or at least 2 ("1e+02").
  let general2 (x: float) =
    match nonFinite x with
    | Some s -> s
    | None ->
      if x = 0.0 then (if Double.IsNegative x then "-0" else "0")
      else
        let e = x.ToString("E1", inv)                   // d.dE+xxx, rounded to 2 significant digits
        let exp10 = int (e.Substring(e.IndexOf 'E' + 1))
        let trim (s: string) = if s.Contains '.' then s.TrimEnd('0').TrimEnd('.') else s
        if exp10 < -4 || exp10 >= 2 then
          trim (e.Substring(0, e.IndexOf 'E')) + sciExponent exp10
        else
          trim (x.ToString("F" + string (1 - exp10), inv))
