namespace ChessLibrary.Match

open System

/// The reference's match statistics (matchmaking/stats.hpp, matchmaking/elo/*.cpp at 60d7a7a), ported
/// operation for operation so the numbers it prints come out the same: W/L/D and pentanomial
/// counts, and the WDL and pentanomial Elo, error, nElo and LOS built from them. Nothing here is
/// guarded against empty counts, as nothing there is: an empty or one-sided result gives the same
/// "nan"/"inf" the reference prints (MatchFormat).
module MatchStats =

  /// W/L/D from the first engine's side, plus pentanomial pair counts (WW = both games won, ...).
  type Stats =
    { Wins: int; Losses: int; Draws: int
      PentaWW: int; PentaWD: int; PentaWL: int; PentaDD: int; PentaLD: int; PentaLL: int }

    static member Empty =
      { Wins = 0; Losses = 0; Draws = 0; PentaWW = 0; PentaWD = 0; PentaWL = 0; PentaDD = 0; PentaLD = 0; PentaLL = 0 }

    /// The reference's Stats(wins, losses, draws) - note the order.
    static member OfWld(wins, losses, draws) = { Stats.Empty with Wins = wins; Losses = losses; Draws = draws }

    static member (+) (a: Stats, b: Stats) =
      { Wins = a.Wins + b.Wins; Losses = a.Losses + b.Losses; Draws = a.Draws + b.Draws
        PentaWW = a.PentaWW + b.PentaWW; PentaWD = a.PentaWD + b.PentaWD; PentaWL = a.PentaWL + b.PentaWL
        PentaDD = a.PentaDD + b.PentaDD; PentaLD = a.PentaLD + b.PentaLD; PentaLL = a.PentaLL + b.PentaLL }

    /// The other engine's view (the reference's operator~): W<->L, WW<->LL, WD<->LD.
    member s.Inverted =
      { s with Wins = s.Losses; Losses = s.Wins; PentaWW = s.PentaLL; PentaLL = s.PentaWW; PentaWD = s.PentaLD; PentaLD = s.PentaWD }

    member s.Sum = s.Wins + s.Losses + s.Draws
    member s.TotalPairs = s.PentaWW + s.PentaWD + s.PentaWL + s.PentaDD + s.PentaLD + s.PentaLL
    member s.Points = float s.Wins + 0.5 * float s.Draws
    member s.WlDdRatio = float s.PentaWL / float s.PentaDD
    member s.DrawRatio = 100.0 * float s.Draws / float s.Sum
    member s.DrawRatioPenta = (float (s.PentaWL + s.PentaDD) / float s.TotalPairs) * 100.0
    member s.PairsRatio = float (s.PentaWW + s.PentaWD) / float (s.PentaLD + s.PentaLL)
    member s.PointsRatio = s.Points / float s.Sum * 100.0

  /// z for a two-sided 95% interval (elo.hpp).
  let [<Literal>] CI95ZSCORE = 1.959963984540054

  /// A score outside [0, 1] - a confidence bound after very few games - has no Elo: NaN, printed
  /// "-nan" as the reference's release build prints it (its sign is the optimiser's: an -O2 build of
  /// the same source prints "nan"; the release flags, -O3 -march=x86-64, give "-nan").
  let scoreToEloDiff (score: float) = -400.0 * Math.Log10(1.0 / score - 1.0)

  /// One Elo estimate: the reference's EloWDL or EloPentanomial after construction.
  type Elo =
    { Diff: float
      Error: float
      NEloDiff: float
      NEloError: float
      Score: float
      /// variance per game (WDL) or per pair (pentanomial), as LOS uses it
      VariancePer: float }

    /// "{diff:.2f} +/- {error:.2f}"
    member e.GetElo = MatchFormat.fixedPoint 2 e.Diff + " +/- " + MatchFormat.fixedPoint 2 e.Error
    /// "{nelo:.2f} +/- {error:.2f}"
    member e.NElo = MatchFormat.fixedPoint 2 e.NEloDiff + " +/- " + MatchFormat.fixedPoint 2 e.NEloError
    member e.LosValue = (1.0 - MathNet.Numerics.SpecialFunctions.Erf (-(e.Score - 0.5) / Math.Sqrt(2.0 * e.VariancePer))) / 2.0
    /// "{los*100:.2f} %"
    member e.Los = MatchFormat.fixedPoint 2 (e.LosValue * 100.0) + " %"

  let private sq (x: float) = x * x   // std::pow(x, 2) is exact squaring in glibc and MSVC

  /// EloWDL (elo_wdl.cpp).
  let eloWdl (stats: Stats) =
    let games = float stats.Sum
    let w = float stats.Wins / games
    let d = float stats.Draws / games
    let l = float stats.Losses / games
    let score = w + 0.5 * d
    let variance = w * sq (1.0 - score) + d * sq (0.5 - score) + l * sq (0.0 - score)
    let perGame = variance / games
    let upper = score + CI95ZSCORE * Math.Sqrt perGame
    let lower = score - CI95ZSCORE * Math.Sqrt perGame
    let nelo (s: float) = (s - 0.5) / Math.Sqrt variance * (800.0 / Math.Log 10.0)
    { Diff = scoreToEloDiff score
      Error = (scoreToEloDiff upper - scoreToEloDiff lower) / 2.0
      NEloDiff = nelo score
      NEloError = (nelo upper - nelo lower) / 2.0
      Score = score
      VariancePer = perGame }

  /// EloPentanomial (elo_pentanomial.cpp).
  let eloPenta (stats: Stats) =
    let pairs = float stats.TotalPairs
    let ww = float stats.PentaWW / pairs
    let wd = float stats.PentaWD / pairs
    let wl = float stats.PentaWL / pairs
    let dd = float stats.PentaDD / pairs
    let ld = float stats.PentaLD / pairs
    let ll = float stats.PentaLL / pairs
    let score = ww + 0.75 * wd + 0.5 * (wl + dd) + 0.25 * ld
    let variance =
      ww * sq (1.0 - score) + wd * sq (0.75 - score) + (wl + dd) * sq (0.5 - score)
      + ld * sq (0.25 - score) + ll * sq (0.0 - score)
    let perPair = variance / pairs
    let upper = score + CI95ZSCORE * Math.Sqrt perPair
    let lower = score - CI95ZSCORE * Math.Sqrt perPair
    let nelo (s: float) = (s - 0.5) / Math.Sqrt(2.0 * variance) * (800.0 / Math.Log 10.0)
    { Diff = scoreToEloDiff score
      Error = (scoreToEloDiff upper - scoreToEloDiff lower) / 2.0
      NEloDiff = nelo score
      NEloError = (nelo upper - nelo lower) / 2.0
      Score = score
      VariancePer = perPair }

  /// The estimate the reference reports: pentanomial when it reports pentanomial, else WDL.
  let elo (reportPenta: bool) (stats: Stats) = if reportPenta then eloPenta stats else eloWdl stats
