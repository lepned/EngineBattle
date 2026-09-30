namespace ChessLibrary.Match

open System

/// The reference's SPRT (matchmaking/sprt/sprt.cpp at 60d7a7a), ported operation for operation: the
/// bounds from alpha/beta, the log-likelihood ratio for the normalized, logistic and bayesian
/// models over trinomial or pentanomial counts, and the ITP root finder the models solve with.
/// The reference credits Michel Van den Bergh's notes on the generalized LLR and normalized Elo, and
/// Oliveira & Takahashi (2020) for ITP.
module MatchSprt =

  type SprtResult =
    | H0
    | H1
    | Continue

  type Sprt =
    { Enabled: bool
      Lower: float
      Upper: float
      Elo0: float
      Elo1: float
      Model: string }

  /// SPRT(alpha, beta, elo0, elo1, model, enabled); a disabled one keeps zeros, as the reference's.
  let create (alpha: float) (beta: float) (elo0: float) (elo1: float) (model: string) (enabled: bool) =
    if enabled then
      { Enabled = true
        Lower = Math.Log(beta / (1.0 - alpha))
        Upper = Math.Log((1.0 - beta) / alpha)
        Elo0 = elo0
        Elo1 = elo1
        Model = model }
    else
      { Enabled = false; Lower = 0.0; Upper = 0.0; Elo0 = 0.0; Elo1 = 0.0; Model = "normalized" }

  /// SPRT::isValid: the first error, in the reference's order and words, or None. `reportPenta` comes
  /// back false for the bayesian model, with the warning the reference prints for it.
  let validate (alpha: float) (beta: float) (elo0: float) (elo1: float) (model: string) (reportPenta: bool) =
    if elo0 >= elo1 then Error "Error; SPRT: elo0 must be less than elo1!"
    elif alpha <= 0.0 || alpha >= 1.0 then Error "Error; SPRT: alpha must be a decimal number between 0 and 1!"
    elif beta <= 0.0 || beta >= 1.0 then Error "Error; SPRT: beta must be a decimal number between 0 and 1!"
    elif alpha + beta >= 1.0 then Error "Error; SPRT: sum of alpha and beta must be less than 1!"
    elif model <> "normalized" && model <> "bayesian" && model <> "logistic" then Error "Error; SPRT: invalid SPRT model!"
    elif model = "bayesian" && reportPenta then
      Ok (false, Some "Warning; Bayesian SPRT model not available with pentanomial statistics. Disabling pentanomial reports...")
    else Ok (reportPenta, None)

  let leloToScore (lelo: float) = 1.0 / (1.0 + Math.Pow(10.0, -lelo / 400.0))

  let bayeseloToScore (bayeselo: float) (drawelo: float) =
    let pwin = 1.0 / (1.0 + Math.Pow(10.0, (-bayeselo + drawelo) / 400.0))
    let ploss = 1.0 / (1.0 + Math.Pow(10.0, (bayeselo + drawelo) / 400.0))
    let pdraw = 1.0 - pwin - ploss
    pwin + 0.5 * pdraw

  let private regularize (value: int) = if value = 0 then 1e-3 else float value

  let private mean (x: float[]) (p: float[]) =
    let mutable result = 0.0
    for i in 0 .. x.Length - 1 do result <- result + x.[i] * p.[i]
    result

  let private meanAndVariance (x: float[]) (p: float[]) =
    let mu = mean x p
    let mutable var = 0.0
    for i in 0 .. x.Length - 1 do var <- var + p.[i] * (x.[i] - mu) * (x.[i] - mu)
    mu, var

  /// ITP (interpolate, truncate, project), exactly as the reference's: the same swap when f(a) > 0,
  /// the same step count, the same sign tests. Its callers pass +inf/-inf as f(a)/f(b), so the
  /// first regula-falsi point is NaN and the first step bisects - that is reproduced too.
  let itp (f: float -> float) (a: float) (b: float) (fA: float) (fB: float) (k1: float) (k2: float) (n0: float) (epsilon: float) =
    let mutable a = a
    let mutable b = b
    let mutable fA = fA
    let mutable fB = fB
    if fA > 0.0 then
      let t = a in a <- b; b <- t
      let t = fA in fA <- fB; fB <- t
    let nHalf = Math.Ceiling(Math.Log2(abs (b - a) / (2.0 * epsilon)))
    let nMax = nHalf + n0
    let mutable i = 0.0
    while abs (b - a) > 2.0 * epsilon do
      let xHalf = (a + b) / 2.0
      let r = epsilon * Math.Pow(2.0, nMax - i) - (b - a) / 2.0
      let delta = k1 * Math.Pow(b - a, k2)
      let xF = (fB * a - fA * b) / (fB - fA)
      let sigma = (xHalf - xF) / abs (xHalf - xF)
      let xT = if delta <= abs (xHalf - xF) then xF + sigma * delta else xHalf
      let xItp = if abs (xT - xHalf) <= r then xT else xHalf - sigma * r
      let fItp = f xItp
      if fItp = 0.0 then
        a <- xItp
        b <- xItp
      elif Double.IsNegative fItp then
        a <- xItp
        fA <- fItp
      else
        b <- xItp
        fB <- fItp
      i <- i + 1.0
    (a + b) / 2.0

  /// getLLR_logistic: the generalized LLR between the maximum-likelihood distributions with
  /// expected scores s0 and s1.
  let private llrLogistic (total: float) (scores: float[]) (probs: float[]) (s0: float) (s1: float) =
    let n = scores.Length
    let mle (s: float) =
      let minTheta = -1.0 / (scores.[n - 1] - s)
      let maxTheta = -1.0 / (scores.[0] - s)
      let theta =
        itp (fun x ->
              let mutable result = 0.0
              for i in 0 .. n - 1 do
                result <- result + probs.[i] * (scores.[i] - s) / (1.0 + x * (scores.[i] - s))
              result)
            minTheta maxTheta Double.PositiveInfinity Double.NegativeInfinity 0.1 2.0 0.99 1e-3
      Array.init n (fun i -> probs.[i] / (1.0 + theta * (scores.[i] - s)))
    let p0 = mle s0
    let p1 = mle s1
    total * mean (Array.init n (fun i -> Math.Log p1.[i] - Math.Log p0.[i])) probs

  /// getLLR_normalized: the same with distributions of normalized Elo t0 and t1, found
  /// iteratively from a uniform start (at most 10 rounds, stopping below 1e-4 change).
  let private llrNormalized (total: float) (scores: float[]) (probs: float[]) (t0: float) (t1: float) =
    let n = scores.Length
    let mle (muRef: float) (tStar: float) =
      let p = Array.create n (1.0 / float n)
      let mutable iterations = 0
      let mutable fin = false
      while not fin && iterations < 10 do
        let mu, var = meanAndVariance scores p
        let sigma = Math.Sqrt var
        let phi =
          Array.init n (fun i ->
            let aI = scores.[i]
            aI - muRef - 0.5 * tStar * sigma * (1.0 + ((aI - mu) / sigma) * ((aI - mu) / sigma)))
        let u = Array.min phi
        let v = Array.max phi
        let theta =
          itp (fun x ->
                let mutable result = 0.0
                for i in 0 .. n - 1 do result <- result + probs.[i] * phi.[i] / (1.0 + x * phi.[i])
                result)
              (-1.0 / v) (-1.0 / u) Double.PositiveInfinity Double.NegativeInfinity 0.1 2.0 0.99 1e-7
        let mutable maxDiff = 0.0
        for i in 0 .. n - 1 do
          let newP = probs.[i] / (1.0 + theta * phi.[i])
          maxDiff <- max maxDiff (abs (newP - p.[i]))
          p.[i] <- newP
        if maxDiff < 1e-4 then fin <- true
        iterations <- iterations + 1
      p
    let p0 = mle 0.5 t0
    let p1 = mle 0.5 t1
    total * mean (Array.init n (fun i -> Math.Log p1.[i] - Math.Log p0.[i])) probs

  let private eloScale = 800.0 / Math.Log 10.0

  /// LLR over W/D/L (getLLR(win, draw, loss)).
  let llrTrinomial (sprt: Sprt) (win: int) (draw: int) (loss: int) =
    if not sprt.Enabled then 0.0
    else
      let l = regularize loss
      let d = regularize draw
      let w = regularize win
      let total = l + d + w
      let probs = [| l / total; d / total; w / total |]
      let scores = [| 0.0; 0.5; 1.0 |]
      if sprt.Model = "normalized" then
        llrNormalized total scores probs (sprt.Elo0 / eloScale) (sprt.Elo1 / eloScale)
      elif sprt.Model = "bayesian" then
        if win = 0 || loss = 0 then 0.0
        else
          let pl = probs.[0]
          let pw = probs.[2]
          let drawelo = 200.0 * Math.Log10((1.0 - pl) / pl * (1.0 - pw) / pw)
          llrLogistic total scores probs (bayeseloToScore sprt.Elo0 drawelo) (bayeseloToScore sprt.Elo1 drawelo)
      else
        llrLogistic total scores probs (leloToScore sprt.Elo0) (leloToScore sprt.Elo1)

  /// LLR over pentanomial pairs (getLLR(ww, wd, wl, dd, ld, ll)); bayesian falls back to logistic
  /// here, as in the reference (which turns pentanomial off for bayesian anyway).
  let llrPentanomial (sprt: Sprt) (ww: int) (wd: int) (wl: int) (dd: int) (ld: int) (ll: int) =
    if not sprt.Enabled then 0.0
    else
      let pLL = regularize ll
      let pLD = regularize ld
      let pWLDD = regularize (dd + wl)
      let pWD = regularize wd
      let pWW = regularize ww
      let total = pWW + pWD + pWLDD + pLD + pLL
      let probs = [| pLL / total; pLD / total; pWLDD / total; pWD / total; pWW / total |]
      let scores = [| 0.0; 0.25; 0.5; 0.75; 1.0 |]
      if sprt.Model = "normalized" then
        llrNormalized total scores probs (Math.Sqrt 2.0 * sprt.Elo0 / eloScale) (Math.Sqrt 2.0 * sprt.Elo1 / eloScale)
      else
        llrLogistic total scores probs (leloToScore sprt.Elo0) (leloToScore sprt.Elo1)

  /// getLLR(stats, penta)
  let llr (sprt: Sprt) (stats: MatchStats.Stats) (penta: bool) =
    if penta then llrPentanomial sprt stats.PentaWW stats.PentaWD stats.PentaWL stats.PentaDD stats.PentaLD stats.PentaLL
    else llrTrinomial sprt stats.Wins stats.Draws stats.Losses

  /// How far towards a bound, for "LLR: x (y%)": llr/upper, or below zero -llr/lower, which is
  /// negative (lower < 0), so the reference prints a negative percentage there.
  let fraction (sprt: Sprt) (llr: float) = if llr >= 0.0 then llr / sprt.Upper else -llr / sprt.Lower

  let result (sprt: Sprt) (llr: float) =
    if not sprt.Enabled then Continue
    elif llr >= sprt.Upper then H1
    elif llr <= sprt.Lower then H0
    else Continue

  /// "({lower:.2f}, {upper:.2f})"
  let bounds (sprt: Sprt) = "(" + MatchFormat.fixedPoint 2 sprt.Lower + ", " + MatchFormat.fixedPoint 2 sprt.Upper + ")"

  /// "[{elo0:.2f}, {elo1:.2f}]"
  let eloRange (sprt: Sprt) = "[" + MatchFormat.fixedPoint 2 sprt.Elo0 + ", " + MatchFormat.fixedPoint 2 sprt.Elo1 + "]"
