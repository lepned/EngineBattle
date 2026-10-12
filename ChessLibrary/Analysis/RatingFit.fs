/// Ratings of every player of a set of games at once, in process (what EB used Ordo for): a
/// Bradley-Terry maximum-likelihood fit with a draw as half a win (Ordo's "predicted score =
/// observed score" solution), an anchor or the pool average at 0, Ordo's draw model for the error
/// bars and the draw rate for equal opponents, and the confidence for superiority between players.
/// Pure: games in, numbers out.
module ChessLibrary.RatingFit

open System
open System.Collections.Generic
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.Distributions

type Outcome =
    | WhiteWin
    | Draw
    | BlackWin

type Game =
    { White: string
      Black: string
      Outcome: Outcome }

type Options =
    { /// This player is fixed at 0 and the errors are relative to it; None = the pool average is 0
      Anchor: string option
      /// White's advantage in Elo; None = fitted from the games, with a weak prior (whiteAdvantagePriorSd)
      /// that keeps a handful of games from sending it anywhere
      WhiteAdvantage: float option
      /// The draw rate for equal opponents, in percent; None = fitted from the games
      DrawRate: float option
      /// The confidence of the error bars (0.95)
      Confidence: float
      /// 0 = errors from the information matrix; N > 0 = Ordo's way, the games replayed N times
      Simulations: int
      /// The seed of the replays
      Seed: int }

let defaultOptions =
    { Anchor = None
      WhiteAdvantage = None
      DrawRate = None
      Confidence = 0.95
      Simulations = 0
      Seed = 1 }

/// A player who won or lost every game has no finite rating: it is rated as if one of its games
/// were a draw, and the rating is a floor (at least) or a ceiling (at most).
type Bound =
    | Exact
    | Floor
    | Ceiling

type PlayerRating =
    { Name: string
      Rating: float
      /// At the confidence asked for, relative to the anchor (or the pool average)
      Error: float
      Games: int
      Wins: int
      Draws: int
      Losses: int
      Bound: Bound }

type Fit =
    { /// Strongest first
      Players: PlayerRating list
      /// For equal opponents, in percent, with its error (0 when it was given or lies at 0 or 100)
      DrawRate: float
      DrawRateError: float
      WhiteAdvantage: float
      WhiteAdvantageError: float
      /// The pools the games connect; players of different pools cannot be compared
      Groups: string list list
      /// The probability that the first player is stronger than the second; NaN across pools
      Cfs: string -> string -> float }

// ------------------------------------------------------------------ the model

/// Standard Elo: a 400-point difference is a 10:1 expected score.
let beta = Math.Log 10.0 / 400.0

/// The prior on a fitted white advantage: Gaussian, mean 0, this standard deviation in Elo. A few
/// hundred games outweigh it (the games' information grows with their number, the prior's does
/// not: at 1 000 games it moves the estimate by about 1 %); without it six games can put the
/// advantage anywhere.
let whiteAdvantagePriorSd = 100.0

/// The prior on every rating: Gaussian, mean 0, this standard deviation in Elo. Far too wide to
/// move a rated player (0.2 Elo on 300 Elo from 50 games), it keeps a field whose games split it
/// into two camps with every game between them won by the same camp - a four-player round
/// robin after its first round can - from running to thousands of Elo with errors of millions:
/// no finite maximum exists without it. It also makes the problem strictly concave, so no rating
/// has to be held during the solve.
let ratingPriorSd = 2000.0

/// The expected score of a player `diff` Elo stronger than the opponent.
let expectedScore (diff: float) = 1.0 / (1.0 + Math.Exp(-beta * diff))

let private minProb = 1e-32

/// Ordo's draw model: the draw probability at expected score `e` when equal opponents draw a
/// fraction `d0` of their games. The positive root of a D² + 2 D - 4 e (1 - e) = 0 with
/// a = (1 - 2 d0) / d0², written so that a near 0 (d0 near 1/2) is harmless.
let drawProbability (d0: float) (e: float) =
    if d0 <= 0.0 then 0.0
    else
        let d0 = min d0 1.0
        let a = (1.0 - 2.0 * d0) / (d0 * d0)
        let x = e * (1.0 - e)
        4.0 * x / (sqrt (1.0 + 4.0 * a * x) + 1.0)

/// Win, draw and loss probabilities at expected score `e`; they sum to 1 and give `e` as the mean.
let wdl (d0: float) (e: float) =
    let d = drawProbability d0 e
    max minProb (e - d / 2.0), max minProb d, max minProb (1.0 - e - d / 2.0)

// ------------------------------------------------------------------ the games as pair counts

/// Every game between the same White and the same Black, added up.
type private Pair =
    { W: int
      B: int
      mutable Games: int
      /// White's points
      mutable Score: float
      mutable WhiteWins: int
      mutable Draws: int
      mutable BlackWins: int }

let private copyPair (p: Pair) = { p with Games = p.Games }

/// One connected pool: its players (local indices), its pairs and each player's pairs.
type private Pool =
    { Names: string[]
      Pairs: Pair[]
      ByPlayer: int[][] }

let private poolOf (names: string[]) (pairs: Pair[]) =
    let byPlayer = Array.init names.Length (fun _ -> ResizeArray<int>())
    pairs |> Array.iteri (fun k p -> byPlayer.[p.W].Add k; byPlayer.[p.B].Add k)
    { Names = names; Pairs = pairs; ByPlayer = byPlayer |> Array.map (fun l -> l.ToArray()) }

/// The games split into connected pools (union-find over the players).
let private pools (games: Game seq) : Pool list =
    let index = Dictionary<string, int>()
    let names = ResizeArray<string>()
    let idx name =
        match index.TryGetValue name with
        | true, i -> i
        | _ ->
            index.[name] <- names.Count
            names.Add name
            names.Count - 1
    let pairs = Dictionary<struct (int * int), Pair>()
    for g in games do
        if g.White <> g.Black then
            let w, b = idx g.White, idx g.Black
            let p =
                match pairs.TryGetValue(struct (w, b)) with
                | true, p -> p
                | _ ->
                    let p = { W = w; B = b; Games = 0; Score = 0.0; WhiteWins = 0; Draws = 0; BlackWins = 0 }
                    pairs.[struct (w, b)] <- p
                    p
            p.Games <- p.Games + 1
            match g.Outcome with
            | WhiteWin -> p.Score <- p.Score + 1.0; p.WhiteWins <- p.WhiteWins + 1
            | Draw -> p.Score <- p.Score + 0.5; p.Draws <- p.Draws + 1
            | BlackWin -> p.BlackWins <- p.BlackWins + 1
    let n = names.Count
    let parent = Array.init n id
    let rec find i = if parent.[i] = i then i else (parent.[i] <- find parent.[i]; parent.[i])
    for p in pairs.Values do parent.[find p.W] <- find p.B
    let groups = Dictionary<int, ResizeArray<int>>()
    for i in 0 .. n - 1 do
        let root = find i
        match groups.TryGetValue root with
        | true, g -> g.Add i
        | _ -> groups.[root] <- ResizeArray [ i ]
    [ for g in groups.Values do
        let members = g.ToArray()
        let local = Dictionary<int, int>()
        members |> Array.iteri (fun k i -> local.[i] <- k)
        let localPairs =
            pairs.Values
            |> Seq.filter (fun p -> local.ContainsKey p.W)
            |> Seq.map (fun p -> { p with W = local.[p.W]; B = local.[p.B] })
            |> Seq.toArray
        poolOf (members |> Array.map (fun i -> names.[i])) localPairs ]

/// Each player's points and games.
let private totals (pool: Pool) =
    let score = Array.zeroCreate<float> pool.Names.Length
    let games = Array.zeroCreate<int> pool.Names.Length
    for p in pool.Pairs do
        score.[p.W] <- score.[p.W] + p.Score
        score.[p.B] <- score.[p.B] + float p.Games - p.Score
        games.[p.W] <- games.[p.W] + p.Games
        games.[p.B] <- games.[p.B] + p.Games
    score, games

/// Perfect scores (every game won or lost) have no finite rating. As Ordo: one of the player's
/// games is counted as a draw and the rating becomes a floor or a ceiling. Returns the pairs to
/// fit (copies) and each player's bound.
let private boundPerfectScores (pool: Pool) =
    let pairs = pool.Pairs |> Array.map copyPair
    let fitted = { pool with Pairs = pairs }
    // marked from the games as played; a converted draw is only needed while the score is still perfect
    let score0, games0 = totals pool
    let bounds = Array.init pool.Names.Length (fun i -> if games0.[i] = 0 then Exact elif score0.[i] = float games0.[i] then Floor elif score0.[i] = 0.0 then Ceiling else Exact)
    for i in 0 .. pool.Names.Length - 1 do
        let score, games = totals fitted
        if games.[i] > 0 && score.[i] = float games.[i] then
            // a win of i turned into a draw: the pair with the most games where it won
            let k = fitted.ByPlayer.[i] |> Array.filter (fun k -> let p = pairs.[k] in if p.W = i then p.WhiteWins > 0 else p.BlackWins > 0) |> Array.maxBy (fun k -> pairs.[k].Games)
            let p = pairs.[k]
            if p.W = i then (p.WhiteWins <- p.WhiteWins - 1; p.Score <- p.Score - 0.5)
            else (p.BlackWins <- p.BlackWins - 1; p.Score <- p.Score + 0.5)
            p.Draws <- p.Draws + 1
        elif games.[i] > 0 && score.[i] = 0.0 then
            let k = fitted.ByPlayer.[i] |> Array.filter (fun k -> let p = pairs.[k] in if p.W = i then p.BlackWins > 0 else p.WhiteWins > 0) |> Array.maxBy (fun k -> pairs.[k].Games)
            let p = pairs.[k]
            if p.W = i then (p.BlackWins <- p.BlackWins - 1; p.Score <- p.Score + 0.5)
            else (p.WhiteWins <- p.WhiteWins - 1; p.Score <- p.Score - 0.5)
            p.Draws <- p.Draws + 1
    fitted, bounds

// ------------------------------------------------------------------ the fit

/// The ratings (pool average 0) and the white advantage: Newton's method on the Bradley-Terry
/// log-likelihood with draws as half and the priors, each step solved by conjugate gradients on
/// the sparse Hessian (one pass over the pairs per CG iteration), with backtracking so the
/// likelihood never falls. The ratings are centred at the end (the prior leaves them there
/// already). Returns ratings, white advantage, converged.
let private solve (pool: Pool) (whiteAdvantage: float option) =
    let n = pool.Names.Length
    let fitW = whiteAdvantage.IsNone
    let dim = n + (if fitW then 1 else 0)
    let w0 = defaultArg whiteAdvantage 0.0
    let wOf (x: float[]) = if fitW then x.[n] else w0
    let pairs = pool.Pairs
    let logLikelihood (x: float[]) =
        let w = wOf x
        let mutable ll = 0.0
        for p in pairs do
            let e = expectedScore (x.[p.W] + w - x.[p.B]) |> max 1e-300 |> min (1.0 - 1e-16)
            ll <- ll + p.Score * log e + (float p.Games - p.Score) * log (1.0 - e)
        for i in 0 .. n - 1 do ll <- ll - x.[i] * x.[i] / (2.0 * ratingPriorSd * ratingPriorSd)
        if fitW then ll - w * w / (2.0 * whiteAdvantagePriorSd * whiteAdvantagePriorSd) else ll
    let gradient (x: float[]) =
        let w = wOf x
        let g = Array.zeroCreate<float> dim
        for p in pairs do
            let d = beta * (p.Score - float p.Games * expectedScore (x.[p.W] + w - x.[p.B]))
            g.[p.W] <- g.[p.W] + d
            g.[p.B] <- g.[p.B] - d
            if fitW then g.[n] <- g.[n] + d
        for i in 0 .. n - 1 do g.[i] <- g.[i] - x.[i] / (ratingPriorSd * ratingPriorSd)
        if fitW then g.[n] <- g.[n] - w / (whiteAdvantagePriorSd * whiteAdvantagePriorSd)
        g
    // -Hessian as a function, and its diagonal for the preconditioner
    let curvature (x: float[]) =
        let w = wOf x
        let c = pairs |> Array.map (fun p -> let e = expectedScore (x.[p.W] + w - x.[p.B]) in beta * beta * float p.Games * e * (1.0 - e))
        let apply (v: float[]) =
            let y = Array.zeroCreate<float> dim
            for k in 0 .. pairs.Length - 1 do
                let p = pairs.[k]
                let sv = c.[k] * (v.[p.W] - v.[p.B] + (if fitW then v.[n] else 0.0))
                y.[p.W] <- y.[p.W] + sv
                y.[p.B] <- y.[p.B] - sv
                if fitW then y.[n] <- y.[n] + sv
            for i in 0 .. n - 1 do y.[i] <- y.[i] + v.[i] / (ratingPriorSd * ratingPriorSd)
            if fitW then y.[n] <- y.[n] + v.[n] / (whiteAdvantagePriorSd * whiteAdvantagePriorSd)
            y
        let diag = Array.zeroCreate<float> dim
        for k in 0 .. pairs.Length - 1 do
            let p = pairs.[k]
            diag.[p.W] <- diag.[p.W] + c.[k]
            diag.[p.B] <- diag.[p.B] + c.[k]
            if fitW then diag.[n] <- diag.[n] + c.[k]
        for i in 0 .. n - 1 do diag.[i] <- diag.[i] + 1.0 / (ratingPriorSd * ratingPriorSd)
        if fitW then diag.[n] <- diag.[n] + 1.0 / (whiteAdvantagePriorSd * whiteAdvantagePriorSd)
        apply, diag
    let dot (a: float[]) (b: float[]) = let mutable s = 0.0 in (for k in 0 .. a.Length - 1 do s <- s + a.[k] * b.[k]); s
    // preconditioned conjugate gradients for A d = g
    let cg (apply: float[] -> float[]) (diag: float[]) (g: float[]) =
        let d = Array.zeroCreate<float> dim
        let r = Array.copy g
        let z = Array.init dim (fun i -> if diag.[i] > 0.0 then r.[i] / diag.[i] else r.[i])
        let p = Array.copy z
        let mutable rz = dot r z
        let tol = 1e-20 * max 1.0 (dot g g)
        let mutable it = 0
        while rz > 0.0 && dot r r > tol && it < 10 * dim + 100 do
            let ap = apply p
            let alpha = rz / dot p ap
            for i in 0 .. dim - 1 do
                d.[i] <- d.[i] + alpha * p.[i]
                r.[i] <- r.[i] - alpha * ap.[i]
            let z = Array.init dim (fun i -> if diag.[i] > 0.0 then r.[i] / diag.[i] else r.[i])
            let rzNew = dot r z
            let b = rzNew / rz
            for i in 0 .. dim - 1 do p.[i] <- z.[i] + b * p.[i]
            rz <- rzNew
            it <- it + 1
        d
    let x = Array.zeroCreate<float> dim
    let mutable ll = logLikelihood x
    let mutable converged = false
    let mutable iter = 0
    while not converged && iter < 100 do
        let g = gradient x
        if g |> Array.forall (fun v -> abs v < 1e-12) then converged <- true
        else
            let apply, diag = curvature x
            let d = cg apply diag g
            let mutable t = 1.0
            let mutable tries = 0
            let mutable next = Array.map2 (fun xi di -> xi + t * di) x d
            let mutable llNext = logLikelihood next
            while llNext < ll - 1e-12 && tries < 40 do
                t <- t / 2.0
                next <- Array.map2 (fun xi di -> xi + t * di) x d
                llNext <- logLikelihood next
                tries <- tries + 1
            let moved = d |> Array.map (fun v -> abs (t * v)) |> Array.max
            Array.blit next 0 x 0 dim
            ll <- llNext
            if moved < 1e-7 then converged <- true
        iter <- iter + 1
    let mean = (Array.take n x |> Array.sum) / float n
    let r = Array.init n (fun i -> x.[i] - mean)
    r, wOf x, converged

/// The draw rate for equal opponents, as Ordo's -D finds it: the one at which the model's expected
/// number of draws over the games equals the number played (bisection: the expectation rises with
/// the rate). Its error by the delta method, from the variance of the draw count under the model.
/// 0 or 1 with error 0 when the games have no draw, or nothing else (or the rate lands on 1).
/// The draws are the games as played (`pool`); the expected scores come from the fitted ratings.
let private fitDrawRate (pool: Pool) (r: float[]) (w: float) =
    let es = pool.Pairs |> Array.map (fun p -> expectedScore (r.[p.W] + w - r.[p.B]))
    let draws = pool.Pairs |> Array.sumBy (fun p -> float p.Draws)
    let games = pool.Pairs |> Array.sumBy (fun p -> float p.Games)
    if draws <= 0.0 then 0.0, 0.0
    elif draws >= games then 1.0, 0.0
    else
        let expectedDraws d0 = Array.fold2 (fun acc e (p: Pair) -> acc + float p.Games * drawProbability d0 e) 0.0 es pool.Pairs
        let mutable lo, hi = 1e-9, 1.0
        for _ in 1 .. 60 do
            let mid = (lo + hi) / 2.0
            if expectedDraws mid < draws then lo <- mid else hi <- mid
        let d0 = (lo + hi) / 2.0
        if d0 >= 1.0 - 1e-6 then 1.0, 0.0
        else
            let variance = Array.fold2 (fun acc e (p: Pair) -> let d = drawProbability d0 e in acc + float p.Games * d * (1.0 - d)) 0.0 es pool.Pairs
            let h = 1e-5
            let slope = (expectedDraws (min 1.0 (d0 + h)) - expectedDraws (max 0.0 (d0 - h))) / (min 1.0 (d0 + h) - max 0.0 (d0 - h))
            d0, (if slope > 0.0 then sqrt variance / slope else 0.0)

/// The variances and covariances of the ratings (and of the white advantage, index n, when fitted).
type private Covariance =
    { Var: int -> float
      Cov: int -> int -> float }

/// The covariance of the ratings under the draw model: the sandwich H⁻¹ J H⁻¹ of the
/// Bradley-Terry estimator (H the curvature of its log-likelihood, priors included; J the
/// covariance of its score under win/draw/loss outcomes), as the covariance of the contrasts
/// R_i - R_anchor or, with no anchor, R_i - mean. J is as sparse as the game graph, so entries
/// are computed on demand from H⁻¹'s columns, O(pairs + n) each, and the centering from
/// matrix-vector products - no dense product of n x n matrices.
let private sandwich (pool: Pool) (r: float[]) (w: float) (fitW: bool) (d0: float) (anchor: int option) : Covariance =
    let n = pool.Names.Length
    let dim = n + (if fitW then 1 else 0)
    let terms =
        pool.Pairs |> Array.map (fun p ->
            let e = expectedScore (r.[p.W] + w - r.[p.B])
            let pw, pd, pl = wdl d0 e
            let hv = beta * beta * float p.Games * e * (1.0 - e)
            let jv = beta * beta * float p.Games * (pw * (1.0 - e) ** 2.0 + pd * (0.5 - e) ** 2.0 + pl * e * e)
            struct (p.W, p.B, hv, jv))
    let h = Array2D.zeroCreate<float> dim dim
    for struct (wi, bi, hv, _) in terms do
        let add a b v = h.[a, b] <- h.[a, b] + v
        add wi wi hv; add bi bi hv; add wi bi -hv; add bi wi -hv
        if fitW then (add wi n hv; add n wi hv; add bi n -hv; add n bi -hv; add n n hv)
    // the priors' curvature (they are no random quantities: nothing in J)
    for i in 0 .. n - 1 do h.[i, i] <- h.[i, i] + 1.0 / (ratingPriorSd * ratingPriorSd)
    if fitW then h.[n, n] <- h.[n, n] + 1.0 / (whiteAdvantagePriorSd * whiteAdvantagePriorSd)
    let hInv = (DenseMatrix.ofArray2 h).Cholesky().Solve(DenseMatrix.identity dim)
    let hCol i = Array.init dim (fun k -> hInv.[k, i])
    // J x over the pairs (J = Σ jv s sᵀ with s = +1 at White, -1 at Black, +1 at w)
    let applyJ (x: float[]) =
        let y = Array.zeroCreate<float> dim
        for struct (wi, bi, _, jv) in terms do
            let sx = jv * (x.[wi] - x.[bi] + (if fitW then x.[n] else 0.0))
            y.[wi] <- y.[wi] + sx
            y.[bi] <- y.[bi] - sx
            if fitW then y.[n] <- y.[n] + sx
        y
    let dot (a: float[]) (b: float[]) = let mutable s = 0.0 in (for k in 0 .. a.Length - 1 do s <- s + a.[k] * b.[k]); s
    let entry i j = dot (hCol i) (applyJ (hCol j))
    match anchor with
    | Some a ->
        // Cov(R_i - R_a, R_j - R_a) = V_ij - V_ia - V_ja + V_aa; the white advantage (index n) as it is
        let vaa = entry a a
        let viaA = Array.create dim nan
        let via i = if i >= n then 0.0 else (if Double.IsNaN viaA.[i] then viaA.[i] <- entry i a); viaA.[i]
        let rel i j = entry i j - via i - via j + (if i < n && j < n then vaa else 0.0)
        { Var = (fun i -> rel i i); Cov = rel }
    | None ->
        // Cov(R_i - mean, R_j - mean) = V_ij - m_i - m_j + g, with m = V 1_R / n and g = 1_Rᵀ V 1_R / n²
        let hTimes (x: float[]) = Array.init dim (fun k -> dot (hCol k) x)
        let v1 = hTimes (applyJ (hTimes (Array.init dim (fun k -> if k < n then 1.0 else 0.0))))
        let m = v1 |> Array.map (fun x -> x / float n)
        let g = (v1 |> Array.take n |> Array.sum) / float (n * n)
        let rel i j = entry i j - (if i < n then m.[j] else 0.0) - (if j < n then m.[i] else 0.0) + (if i < n && j < n then g else 0.0)
        { Var = (fun i -> rel i i); Cov = rel }

/// Ordo's errors: the games replayed `sims` times with outcomes drawn from the fitted
/// probabilities, refitted each time; the covariance of the ratings over the replays.
let private simulate (pool: Pool) (r: float[]) (w: float) (whiteAdvantage: float option) (d0: float) (fixedAt: int option) (sims: int) (seed: int) =
    let n = pool.Names.Length
    let fitW = whiteAdvantage.IsNone
    let dim = n + (if fitW then 1 else 0)
    let samples = Array2D.zeroCreate<float> sims dim
    let probs = pool.Pairs |> Array.map (fun p -> wdl d0 (expectedScore (r.[p.W] + w - r.[p.B])))
    Threading.Tasks.Parallel.For(0, sims, fun s ->
        let rng = Random(seed + s)
        let pairs =
            pool.Pairs |> Array.mapi (fun k p ->
                let pw, pd, _ = probs.[k]
                let q = { copyPair p with Games = p.Games; Score = 0.0; WhiteWins = 0; Draws = 0; BlackWins = 0 }
                for _ in 1 .. p.Games do
                    let u = rng.NextDouble()
                    if u < pw then (q.WhiteWins <- q.WhiteWins + 1; q.Score <- q.Score + 1.0)
                    elif u < pw + pd then (q.Draws <- q.Draws + 1; q.Score <- q.Score + 0.5)
                    else q.BlackWins <- q.BlackWins + 1
                q)
        // a replay can hand a player a perfect score: bounded as the real games are
        let replay, _ = boundPerfectScores { pool with Pairs = pairs }
        let rs, ws, _ = solve replay whiteAdvantage
        let shift = match fixedAt with Some i -> rs.[i] | None -> 0.0
        for i in 0 .. n - 1 do samples.[s, i] <- rs.[i] - shift
        if fitW then samples.[s, n] <- ws) |> ignore
    let mean = Array.init dim (fun i -> (Seq.init sims (fun s -> samples.[s, i]) |> Seq.average))
    let v =
        Array2D.init dim dim (fun a b ->
            (Seq.init sims (fun s -> (samples.[s, a] - mean.[a]) * (samples.[s, b] - mean.[b])) |> Seq.sum) / float (max 1 (sims - 1)))
    { Var = (fun i -> v.[i, i]); Cov = (fun i j -> v.[i, j]) }

/// The rating fit of the games: see the module's types.
let fit (options: Options) (games: Game seq) : Fit =
    let z = Normal.InvCDF(0.0, 1.0, 0.5 + options.Confidence / 2.0)
    let byPool = Dictionary<string, (Covariance * Dictionary<string, int> * float[])>()
    let players = ResizeArray<PlayerRating>()
    let mutable drawRate, drawRateError = 0.0, 0.0
    let mutable whiteAdv, whiteAdvError = defaultArg options.WhiteAdvantage 0.0, 0.0
    let groups = ResizeArray<string list>()
    // the draw rate and the white advantage are one number for the whole set: taken from the
    // largest pool (one pool is the normal case)
    let poolList = pools games |> List.sortByDescending (fun p -> p.Pairs |> Array.sumBy (fun q -> q.Games))
    poolList |> List.iteri (fun poolNr pool ->
        let fitted, bounds = boundPerfectScores pool
        let r, w, _ = solve fitted options.WhiteAdvantage
        let fitW = options.WhiteAdvantage.IsNone
        let d0, d0Err =
            match options.DrawRate with
            | Some pct -> pct / 100.0, 0.0
            | None -> fitDrawRate pool r w
        let n = pool.Names.Length
        let anchorAt = options.Anchor |> Option.bind (fun a -> pool.Names |> Array.tryFindIndex ((=) a))
        let shift = match anchorAt with Some i -> r.[i] | None -> 0.0
        let ratings = r |> Array.map (fun x -> x - shift)
        // the errors' draw model stops short of "every game a draw": at 1 the variance of every
        // outcome is 0 and five opening draws would print no errors at all
        let d0Errors = min d0 0.99
        let v =
            if options.Simulations > 0 then simulate fitted r w options.WhiteAdvantage d0Errors anchorAt options.Simulations (options.Seed + 1000 * poolNr)
            else sandwich fitted r w fitW d0Errors anchorAt
        if poolNr = 0 then
            drawRate <- d0 * 100.0
            drawRateError <- d0Err * 100.0
            whiteAdv <- w
            whiteAdvError <- if fitW then z * sqrt (max 0.0 (v.Var n)) else 0.0
        let _, gamesOf = totals pool
        let wins = Array.zeroCreate n
        let draws = Array.zeroCreate n
        for p in pool.Pairs do
            wins.[p.W] <- wins.[p.W] + p.WhiteWins
            wins.[p.B] <- wins.[p.B] + p.BlackWins
            draws.[p.W] <- draws.[p.W] + p.Draws
            draws.[p.B] <- draws.[p.B] + p.Draws
        let index = Dictionary<string, int>()
        pool.Names |> Array.iteri (fun i name -> index.[name] <- i)
        byPool.[string poolNr] <- (v, index, ratings)
        for i in 0 .. n - 1 do
            players.Add
                { Name = pool.Names.[i]
                  Rating = ratings.[i]
                  Error = z * sqrt (max 0.0 (v.Var i))
                  Games = gamesOf.[i]
                  Wins = wins.[i]
                  Draws = draws.[i]
                  Losses = gamesOf.[i] - wins.[i] - draws.[i]
                  Bound = bounds.[i] }
        groups.Add(pool.Names |> Array.sortByDescending (fun name -> ratings.[index.[name]]) |> List.ofArray))
    let poolOfPlayer = Dictionary<string, string>()
    for KeyValue(k, (_, index, _)) in byPool do
        for name in index.Keys do poolOfPlayer.[name] <- k
    let cfs a b =
        match poolOfPlayer.TryGetValue a, poolOfPlayer.TryGetValue b with
        | (true, pa), (true, pb) when pa = pb ->
            let v, index, ratings = byPool.[pa]
            let i, j = index.[a], index.[b]
            let diff = ratings.[i] - ratings.[j]
            let var = v.Var i + v.Var j - 2.0 * v.Cov i j
            if var > 1e-12 then Normal.CDF(0.0, 1.0, diff / sqrt var)
            elif diff > 0.0 then 1.0 elif diff < 0.0 then 0.0 else 0.5
        | _ -> nan
    { Players = players |> Seq.sortByDescending (fun p -> p.Rating) |> List.ofSeq
      DrawRate = drawRate
      DrawRateError = drawRateError
      WhiteAdvantage = whiteAdv
      WhiteAdvantageError = whiteAdvError
      Groups = groups |> List.ofSeq
      Cfs = cfs }
