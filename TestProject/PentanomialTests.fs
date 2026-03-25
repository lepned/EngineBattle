module PentanomialTests

open Xunit
open ChessLibrary.PGNTypes
open ChessLibrary.Configuration
open ChessLibrary.Statistics

let private mkGame gameNumber openingHash white black result =
    let meta =
        { GameMetadata.Empty with
            White = white
            Black = black
            Result = result
            OpeningHash = openingHash }
    { PgnGame.Empty gameNumber with GameMetaData = meta }

[<Fact>]
let ``Pentanomial counts W15 for 1.5-0.5 pair`` () =
    let g1 = mkGame 1 "h1" "A" "B" "1-0"        // A scores 1.0
    let g2 = mkGame 2 "h1" "B" "A" "1/2-1/2"    // A scores 0.5
    let res = Pentanomial.calculateAllMatchups [ g1; g2 ]

    Assert.Single(res) |> ignore
    let ((a, b), c) = res.Head
    Assert.Equal(("A", "B"), (a, b))
    Assert.Equal(1, c.W15)
    Assert.Equal(1, c.CompletedPairs)
    Assert.Equal(0, c.IncompletePairs)

[<Fact>]
let ``Pentanomial reports incomplete pair when only one game exists`` () =
    let g1 = mkGame 1 "h1" "A" "B" "1-0"
    let res = Pentanomial.calculateAllMatchups [ g1 ]

    Assert.Single(res) |> ignore
    let ((a, b), c) = res.Head
    Assert.Equal(("A", "B"), (a, b))
    Assert.Equal(0, c.CompletedPairs)
    Assert.Equal(1, c.IncompletePairs)

[<Fact>]
let ``Pentanomial counts W2 for 2-0 pair`` () =
    let g1 = mkGame 1 "h1" "A" "B" "1-0"  // A scores 1.0
    let g2 = mkGame 2 "h1" "B" "A" "0-1"  // A scores 1.0 (as black)
    let res = Pentanomial.calculateAllMatchups [ g1; g2 ]

    let ((_, _), c) = res.Head
    Assert.Equal(1, c.W2)
    Assert.Equal(1, c.CompletedPairs)

[<Fact>]
let ``Pentanomial per-engine totals count both sides`` () =
    // One opening pair: A gets 1.5, B gets 0.5
    let g1 = mkGame 1 "h1" "A" "B" "1-0"
    let g2 = mkGame 2 "h1" "B" "A" "1/2-1/2"
    let res = Pentanomial.calculatePerEngine [ g1; g2 ]

    let a = res |> List.find (fun e -> e.Engine = "A")
    let b = res |> List.find (fun e -> e.Engine = "B")

    Assert.Equal(1, a.W15)
    Assert.Equal(1, a.CompletedPairs)
    Assert.Equal(1, b.L15)
    Assert.Equal(1, b.CompletedPairs)

[<Fact>]
let ``calculateAllMatchups returns counts from alphabetically-first engine perspective`` () =
    // "[tuned]" sorts after "[initial]" alphabetically, so counts are from [initial]'s perspective.
    // [initial] wins both games as white and black → W2 for [initial], which is L2 for [tuned].
    let g1 = mkGame 1 "h1" "[initial]" "[tuned]" "1-0"   // [initial] wins as white
    let g2 = mkGame 2 "h1" "[tuned]" "[initial]" "0-1"   // [initial] wins as black
    let res = Pentanomial.calculateAllMatchups [ g1; g2 ]

    let ((e1, e2), c) = res.Head
    // calculateAllMatchups alphabetically sorts: [initial] < [tuned]
    Assert.Equal("[initial]", e1)
    Assert.Equal("[tuned]", e2)
    // Counts are from [initial]'s perspective: both wins → W2
    Assert.Equal(1, c.W2)
    Assert.Equal(0, c.L2)
    // A consumer treating [tuned] as candidate must swap W↔L:
    // swapped.W2 = c.L2 = 0, swapped.L2 = c.W2 = 1 → candidate lost both
    let swapped = { c with W2 = c.L2; L2 = c.W2; W15 = c.L15; L15 = c.W15 }
    Assert.Equal(0, swapped.W2)
    Assert.Equal(1, swapped.L2)

[<Fact>]
let ``Pentanomial Elo, error and LOS from OpenBench reference data`` () =
    // Reference: Elo 47.31 +-8.51 (95%), Games N:1330 W:423 L:243 D:664, Penta [0, 66, 353, 246, 0]
    let counts =
      { Pentanomial.Counts.Empty with
          L2 = 0; L15 = 66; D = 353; W15 = 246; W2 = 0
          CompletedPairs = 665 }
    let elo, error, los = Pentanomial.pentanomialEloErrorAndLos counts
    Assert.InRange(elo, 47.0, 48.0)       // ~47.3
    Assert.InRange(error, 8.0, 9.0)       // ~8.5 (95% CI, z=1.96)
    Assert.InRange(los, 0.999, 1.001)     // ~100%

[<Fact>]
let ``Trinomial Elo and error from OpenBench reference data`` () =
    // Same reference: Games N:1330 W:423 L:243 D:664
    let elo = EloCalculator.eloDiffWDL 423.0 664.0 243.0
    let error = EloCalculator.calculateEloError 423 664 243
    let los = EloCalculator.calculateLikelihoodOfSuperiority 423 243 (float (423 + 243))
    let cfs = los * 100.0
    Assert.InRange(elo, 47.0, 48.0)       // ~47.3 (same point estimate as pentanomial)
    Assert.InRange(error, 11.0, 12.0)     // ~11.6 (wider than pentanomial's ~8.5, uses z=1.72)
    Assert.InRange(los, 0.999, 1.001)     // ~100%
    Assert.InRange(cfs, 99.9, 100.1)      // CFS ~100%
