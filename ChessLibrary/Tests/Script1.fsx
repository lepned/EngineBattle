open System
open System.IO
open System.Collections.Generic
open System.Text
open System.Text.RegularExpressions
open System.Net.Http
open System.Runtime.Intrinsics.X86
open System.Diagnostics
open Microsoft.FSharp.Control


#r "nuget: MathNet.Numerics.FSharp"
open MathNet.Numerics

//#r "nuget: Microsoft.Extensions.Logging, 8.0.0"
//#r "C:/Users/Navn/.nuget/packages/cliwrap/3.6.6/lib/netcoreapp3.0/CliWrap.dll"
//open Microsoft.Extensions.Logging
//open CliWrap

//#load "..\QBBOperations.fs"
//#load "..\CoreTypes.fs"
//#load "..\MoveGeneration.fs"
//#load "..\LowLevelUtilities.fs"
//#load "..\Parser.fs"
//#load "..\Utilities.fs"
//#load "..\Board.fs"


//-------------------------------------------------------------------------------------
// 1. Update Parameters Function
//    (Your function to update V1TEMP and V2FRAC in the config string)
//-------------------------------------------------------------------------------------
let updateParameters (input: string) (newV1Temp: float) (newV2Frac: float) =
    // Pattern to match V1TEMP and V2FRAC
    let patternV1 = @"(V1TEMP=)([\d\.]+)"
    let patternV2 = @"(V2FRAC=)([\d\.]+)"

    // Replace the V1TEMP value
    let updatedV1 =
        Regex.Replace(input, patternV1, fun (m: Match) ->
            let prefix = m.Groups.[1].Value
            sprintf "%s%g" prefix newV1Temp)

    // Then update the V2FRAC value
    let updatedV2 =
        Regex.Replace(updatedV1, patternV2, fun (m: Match) ->
            let prefix = m.Groups.[1].Value
            sprintf "%s%g" prefix newV2Frac)
    updatedV2

//-------------------------------------------------------------------------------------
// 2. Helper: Extract a Parameter from the Config String
//-------------------------------------------------------------------------------------
let extractParameter (paramName: string) (config: string) : float option =
    let pattern = sprintf @"%s=([\d\.]+)" paramName
    let m = Regex.Match(config, pattern)
    if m.Success then Some (float m.Groups.[1].Value) else None

//-------------------------------------------------------------------------------------
// 3. Dummy Score Function
//    For demonstration, we define the “best” parameters as:
//       V1TEMP = 0.85 and V2FRAC = 0.25
//    The score here is higher (closer to 0) when the parameters are closer to the optimum.
//-------------------------------------------------------------------------------------
let dummyScore (config: string) : float =
    match extractParameter "V1TEMP" config, extractParameter "V2FRAC" config with
    | Some v1, Some v2 ->
        // A simple function: the closer v1 is to 0.85 and v2 to 0.25 the higher the score.
        - (abs (v1 - 0.85) + abs (v2 - 0.25))
    | _ -> -1000.0  // Very low score if something goes wrong

//-------------------------------------------------------------------------------------
// 4. Parameter Optimization Functions
//    These functions will try stepping one parameter at a time (by 'step') until
//    the score does not improve further.
//-------------------------------------------------------------------------------------

/// Optimize V1TEMP by decreasing it by 'step' as long as the score improves.
let rec optimizeV1 (config: string) (currentV1: float) (step: float) (scoreFunc: string -> float) =
    // Try a new value (decrease by step)
    let newV1 = currentV1 - step
    // Keep the current V2 from the configuration
    let currentV2 =
        match extractParameter "V2FRAC" config with 
        | Some v -> v 
        | None -> failwith "V2FRAC not found in config"
    // Build a new config with the updated V1TEMP
    let newConfig = updateParameters config newV1 currentV2
    let currentScore = scoreFunc config
    let newScore = scoreFunc newConfig

    // If the new score is better, continue stepping.
    if newScore > currentScore then
         optimizeV1 newConfig newV1 step scoreFunc
    else
         (currentV1, config)

/// Optimize V2FRAC by decreasing it by 'step' as long as the score improves.
let rec optimizeV2 (config: string) (currentV2: float) (step: float) (scoreFunc: string -> float) =
    let newV2 = currentV2 - step
    let currentV1 =
        match extractParameter "V1TEMP" config with 
        | Some v -> v 
        | None -> failwith "V1TEMP not found in config"
    let newConfig = updateParameters config currentV1 newV2
    let currentScore = scoreFunc config
    let newScore = scoreFunc newConfig

    if newScore > currentScore then
         optimizeV2 newConfig newV2 step scoreFunc
    else
         (currentV2, config)

//-------------------------------------------------------------------------------------
// 5. Coordinate Descent Routine
//    Alternately optimize V1TEMP and V2FRAC until no further improvement is found.
//-------------------------------------------------------------------------------------
let rec coordinateDescent (config: string) (step: float) (scoreFunc: string -> float) =
    let currentScore = scoreFunc config
    // Get the current parameter values.
    let currentV1 = match extractParameter "V1TEMP" config with Some v -> v | None -> failwith "V1TEMP not found"
    let currentV2 = match extractParameter "V2FRAC" config with Some v -> v | None -> failwith "V2FRAC not found"

    // Optimize V1TEMP first.
    let (optV1, configAfterV1) = optimizeV1 config currentV1 step scoreFunc
    // Then optimize V2FRAC, using the config updated by V1TEMP.
    let (optV2, configAfterV2) = optimizeV2 configAfterV1 currentV2 step scoreFunc

    let newScore = scoreFunc configAfterV2

    // If the combined optimization improved the score, repeat the coordinate descent.
    if newScore > currentScore then
         coordinateDescent configAfterV2 step scoreFunc
    else
         configAfterV2

//-------------------------------------------------------------------------------------
// 6. Main: Run the Optimization
//-------------------------------------------------------------------------------------

// The initial configuration string.
let initialConfig = "C:/Dev/Chess/Networks/CeresTrainNet/Official/C1-640-34.value3_L32_ZDeblundered_x1_d2.onnx.value3.onnx|V1TEMP=0.95;V2FRAC=0.3"
// Define the step size (e.g. 0.05)
let step = 0.05

printfn "Initial configuration:\n%s" initialConfig
printfn "Initial score: %g" (dummyScore initialConfig)

// Run coordinate descent to optimize both parameters.
let optimizedConfig = coordinateDescent initialConfig step dummyScore
let optimizedScore = dummyScore optimizedConfig

printfn "\nOptimized configuration:\n%s" optimizedConfig
printfn "Optimized score: %g" optimizedScore

    //0 // Return an integer exit code.


let formatTimeSpan (fixedTime: TimeSpan) (incrementTime: TimeSpan) : string =
    let totalFixedMinutes = fixedTime.TotalMinutes
    let totalFixedSeconds = fixedTime.TotalSeconds
    let totalIncrementSeconds = float incrementTime.Seconds + (float incrementTime.Milliseconds / 1000.0) + float incrementTime.Minutes * 60.0 + float incrementTime.Hours * 3600.0
    let fixedTimePart =
        if totalFixedMinutes >= 1.0 then sprintf "%.0f'" totalFixedMinutes
        else sprintf "%.0f''" totalFixedSeconds    
    let incrementTimePart = if incrementTime.Milliseconds > 0 then sprintf "%.1f''" totalIncrementSeconds else sprintf "%.0f''" totalIncrementSeconds 
    sprintf "%s + %s" fixedTimePart incrementTimePart

// Example usage:
let fixedTime1 = TimeSpan(0, 0, 2)         // 2 seconds
let incrementTime1 = TimeSpan(0, 0, 1)     // 1 second
let result1 = formatTimeSpan fixedTime1 incrementTime1
printfn "%s" result1  // Output: "2'' + 1.0''"

let fixedTime2 = TimeSpan(0, 1, 2)        // 30 minutes, 0 seconds
let incrementTime2 = TimeSpan(0, 0, 0, 1, 100)  // 1 seconds, 100 milliseconds
let result2 = formatTimeSpan fixedTime2 incrementTime2
printfn "%s" result2  // Output: "30' + 2.1''"








let eloDifferenceFromScore score =
    -log (1.0 / (score + 0.000001) - 1.0) * 400.0 / log 10.0

let calculateEloConfidenceInterval wins draws losses confidenceMultiplier =
    let totalGames = float (wins + draws + losses)
    let winningFraction = (float wins + 0.5 * float draws) / totalGames
    let variance = Math.Sqrt((float wins * Math.Pow(1.0 - winningFraction, 2.0) +
                              float losses * Math.Pow(winningFraction, 2.0) +
                              float draws * Math.Pow(0.5 - winningFraction, 2.0)) / totalGames)
    let minFraction = max 0.00001 (winningFraction - confidenceMultiplier * variance / Math.Sqrt(totalGames))
    let maxFraction = min 0.99999 (winningFraction + confidenceMultiplier * variance / Math.Sqrt(totalGames))
    (eloDifferenceFromScore minFraction, eloDifferenceFromScore winningFraction, eloDifferenceFromScore maxFraction)

// Calculate the error based on the Elo confidence interval
let calculateEloError wins draws losses =
    let numberOfGames = float (wins + draws + losses)
    let (minElo, _, maxElo) = calculateEloConfidenceInterval wins draws losses 1.72
    printfn "Error calc: Min Elo: %f, Max Elo: %f" minElo maxElo
    let diff = (maxElo - minElo)
    if diff = 0.0 || Double.IsNaN minElo || Double.IsNaN maxElo then 
      999.0 / numberOfGames
    else
      min 699.0 (diff / 2.0)

// Calculate LOS using the error function
let calculateLikelihoodOfSuperiority wins losses totalGames =
    0.5 + 0.5 * MathNet.Numerics.SpecialFunctions.Erf((float (wins - losses)) / sqrt (2.0 * totalGames))

// Main function to calculate all chess metrics
let displayChessMetrics wins draws losses  =
    let totalGames = float (wins + draws + losses)
    printfn "Number of games: %g" totalGames
    let (_, eloDiff, _) = calculateEloConfidenceInterval wins draws losses 1.72
    printfn "Elo difference: %+g" eloDiff
    let los = calculateLikelihoodOfSuperiority wins losses totalGames
    printfn "LOS: %g" los
    let error = calculateEloError wins draws losses
    printfn "Error: %g" error

// Example usage:
displayChessMetrics 0 1 1


let a = -2
let b = -1
let diff = abs (a - b)

// Displaying positive infinity symbol
printfn "%A" Double.PositiveInfinity // Outputs "Infinity"

// Displaying negative infinity symbol
printfn "%A" Double.NegativeInfinity // Outputs "-Infinity"

// Displaying positive infinity symbol
printfn "%s" "\u221E" // Outputs "∞"

// Displaying negative infinity symbol
printfn "%s" "-\u221E" // Outputs "-∞"

let positiveInfinitySymbol = "\u221E" // Positive infinity symbol
let negativeInfinitySymbol = "-\u221E" // Negative infinity symbol


// Define the paths to your files
let fileToAppendPath = "fileToAppend.txt"
let targetFilePath = "file2.txt"

let appendContentWithNewLine targetFile appendFile =
    let targetContent = File.ReadAllText(targetFile)    
    let contentToAppend = File.ReadAllText(appendFile)
    // If the target file does not end with a newline, add one before the content to append
    if not (targetContent.EndsWith("\n") || targetContent.EndsWith("\r\n")) then       
        File.AppendAllText(targetFile, Environment.NewLine + contentToAppend)
    else       
        File.AppendAllText(targetFile, contentToAppend)

// Invoke the function with the specified file paths
//appendContentWithNewLine targetFilePath fileToAppendPath

sprintf "setoption name %s value %s" "FpuStrategy" "absolute"

let calculateStandardDeviation (eloDifferences: int list) =
    // Calculate mean
    let meanDiff = List.averageBy float eloDifferences

    // Calculate sum of squares
    let sumOfSquares = 
        eloDifferences 
        |> List.map (fun diff -> (float diff - meanDiff) ** 2.0)
        |> List.sum

    // Calculate variance
    let variance = sumOfSquares / float (List.length eloDifferences)

    // Calculate standard deviation
    let stdDev = Math.Sqrt(variance)

    stdDev

// Example usage
//let eloDifferences = [10; -5; 20; -15; 5]
let eloDifferences = [-20; -25; -35; -45]
let stdDev = calculateStandardDeviation eloDifferences
printfn "Standard Deviation of Elo Differences: %f" stdDev


let eloDiff winPercentage =
    let eloDiff = -400.0 * Math.Log10(1.0 / winPercentage - 1.0)
    if eloDiff = -0 then 0. else eloDiff


let eloConfidenceInterval (wins:int) (draws:int) (losses:int) stdDev =    
    let count = float (wins + draws + losses)
    let score = (float wins + 0.5 * float draws) / count
    let stdev = Math.Sqrt((float wins * Math.Pow(1.0 - score, 2.0) +
                            float losses * Math.Pow(score, 2.0) +
                            float draws * Math.Pow(0.5 - score, 2.0)) / count)
    let min = score - stdDev * stdev / Math.Sqrt(count)
    let max = score + stdDev * stdev / Math.Sqrt(count)
    (eloDiff min, eloDiff score, eloDiff max)

let error wins draws losses =
  let (min, _, max) = eloConfidenceInterval wins draws losses 1.96
  let error = (max - min) / 2.0
  error

let likelihoodSuperiorityOld wins draws losses =
    let error = error wins draws losses
    let wins, draws, losses = float wins, float draws, float losses
    let adjustedWins = wins + 0.5 * draws
    let adjustedLosses = losses + 0.5 * draws
    let totalGames = float (wins + losses + draws)
    let x = (adjustedWins - adjustedLosses) / Math.Sqrt(2.0 * totalGames + error**2.0)
    let erf = SpecialFunctions.Erf x
    printfn "x: %f, erf: %f" x erf
    0.5 * (1.0 + erf)

likelihoodSuperiorityOld 1 1 2

//let likelihoodSuperiorityNew wins draws losses =
//    //let adjustedWins = float (wins + 0.5 * draws)
//    //let adjustedLosses = float (losses + 0.5 * draws)
//    //let totalGames = float (wins + losses + draws)
//    //let error = error wins draws losses
//    let x = (wins - losses) / Math.Sqrt(2.0 * wins + 2.0 * losses)
//    let erf = SpecialFunctions.Erf x  
//    0.5 * (1.0 + erf)


// Define a type to represent participants
type Participant = string

// Function to simulate a match (for demonstration purposes)
let simulateMatch (participant1: Participant) (participant2: Participant) =
    // For simplicity, let's assume participant1 always wins
    participant1

// Function to set up a knockout tournament
let setupKnockoutTournament (participants: Participant seq) =
    let participants = participants |> Seq.toArray
    // Create a mutable list to store the pairings
    let pairings = ResizeArray<_>()
    // Loop through the participants in pairs
    for i in 0 .. 2 .. participants.Length - 1 do
        let participant1 = participants.[i]
        let participant2 = participants.[i + 1]
        // Simulate the match and add the winner to the pairings list
        pairings.Add(simulateMatch participant1 participant2)
    pairings

// simulate the whole tournament and return the winners
let simulateKnockoutTournament (participants: Participant list) =
    let rec simulateRound (participants: Participant seq) =
        // Base case: if there is only one participant, return it as the winner
        if participants |> Seq.length = 1 then
            participants
        else
            // Set up the pairings for the current round
            let pairings = setupKnockoutTournament participants
            // Recursively simulate the next round with the winners
            let playersInTourny = simulateRound pairings
            let pl = playersInTourny|> Seq.toList
            printfn "Players: %A" pl
            simulateRound playersInTourny
    // Start the simulation with the initial list of participants
    simulateRound participants

// Example usage
let participants = ["Player1"; "Player2"; "Player3"; "Player4"; "Player5"; "Player6"; "Player7"; "Player8"]
let pairings = setupKnockoutTournament participants
printfn "%A" pairings
let tourny = simulateKnockoutTournament participants
printfn "%A" tourny


type ReplayData = { Move: string; TimeLeftInMs: int }

type ReferenceGameReplay() = 
    inherit Dictionary<uint64, ReplayData>()

    member this.TryGet (hash) = 
        let mutable data = Unchecked.defaultof<ReplayData>
        if this.TryGetValue(hash, &data) then Some data
        else None

    member this.Seed (initialData: seq<uint64 * ReplayData>) =
        for (key, value) in initialData do
            this.Add(key, value)


let prettyPrintTimeSpan (timeSpan: TimeSpan) =
    let hours = timeSpan.Hours
    let minutes = timeSpan.Minutes
    let seconds = timeSpan.Seconds
    sprintf "%02dh %02dm %02ds" hours minutes seconds

// Usage example
let exampleTimeSpan = TimeSpan.FromHours(0.5) // 1 hour and 30 minutes
let prettyTime = prettyPrintTimeSpan exampleTimeSpan
printfn "%s" prettyTime


let extractFEN (inputString : string) =
    let prefix = "position fen "
    let startIndex = inputString.IndexOf(prefix) + prefix.Length
    if startIndex >= 0 && startIndex < inputString.Length then
        inputString.Substring(startIndex) |> Some
    else
        None

// Usage example
let fenT1 = extractFEN "position fen r1b2rk1/ppp1npp1/7p/P2q2NQ/8/3B4/P4PPP/bNBR2K1 b - - 1 14"
match fenT1 with 
|Some fen -> printfn "%s" fen 
|None -> printfn "No FEN found"


let input = "rbqknnbr/pppppppp/8/8/8/8/PPPPPPPP/QBNRBNKR w KQkq - 0 1 moves d2d4 d7d5 e2e4 g8f6"
let input1 = "rbqknnbr/pppppppp/8/8/8/8/PPPPPPPP/QBNRBNKR w KQkq - 0 1 moves"

let parseFENandMoves (fenMoves : string) =
    let parts = fenMoves.Split ([| "moves" |], System.StringSplitOptions.RemoveEmptyEntries)
    let fen = parts.[0].Trim()
    let moves = if parts.Length > 1 then parts.[1].Trim().Split ' ' else [||]
    (fen, moves)

// Usage Example
let fen, moves = parseFENandMoves input
printfn "FEN: %s and moves: %A" fen moves
let fen1, moves1 = parseFENandMoves input1
printfn "FEN: %s and moves: %A" fen1 moves1



let number = 1UL <<< 2
let reversed (binary:string) = System.String(Array.rev (binary.ToCharArray()))

//print binary representation
let printBinary (number : uint64) = Convert.ToString(int64 number, 2) |> reversed |> printfn "%s"

printBinary number

//test perft chess960 positions
let path = "C:/Dev/Chess/chess960.txt"
type Chess960Record = {
    PositionNumber : int
    FEN : string
    Depth1 : int64
    Depth2 : int64
    Depth3 : int64
    Depth4 : int64
    Depth5 : int64
    Depth6 : int64
}

let parseChess960Record (input: string) =
    printfn "%s" input
    let parts = input.Split('\t')
    let positionNumber = int parts.[0]
    let fen = parts.[1]
    let depth1 = int64 parts.[2]
    let depth2 = int64 parts.[3]
    let depth3 = int64 parts.[4]
    let depth4 = int64 parts.[5]
    let depth5 = int64 parts.[6]
    let depth6 = int64 parts.[7]
    { PositionNumber = positionNumber; FEN = fen; Depth1 = depth1; Depth2 = depth2; Depth3 = depth3; Depth4 = depth4; Depth5 = depth5; Depth6 = depth6 }

let parseFile (filePath : string) =
    let lines = File.ReadAllLines(filePath) |> Array.skip 1
    let records = lines |> Array.map parseChess960Record
    records

let records = parseFile path

// Define each file mask as a constant
let [<Literal>] AFile = 0x101010101010100UL
let [<Literal>] BFile = 0x202020202020200UL
let [<Literal>] CFile = 0x404040404040400UL
let [<Literal>] DFile = 0x808080808080800UL
let [<Literal>] EFile = 0x1010101010101000UL
let [<Literal>] FFile = 0x2020202020202000UL
let [<Literal>] GFile = 0x4040404040404000UL
let [<Literal>] HFile = 0x8080808080808000UL
let FileMasks = [| AFile; BFile; CFile; DFile; EFile; FFile; GFile; HFile |]

let BishopAttacks = 
  [|0x8040201008040200UL;0x0080402010080500UL;0x0000804020110A00UL;0x0000008041221400UL;
    0x0000000182442800UL;0x0000010204885000UL;0x000102040810A000UL;0x102040810204000UL  |]

//bitboards
let inline ExtractLSB(bb:uint64) = Bmi1.X64.ExtractLowestSetBit(bb)
let inline MSB(bb:uint64) = 63UL ^^^ (uint64)(System.Numerics.BitOperations.LeadingZeroCount(bb))

let KingRowOneMasks = Array2D.init 8 8 (fun fromSq toSq -> 
  //set the bit for the fromSq and the toSq
  let mask = 1UL <<< fromSq ||| 1UL <<< toSq
  mask)

//create a function that returns the King mask for a given fromSq and toSq
let getKingRowOneMask fromSq toSq = KingRowOneMasks.[fromSq, toSq]

//test getkingrowonemask
let kingRowOneMask = getKingRowOneMask 0 1 //should be 3UL
let kingRowOneMask2 = getKingRowOneMask 0 2 //should be 5UL
let kingRowOneMask3 = getKingRowOneMask 0 3 //should be 9UL
let kingRowOneMask4 = getKingRowOneMask 2 4 //should be 20UL
let kingRowOneMask5 = getKingRowOneMask 4 2 //should be 20UL

let rank (square: int) = square / 8
let file (square: int) = square % 8

let diagonalMask (square: int) : uint64 =
    let baseDiagonal = 0x8040201008040201UL  // A1 to H8 diagonal
    let r = rank square
    let f = file square
    let diagonalShift = f - r
    if diagonalShift > 0 then
        baseDiagonal >>> (diagonalShift * 8)
    elif diagonalShift < 0 then
        baseDiagonal <<< (-diagonalShift * 8)
    else
        baseDiagonal

let antiDiagonalMask (square: int) : uint64 =
    let baseAntiDiagonal = 0x0102040810204080UL  // A8 to H1 anti-diagonal
    let r = rank square
    let f = file square
    let antiDiagonalSum = r + f
    if antiDiagonalSum > 7 then
        baseAntiDiagonal >>> ((antiDiagonalSum - 7) * 8)
    elif antiDiagonalSum < 7 then
        baseAntiDiagonal <<< ((7 - antiDiagonalSum) * 8)
    else
        baseAntiDiagonal
let square = 1// Your square index (0 to 63)
let mask = diagonalMask square
let antiDiagMask = antiDiagonalMask square

//create anti diagonal masks for each square on the first rank and put it in a list hardcoded

//let antiDiagonalMasks = [ 0UL; 258UL; 66052UL; 16909320UL;    ]


//let diagonalMasks = [ for i in 0 .. 7 -> diagonalMask i ]
//let antiDiagonalMasks = [ for i in 0 .. 7 -> antiDiagonalMask i ]



let extractLSBsFromFiles bitboard = 
  FileMasks
  |> Array.map (fun fileMask -> ExtractLSB (bitboard &&& fileMask)) 
  |> Array.fold (fun acc lsb -> acc ||| lsb) 0UL

let setBitsInRange (fromSquare: int) (toSquare: int) : uint64 =
    let lowerSquare = min fromSquare toSquare
    let upperSquare = max fromSquare toSquare
    let mask = Seq.fold (fun acc bit -> acc ||| (1UL <<< bit)) 0UL [lowerSquare..upperSquare]
    mask

//create bishop masks for each square
let createBishopMask startSq finalSq =
  let mutable mask = 0UL
  let mutable currentSq = startSq
  let dir = if startSq > finalSq then -1 else 1
  while currentSq <> finalSq do
    mask <- mask ||| BishopAttacks.[currentSq]
    currentSq <- currentSq + dir
  mask <- mask ||| BishopAttacks.[currentSq]
  mask

//pre compute a 2D array with bishop masks where fromSq is one index and toSq the second index
let BishopMasks = Array2D.init 8 8 (fun fromSq toSq -> createBishopMask fromSq toSq) 

//create a function that returns the Bishop mask for a given fromSq and toSq
//let getBishopMask fromSq toSq = BishopMasks.[fromSq, toSq]

let getBishopMaskWithOccupancy fromSq toSq occupancy = 
  (BishopMasks.[fromSq, toSq] &&& occupancy) |> extractLSBsFromFiles


//create rook masks for each square
let createRookMaskOld startSq finalSq =
  let mutable mask = 0UL
  let mutable currentSq = startSq
  let dir = if startSq > finalSq then -1 else 1
  while currentSq <> finalSq do
    mask <-  mask ||| ExtractLSB(FileMasks.[currentSq])
    currentSq <- currentSq + dir
  mask <- mask ||| ExtractLSB(FileMasks.[currentSq])
  let dir = if startSq > finalSq then 1 else -1
  currentSq <- startSq + dir
  if dir = 1 then
    mask <- mask ||| ExtractLSB(setBitsInRange currentSq 7)
  else
    mask <- mask ||| ExtractLSB(setBitsInRange 0 currentSq)
  mask

//create rook masks for each square
let createRookMask startSq finalSq =
  let mutable mask = 0UL
  let mutable currentSq = startSq
  let dir = if startSq > finalSq then -1 else 1
  while currentSq <> finalSq do
    mask <-  mask ||| FileMasks.[currentSq]
    currentSq <- currentSq + dir
  mask <- mask ||| FileMasks.[currentSq]
  let dir = if startSq > finalSq then 1 else -1
  currentSq <- startSq + dir
  if dir = 1 then
    mask <- mask ||| setBitsInRange currentSq 7
  else
    mask <- mask ||| setBitsInRange 0 currentSq
  mask

//pre compute a 2D array with rook masks where fromSq is one index and toSq the second index
let RookMasks = Array2D.init 8 8 (fun fromSq toSq -> createRookMask fromSq toSq)

let getRookMask fromSq toSq = RookMasks.[fromSq, toSq]


//create a function that returns the Rook mask for a given fromSq and toSq
let getRookMaskWithOccupancy fromSq toSq occupancy = 
  (RookMasks.[fromSq, toSq] &&& occupancy) |> extractLSBsFromFiles

//test of bitboard functions

let rookMask1 = getRookMaskWithOccupancy 1 2 8589934592UL
let rookMask2 = getRookMaskWithOccupancy 1 2 562958543486976UL
let rookMask3 = getRookMaskWithOccupancy 1 5 2858842304152576UL
let rookMask4 = getRookMaskWithOccupancy 1 6 2858842304152576UL
let rookMask5 = getRookMaskWithOccupancy 1 7 2858842304152576UL

let bishopMask1 = getBishopMaskWithOccupancy 4 6 1165499572716544UL


//assuming 4 games per round in a gauntlet tournament
let transformInput (input: string) =
    let parts = input.Split('.')
    if parts.Length = 2 then
        let major = int parts.[0]
        let minor = int parts.[1]

        // Calculate the new major and minor numbers
        let newMajor = (major - 1) / 4 + 1
        let newMinor = ((major - 1) % 4) + 1

        sprintf "%d.%d" newMajor newMinor
    else
        "Invalid input"

let result = transformInput "8.1"
result



let generateAllChess960Positions () =
    let positions = 
        [ for whiteBishop in 0 .. 2 .. 7 do
            for blackBishop in 1 .. 2 .. 7 do
              let remainingSquares = [ for i in 0 .. 7 do if i <> whiteBishop && i <> blackBishop then yield i ]
              for knights in Seq.choose (fun (a, b) -> if a < b then Some (a, b) else None) (Seq.allPairs remainingSquares remainingSquares) do
                let squaresForRooksKing = List.filter (fun i -> not (List.contains i [fst knights; snd knights])) remainingSquares
                for rook1 in squaresForRooksKing do
                  for rook2 in squaresForRooksKing do
                    if rook1 < rook2 then
                      for king in squaresForRooksKing do
                        if rook1 < king && king < rook2 then
                            let mutable position = Array.create 8 ""
                            position.[whiteBishop] <- "B"
                            position.[blackBishop] <- "B"
                            position.[fst knights] <- "N"
                            position.[snd knights] <- "N"
                            position.[rook1] <- "R"
                            position.[rook2] <- "R"
                            position.[king] <- "K"
                            let queenPosition = position |> Array.findIndex (fun x -> x = "")
                            position.[queenPosition] <- "Q"
                            yield String.concat "" position ]
    positions

// Generate all 960 positions
let allChess960Positions = generateAllChess960Positions ()
printfn "Total positions generated: %d" (List.length allChess960Positions)

let isKingBetweenRooks (position: string) =
    let rooksIndices = [ for i in 0 .. position.Length - 1 do if position.[i] = 'R' then yield i ]
    let kingIndex = position.IndexOf('K')
    rooksIndices.[0] < kingIndex && kingIndex < rooksIndices.[1]

let areBishopsOnDifferentColors (position: string) =
    let bishopsIndices = [ for i in 0 .. position.Length - 1 do if position.[i] = 'B' then yield i ]
    bishopsIndices.[0] % 2 <> bishopsIndices.[1] % 2

// Example usage with a test function
let testPositions () =
    let positions = generateAllChess960Positions ()
    positions |> List.forall (fun p -> isKingBetweenRooks p && areBishopsOnDifferentColors p)

// Running the test
let allPositionsValid = testPositions ()
printfn "All positions have the king between the rooks and bishops on different colored squares: %b" allPositionsValid


type FinalResult = 
    | WhiteWins
    | Draw
    | BlackWins
    | Unknown

type FenRecord = {
    CeresNetPrediction: FinalResult
    TablebaseCorrectAnswer: FinalResult
    Fen: string
}

let parseFinalResult (input: string) =
    match input with
    | "1" -> WhiteWins
    | "0" -> Draw
    | "-1" -> BlackWins
    | _ -> Unknown

let extractFen (input: string) =
    let parts = input.Split(' ', StringSplitOptions.RemoveEmptyEntries)    
    if parts.Length < 4 then None
    else 
        let ceresNetPrediction = parseFinalResult parts.[0]
        let tablebaseCorrectAnswer = parseFinalResult parts.[1]
        let rest = String.concat " " parts[2..]
        Some { CeresNetPrediction = ceresNetPrediction; TablebaseCorrectAnswer = tablebaseCorrectAnswer; Fen = rest.Trim() }

let examples = [
    "1 0  7K/P7/8/4k1P1/8/8/3p4/8 w - - 0 0"
    "-1 0  8/4k3/p7/7K/3P4/8/7P/8 w - - 0 0"
    "0 -1  8/3K4/8/2P4k/7P/8/4p3/8 w - - 0 0"
    "2 0  8/3K4/8/2P4k/7P/8/4p3/8 w - - 0 0"
]

examples |> List.iter (fun example ->
    match extractFen example with
    | Some fenRecord -> 
        printfn "CeresNet Prediction: %A, Tablebase Correct Answer: %A, FEN: %s" 
            fenRecord.CeresNetPrediction fenRecord.TablebaseCorrectAnswer fenRecord.Fen
    | None -> 
        printfn "Invalid input or FEN not found"
)




//let extractFen (input: string) =
//    let parts = input.Split(' ', StringSplitOptions.RemoveEmptyEntries)
    
//    // Find the index of the chess position part
//    let positionIndex = parts |> Array.findIndex (fun part -> part.Contains("/"))
    
//    if positionIndex >= 0 then
//        // Extract the chess position and the rest of the FEN fields
//        let fenParts = parts.[positionIndex..(positionIndex + 5)]
//        Some(String.Join(" ", fenParts))
//    else
//        None

//let examples = [
//    "1 0  7K/P7/8/4k1P1/8/8/3p4/8 w - - 0 0"
//    "-1 0  8/4k3/p7/7K/3P4/8/7P/8 w - - 0 0"
//    "0 -1  8/3K4/8/2P4k/7P/8/4p3/8 w - - 0 0"
//]

//examples |> List.iter (fun example ->
//    match extractFen example with
//    | Some fen -> printfn "%s" fen
//    | None -> printfn "No FEN found"
//)


let baseUrl = "https://tablebase.lichess.ovh/tables/standard/7/4v3_pawnful/"

// List of files to download
let filesToDownload = [
    "KBPPvKBP.rtbw"
    "KBPPvKBP.rtbz"
    "KBPPvKNP.rtbw"
    "KBPPvKNP.rtbz"
    "KBPPvKRP.rtbw"
    "KBPPvKRP.rtbz"
    "KNPPvKBP.rtbw"
    "KNPPvKBP.rtbz"
    "KNPPvKNP.rtbw"
    "KNPPvKNP.rtbz"
    "KPPPvKPP.rtbw"
    "KPPPvKPP.rtbz"
    "KQPPvKQP.rtbw"
    "KQPPvKQP.rtbz"
    "KRPPvKRP.rtbw"
    "KRPPvKRP.rtbz"
]

let downloadFile (fileName: string) (destinationDir: string) =
    async {
        let url = baseUrl + fileName
        let destinationPath = Path.Combine(destinationDir, fileName)
        use client = new HttpClient()      
        use! response = client.GetAsync(url, HttpCompletionOption.ResponseHeadersRead) |> Async.AwaitTask
        if response.IsSuccessStatusCode then
            use! contentStream = response.Content.ReadAsStreamAsync() |> Async.AwaitTask
            use fileStream = new FileStream(destinationPath, FileMode.Create)
            do! contentStream.CopyToAsync(fileStream) |> Async.AwaitTask
            printfn "Downloaded %s" fileName
        else
            printfn "Failed to download %s" fileName        
    }

// Local directory where the files should be saved
let destinationDir = "F:/7Men"

// Ensure the directory exists
if not (Directory.Exists(destinationDir)) then
    Directory.CreateDirectory(destinationDir) |> ignore


let downloadFilesParallel (files: string list) (destinationDir: string) =
    files
    |> List.map (fun file -> async { 
        let! data = downloadFile file destinationDir
        return data
    })
    |> Async.Parallel
    |> Async.RunSynchronously

// Download all files
//filesToDownload
//|> List.iter (fun file ->
//    downloadFile file destinationDir |> Async.RunSynchronously
//    printfn "Downloaded %s" file
//)

downloadFilesParallel filesToDownload destinationDir

printfn "All files downloaded successfully!"



type EPDDetails = {
    RawInput: string
    Fen: string
    BestMove: string option
    AvoidMove: string option
    Id: string option
}


let extractDetails (input: string) = 
    // Extract FEN part
    let fenRegex = Regex(@"^([rnbqkpRNBQKP1-8]+(\/[rnbqkpRNBQKP1-8]+){7}\s[bw]\s(-|[KQkq]{1,4})\s(-|[a-h][36]))")

    let fen = 
      let fenMatch = fenRegex.Match(input)
      if fenMatch.Success then fenMatch.Groups.[1].Value else ""

    // Extract BestMove part
    let bestMoveRegex = Regex(@"bm ([\w\d\+]+);")
    let bestMoveMatch = bestMoveRegex.Match(input)
    let bestMove = if bestMoveMatch.Success then Some bestMoveMatch.Groups.[1].Value else None

    // Extract AvoidMove part
    let avoidMoveRegex = Regex(@"am ([\w\d\+]+);")
    let avoidMoveMatch = avoidMoveRegex.Match(input)
    let avoidMove = if avoidMoveMatch.Success then Some avoidMoveMatch.Groups.[1].Value else None

    // Extract Id part
    let idRegex = Regex(@"id ""(.*?)"";")
    let idMatch = idRegex.Match(input)
    let id = if idMatch.Success then Some idMatch.Groups.[1].Value else None

    { RawInput = input; Fen = fen; BestMove = bestMove; AvoidMove = avoidMove; Id = id }


//let parseEpd input = 
//    let epdRegexPattern = @"^(?<fen>([rnbqkpRNBQKP1-8]+/){7}[rnbqkpRNBQKP1-8]+\s[bw]\s[KQkq-]+\s[a-h1-8-]+(\s\d+)?(\s\d+)?)"


//    let isMatch = Regex.Match(input, epdRegexPattern)
//    if isMatch.Success then 
//        let fen = isMatch.Groups.["fen"].Value.Trim()
//        let fenParts = fen.Split(' ')
//        let adjustedFen = if fenParts.Length < 6 then fen + " 0 1" else fen
//        Some { 
//            RawInput = input
//            Fen = fen //adjustedFen
//            BestMove = if isMatch.Groups.["bestmove"].Success then Some isMatch.Groups.["bestmove"].Value else None
//            Id = if isMatch.Groups.["id"].Success then Some isMatch.Groups.["id"].Value else None 
//        }
//    else
//        None


//let testString = """r1b1r1k1/1pqn1pbp/p2pp1p1/P7/1n1NPP1Q/2NBBR2/1PP3PP/R6K w - - bm f5; id "ERET 003 - Linienoeffnen";"""
//let result = extractDetails testString
//printfn "%A" result

//let parseEpd1 input =
//    let pattern = @"^(?<fen>[^;]+?)(?: bm (?<bestmove>\w+))?(?:;\s*id ""(?<id>.*?))?;$" //@"^(?<fen>.*?(?:\d \d)?)(?: bm (?<bestmove>\w+))?;$"  // @"^(?<fen>[^;]+?) (bm (?<bestmove>\w+);)?$"
//    let isMatch = Regex.Match(input, pattern)
    
//    if isMatch.Success then
//        {
//            RawInput = input
//            Fen = isMatch.Groups.["fen"].Value.Trim()
//            BestMove = 
//                if isMatch.Groups.["bestmove"].Success then
//                    Some isMatch.Groups.["bestmove"].Value
//                else
//                    None
//            Id = 
//                if isMatch.Groups.["id"].Success then
//                    Some isMatch.Groups.["id"].Value
//                else
//                    None
//        }
//    else
//        failwith "Invalid EPD format"

// Test
//let input1 = """5qk1/pr3p2/1pn3p1/2p1P1Bp/2Qr3P/6P1/PP3PB1/4R1K1 w - - bm Qb3;"""
//let input2 = """5qk1/pr3p2/1pn3p1/2p1P1Bp/2Qr3P/6P1/PP3PB1/4R1K1 w - - 1 29;"""
//let input3 = """4q1k1/p1r1Pp2/1pr3p1/2p3Bp/7P/1Q4P1/PP3P2/4R1K1 w - - 1 33 bm Qd5;"""
//let input4 = """1r1qr1k1/pb1pbp2/1pn1n1p1/2pNP2p/Q6P/5NP1/PP3PB1/R1BR2K1 w - - 2 20 bm Be3;"""
//let input5 = """2rq1k2/1b1n3r/4p1p1/1p1pPp2/pPpP1PQP/P1P1NBK1/8/4RR2 w - f6 bm exf6;"""
//let input6 = """5n2/p7/5p2/bP1p1k1p/3P1p1P/3K4/2N2PP1/6B1 b - - bm Ng6;"""
//let input7 = """7r/1p3k2/n1p3p1/Pp1p1p2/1P1q4/5P1P/1R5K/4Q1R1 w - - bm Re2;"""
//let input8 = """7r/1p3k2/n1pq2p1/Pp1p4/5p2/5P1P/4RQ1K/6R1 w - - bm Rge1;"""
//let input9 = """r2b1rk1/3q1p1n/1p1p3p/p1pPpnp1/P1P5/1B1P1N1P/1P1B1PP1/R1Q1R1K1 b - - bm Ng7;"""
//let input10 = """r1bqk1r1/1p1p1n2/p1n2pN1/2p1b2Q/2P1Pp2/1PN5/PB4PP/R4RK1 w q - - bm Rxf4; id "ERET 001 - Entlastung";"""
//let input11 = """r1n2N1k/2n2K1p/3pp3/5Pp1/b5R1/8/1PPP4/8 w - - bm Ng6; id "ERET 002 - Zugzwang";"""

let challengingEpdInputs = [
    """rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq -;""";
    """rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq e3 0 1;""";
    """2r1r1k1/pp1bppbp/6p1/q7/8/1B3N2/PPP2PPP/R2Q1RK1 w - - bm Nxe5; id "Tricky Position 1";""";
    """rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w - -;""";
    """rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w KQkq e6;""";
    """8/8/8/8/4k3/8/3K4/8 w - -;""";
    """2k5/8/2N2R2/2n3q1/8/3P4/PPP2PPP/4K3 b - - bm Qe3+; id "Queen's Entry";""";
    """6k1/pp3ppp/8/2pP4/2P1p3/4P3/PP3PPP/6K1 w - - bm d6; id "Pawn Break";""";
    """n1n5/1n6/n7/8/8/N7/N1N5/8 w - -;""";
    """r2q1rk1/pp2bppp/2nppn2/2p5/4PB2/2NP1N1P/PPPQ1PP1/2KR3R w - - bm Ng5; id "Knight's Jump";"""
]


let inputs = [
    """5qk1/pr3p2/1pn3p1/2p1P1Bp/2Qr3P/6P1/PP3PB1/4R1K1 w - - bm Qb3;"""
    """5qk1/pr3p2/1pn3p1/2p1P1Bp/2Qr3P/6P1/PP3PB1/4R1K1 w - - 1 29;"""
    """4q1k1/p1r1Pp2/1pr3p1/2p3Bp/7P/1Q4P1/PP3P2/4R1K1 w - - 1 33 bm Qd5;"""
    """1r1qr1k1/pb1pbp2/1pn1n1p1/2pNP2p/Q6P/5NP1/PP3PB1/R1BR2K1 w - - 2 20 bm Be3;"""
    """2rq1k2/1b1n3r/4p1p1/1p1pPp2/pPpP1PQP/P1P1NBK1/8/4RR2 w - f6 bm exf6;"""
    """5n2/p7/5p2/bP1p1k1p/3P1p1P/3K4/2N2PP1/6B1 b - - bm Ng6;"""
    """7r/1p3k2/n1p3p1/Pp1p1p2/1P1q4/5P1P/1R5K/4Q1R1 w - - bm Re2;"""
    """7r/1p3k2/n1pq2p1/Pp1p4/5p2/5P1P/4RQ1K/6R1 w - - bm Rge1;"""
    """r2b1rk1/3q1p1n/1p1p3p/p1pPpnp1/P1P5/1B1P1N1P/1P1B1PP1/R1Q1R1K1 b - - bm Ng7;"""
    """r1bqk1r1/1p1p1n2/p1n2pN1/2p1b2Q/2P1Pp2/1PN5/PB4PP/R4RK1 w q - - bm Rxf4; id "ERET 001 - Entlastung";"""
    """r1n2N1k/2n2K1p/3pp3/5Pp1/b5R1/8/1PPP4/8 w - - bm Ng6; id "ERET 002 - Zugzwang";"""
    """3r2k1/6q1/1bb1p3/p1p1PrPp/PpP2B1R/1N1PQ3/2P1K3/5R2 b - - 3 89 am Qg6; id "Blunderbase p.1 - Middlegame";"""
]

let results = inputs |> List.map extractDetails


results |> List.iter (printfn "%A")


let appendFilesToMaster (folderPath: string) (masterFilePath: string) =
    // Get all file paths in the folder
    let filePaths = Directory.GetFiles(folderPath)

    // Create or open the master file for writing
    use masterFileWriter = new StreamWriter(masterFilePath, true) // true means append mode

    // Iterate over each file path and append its contents to the master file
    for filePath in filePaths do
        use fileReader = new StreamReader(filePath)
        while not fileReader.EndOfStream do
            let line = fileReader.ReadLine()
            masterFileWriter.WriteLine(line)

// Usage
let folderPath = @"C:/Dev/Chess/Ordo/PGNs"
let masterFilePath = @"C:/Dev/Chess/Ordo/PGNs/master.pgn"
appendFilesToMaster folderPath masterFilePath


//move deviations

let mainGame = "1.e4 e5 2.d4 exd4 3.Qxd4 Nc6 4.Qe3 g6 5.Bd2 Bg7 6.Na3 Nge7 *"
let otherGames = [
    "1.e4 e5 2.d4 exd4 3.Qxd4 Nc6 4.Qe3 g6 5.Bd2 Bg7 6.Na3 Nge7 *";
    "1.e4 e5 2.d4 exd4 3.Qxd4 Nc6 4.Qe3 g6 5.Bd2 Bg7 6.Nc3 Nge7 *";
    "1.e4 e5 2.d4 exd4 3.Qxd4 Nc6 4.Qe3 g6 5.Bd2 Bg7 6.Nf3 Nge7 *";
    "1.e4 e5 2.d4 exd4 3.Qxd4 Nc6 4.Qe3 Bg7 5.Bd2 g6 6.Na3 Nge7 *";
    "1.e4 e5 2.d4 exd4 3.Qxd4 Nc6 4.Qe4 g6 5.Bd2 Bg7 6.Na3 Nge7 *"
]

let compareGames (pgn1:string) (pgn2:string) =
    let moves1 = pgn1.Split(' ')
    let moves2 = pgn2.Split(' ')

    let minLength = min moves1.Length moves2.Length

    let foundDeviation = 
        [0 .. minLength - 1]
        |> List.tryFind (fun i -> moves1.[i] <> moves2.[i])

    match foundDeviation with
    | Some i ->
        let moveNumber = (i / 2) + 1
        let player = if i % 2 = 0 then 'W' else 'B'
        printfn "Deviation found at move %d by %A. %A != %A" moveNumber player moves1.[i] moves2.[i]
    | None ->
        printfn "No deviation found."

otherGames |> List.iter (compareGames mainGame)


//PV agreements
let countAgreements wArr bArr =
  let zipped = Seq.zip wArr bArr
  let count = zipped |> Seq.takeWhile (fun (a,b) -> a = b)
  count |> Seq.length

let calcPVagreement (white:string) (black:string) (toPlay:string) =
  let wArr = white.Split ' '
  let bArr = black.Split ' '
  printfn "%A" wArr
  printfn "%A" bArr
  if toPlay = "white" then
    let bArr = bArr[2..]
    countAgreements wArr bArr
  else
    let wArr = wArr[1..]
    let bArr = bArr[1..]
    countAgreements wArr bArr

//tests
let whitePV = "2.c4 e6 3.Nf3 Nf6 4.Nc3"
let blackPV = "1... d5 2.c4 e6 3.Nf3 Nf6 4.Nc3"
let testWhite = calcPVagreement whitePV blackPV "white"
let whitePV1 = "1.d4 d5 2.c4 e6 3.Nf3 Nf6"
let blackPV1 = "1... d5 2.c4 e6 3.Nf3"
let testBlack = calcPVagreement whitePV1 blackPV1 "black"



type Pairing = {Opening:string; White: string; Black: string }

// A type to represent a player with a seed number and a name
type Player = { Seed: int; Name: string }

// A type to represent a match between two players
type Match = { Player1: Player; Player2: Player }

// A type to represent a node in the knock-out tournament tree
type Node =
  | Leaf of Player // A leaf node represents a single player
  | Branch of Node * Node * Match // A branch node represents a match between two child nodes and the result of the match

// Function to create a knockout tournament tree from a list of players
let rec createTournamentTree (players: Player list) =
    match players with
    | [] -> invalidArg "players" "The list of players should not be empty."
    | [player] -> Leaf player // If there's only one player, return a leaf node
    | _ ->
        let sortedPlayers = players |> List.sortBy(fun p -> p.Seed) // Sort players by seed
        let halfLength = List.length sortedPlayers / 2
        let leftPlayers = List.take halfLength sortedPlayers
        let rightPlayers = List.skip halfLength sortedPlayers
        let leftTree = createTournamentTree leftPlayers
        let rightTree = createTournamentTree rightPlayers
        let matchPlayer1 = List.last leftPlayers
        let matchPlayer2 = List.head rightPlayers
        Branch (leftTree, rightTree, { Player1 = matchPlayer1; Player2 = matchPlayer2 })

// Function to print the knockout tournament tree
let rec printTournamentTree (tree: Node) =
    match tree with
    | Leaf player -> printfn "%s" player.Name
    | Branch (left, right, matchData) ->
        printfn "Match: %s vs %s" matchData.Player1.Name matchData.Player2.Name
        printfn "  Left:"
        printTournamentTree left
        printfn "  Right:"
        printTournamentTree right

// Example usage
let players =
    [ { Seed = 4; Name = "Player1" }
      { Seed = 2; Name = "Player2" }
      { Seed = 3; Name = "Player3" }
      { Seed = 1; Name = "Player4" }
      { Seed = 5; Name = "Player5" }
      { Seed = 6; Name = "Player6" }
      { Seed = 8; Name = "Player7" }
      { Seed = 7; Name = "Player8" } ]

let tournamentTree = createTournamentTree players
printTournamentTree tournamentTree



// A function that takes a list of players and returns a list of matches for the first round of the knock-out tournament
let createBracket players =
  // Sort the players in ascending order by seed number
  let sortedPlayers = List.sortBy (fun p -> p.Seed) players
  // Split the sorted players into two halves
  let halfLength = List.length sortedPlayers / 2
  let firstHalf = List.take halfLength sortedPlayers
  let secondHalf = List.skip halfLength sortedPlayers
  // Reverse the second half
  let reversedSecondHalf = List.rev secondHalf
  // Zip the first half with the reversed second half and create matches
  List.map2 (fun p1 p2 -> { Player1 = p1; Player2 = p2 }) firstHalf reversedSecondHalf

// A function that takes a list of matches from the previous round and returns a list of matches for the next round
let nextRound matches =
  // A helper function that takes a match and returns the winner of the match
  // For simplicity, we assume that the winner is the one with the higher seed number
  let winner m =
    if m.Player1.Seed > m.Player2.Seed then m.Player1 else m.Player2
  // A helper function that takes a list and returns a list of pairs by grouping adjacent elements
  let rec group xs =
    match xs with
    | [] -> [] // If the list is empty, return an empty list
    | x::y::xs' -> (x, y) :: group xs' // If the list has at least two elements, pair up the first two elements and recurse on the rest
    | _ -> failwith "The list must have an even length" // If the list has an odd length, raise an exception
  // Get the winners from the previous round
  let winners = List.map winner matches
  // Group the winners into pairs and create matches
  group winners |> List.map (fun (p1, p2) -> { Player1 = p1; Player2 = p2 })

// A function that takes a list of matches and returns true if there is only one match left, false otherwise
let isFinal matches =
  List.length matches = 1

let knockOutTree players =
  // Create the matches for the first round
  let matches = createBracket players
  // A recursive function that takes a list of matches and returns a list of nodes by simulating each match and creating branch nodes with child nodes and match results
  let rec simulate matches =
    match matches with
    | [] -> [] // If there are no matches, return an empty list of nodes
    | [m] -> [Branch (Leaf m.Player1, Leaf m.Player2, m)] // If there is only one match left, return a singleton list containing a branch node with two leaf nodes as children and the match result 
    | _ ->
      let next = nextRound matches // Otherwise, get the next round matches 
      let nodes = simulate next // Recurse on the next round matches and get the child nodes 
      let rec pairUp ms ns =
        match ms, ns with 
        | [], [] -> [] // If both lists are empty, return an empty list of nodes 
        | m::ms', n1::n2::ns' -> Branch (n1, n2, m) :: pairUp ms' ns' // If both lists have elements, create a branch node with two child nodes from ns and a match result from ms and recurse on the rest 
        | _ -> failwith "The lists must have matching lengths" // If the lists have different lengths, raise an exception 
      pairUp matches nodes // Pair up the current round matches with the child nodes and
      // Return the first node in the list as the root node of the tournament tree
  let nodes = simulate matches
  List.head nodes

// A sample list of players for 12 players
let playerss = [
  { Seed = 1; Name = "Alice" };
  { Seed = 2; Name = "Bob" };
  { Seed = 3; Name = "Charlie" };
  { Seed = 4; Name = "David" };
  { Seed = 5; Name = "Eve" };
  { Seed = 6; Name = "Frank" };
  { Seed = 7; Name = "Grace" };
  { Seed = 8; Name = "Harry" };

]

// Create the knock-out tournament tree for these players
let tree = knockOutTree playerss



let median (nums: _ list) =
    let sortedNums = List.sort nums
    let length = List.length sortedNums
    match length % 2 with
    | 0 ->
        let idx1 = length / 2 - 1
        let idx2 = length / 2
        (sortedNums.Item idx1 + sortedNums.Item idx2) / 2.0
    | _ ->
        float (sortedNums.Item (length / 2))

// Test the median function
let numbers = [3.0; 1.0; 4.0; 1.0; 5.0; 9.0]
let numbersInt = [3; 1; 4; 1; 5; 9]
printfn "Median: %A" (median numbers)



let calculateRRTournamentTime (numPlayers: int) =
  let gamesPerPlayer = numPlayers - 1
  let minutesPerGame = 60
  let totalGames = numPlayers * gamesPerPlayer / 2 // divide by 2 because each game involves two players
  totalGames * minutesPerGame


//# PLAYER                  :  RATING  ERROR  POINTS  PLAYED  (%) CFS(%)    W    D    L D(%)
//Ceres 0.97RC3-t81-swa-1096:     0.0   ----     9.5      16   59    ---    5    9    2   56
//Dragon_8cpu               :     0.0   88.7     4.0       8   50     24    2    4    2   50
//Berserk_8cpu              :  -137.0   65.1     2.5       8   31     24    0    5    3   62


open MathNet.Numerics.Distributions

SpecialFunctions.Erf 0.99

// Your code using Math.NET Numerics goes here

// Constants and variables
let scorePercentage = 0.9999999 // Example score percentage (88.2%)
let totalGames = 2

let erf x n =
  let zScore = 1.96 // 95% confidence level
  
  // Calculate standard error
  let standardError = sqrt ((1.0 - x) * (1.0 - x) / float n)

  // Calculate margin of error
  let marginOfError = zScore * standardError

  // Calculate lower and upper bounds
  let lowerBound = x - marginOfError
  let upperBound = x + marginOfError

  // Calculate CFS using the erf function (Normal CDF)
  let normalDist = Normal(0.0, 1.0) // Standard normal distribution (mean = 0, standard deviation = 1)
  let cfs = normalDist.CumulativeDistribution(upperBound) - normalDist.CumulativeDistribution(lowerBound)

  // Convert CFS to percentage
  let cfsPercentage = cfs * 100.0
  cfsPercentage
 

let likelihoodSuperiority wins draws losses =
  //0.5f * (1 + (float)ErrorFunction.Erf((wins - losses) / MathF.Sqrt(2 * wins + 2 * losses)));
  let x = (float (wins - losses)) / Math.Sqrt(2.0 * float (wins + losses))
  let erf = SpecialFunctions.Erf (float x)
  printfn "x: %f erf: %f" x erf
  0.5 * (1.0 + erf)  

let better = likelihoodSuperiority 55 0 45
let s1 = likelihoodSuperiority 1 9 0
let cfs = erf 0.5 100

printfn "Better: %A" better
printfn "CFS: %A" cfs


let calc = 
  let sb = System.Text.StringBuilder()
  let appendLine (txt:string) = sb.AppendLine txt |> ignore

  let writeResultHeader =
    sprintf "%-25s : %7s %6s %7s %7s %4s %6s %4s %4s %4s %4s" "# PLAYER" "RATING" "ERROR" "POINTS" "PLAYED" "(%)" "CFS(%)" "W" "D" "L" "D(%)"

  let writeEngineLine player rating error points played percent cfs w d l dpercent =
    sprintf "%-25s : %7.1f %6.1f %7.1f %7d %4d %6d %4d %4d %4d %4d" player rating error points played percent cfs w d l dpercent

  let writeEngineLineBase player rating error points played percent cfs w d l dpercent =
    sprintf "%-25s : %7.1f %6s %7.1f %7d %4d %6s %4d %4d %4d %4d" player rating error points played percent cfs w d l dpercent

  let test =
    sb.Clear() |> ignore
    appendLine "results are:"
    writeResultHeader |> appendLine
    writeEngineLine "lc0.net.t81-swa-11061K" 11.2 20.6 154.5 300 52 86 50 209 41 70 |> appendLine
    writeEngineLineBase "lc0.net.784968" 0.0 "----" 145.5 300 49 "---" 41 209 50 70 |> appendLine
    sb.ToString()

  printfn "%s" test
    //printfn "%-25s : %7.1f %6s %7.1f %7d %4d %6s %4d %4d %4d %4d" "lc0.net.784968" 0.0 "----" 145.5 300 49 "---" 41 209 50 70

let timeOnly10 = new TimeOnly(12, 30)
let timeOnly11 = new TimeOnly(12, 30, 45)
let diffn = (timeOnly10 - timeOnly11).TotalMilliseconds

let timeOnly = new TimeOnly(12, 30, 45)
let timeOnly2 = new TimeOnly(12, 30, 45,12)
let timeOnly3 = new TimeOnly(12, 30, 46,123)

let t1 = timeOnly.ToString("HH:mm:ss")
let t2 = timeOnly2.ToString("HH:mm:ss.fff")

let timeOnly4 = TimeOnly.ParseExact (t1, "HH:mm:ss")
let t3 = timeOnly4.ToString("HH:mm:ss")

//let timeOnly5 = TimeOnly.ParseExact (t2, "HH:mm:ss.ff")
let timeOnly6 = TimeOnly.ParseExact (t2, "HH:mm:ss.fff")
timeOnly6.ToString("HH:mm:ss.fff")

let minTime = TimeOnly.MinValue.ToString("HH:mm:ss")
let maxTime = TimeOnly.MaxValue.ToString("HH:mm:ss")


let getMoveNumberString ply (move:string) = 
    if ply % 2 = 1 then
      let n = ply / 2 + ply % 2
      sprintf "%d. %s " n move
    else      
      sprintf "%s " move

let m = getMoveNumberString 1 "d4"
let m1 = getMoveNumberString 3 "Nf3"
let m2 = getMoveNumberString 2 "d5"
let m3 = getMoveNumberString 5 "Nc3"

let getMoveNumber ply = 
    if ply % 2 = 1 then
      let n = ply / 2 + ply % 2
      n
    else      
      ply / 2 + 1

let m9 = getMoveNumber 18
let m10 = getMoveNumber 19
let m20 = getMoveNumber 20
let m30 = getMoveNumber 21

let comb m m2 =
  sprintf "%s%s" m m2


