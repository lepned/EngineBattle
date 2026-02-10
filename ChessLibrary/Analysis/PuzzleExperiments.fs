module ChessLibrary.PuzzleExperiments

open System
open System.Diagnostics
open System.Threading
open System.Text.RegularExpressions
open ChessLibrary.PuzzleTypes
open ChessLibrary.TypesDef.PuzzleInput
open ChessLibrary.PuzzleRunners

//-------------------------------------------------------------------------
// 1. Generic Helpers
//-------------------------------------------------------------------------

/// Update a given parameter (e.g. "V1TEMP") in the config string.
/// This function uses a regex pattern and returns the updated config.
let updateParameter (config: string) (paramName: string) (newValue: float) =
    let pattern = sprintf @"(%s=)([\d\.]+)" paramName
    Regex.Replace(config, pattern, fun m -> sprintf "%s%g" m.Groups.[1].Value newValue)

/// Extract a parameter's value from the config string.
let extractParameter (paramName: string) (config: string) : float option =
    let pattern = sprintf @"%s=([\d\.]+)" paramName
    let m = Regex.Match(config, pattern)
    if m.Success then Some (float m.Groups.[1].Value) else None

//-------------------------------------------------------------------------
// 2. Generic Parameter Optimizer
//-------------------------------------------------------------------------

/// Generic optimizer for one parameter.
/// - paramName: the parameter to optimize (e.g. "V1TEMP").
/// - config: the current configuration string.
/// - currentValue: the current value for the parameter.
/// - step: how much to change the parameter each iteration (can be negative).
/// - scoreFunc: a function that takes a config string and returns a score.
/// - boundCheck: a predicate ensuring the new value is within allowed bounds.
let rec optimizeParameter paramName (config: string) (currentValue: float)
                          (step: float) (scoreFunc: string -> float)
                          (boundCheck: float -> bool) (bestScore: float) (latestBestValue: float) =
  let newValue = currentValue + step
  let contDescent =
    match paramName with
    | "V1TEMP" -> newValue > 1.0
    | "V2FRAC" -> newValue < 0.2
    | "V2TEMP" -> newValue > 1.0
    | _ -> failwithf "Unknown parameter: %s" paramName

  let step =
    if contDescent then
      let p = sign step
      0.2 * float p
    else
      step

  if not (boundCheck newValue) then
      // Cannot step further: return current config, value, and score.
      config, currentValue, bestScore
  else
      let newConfig = updateParameter config paramName newValue
      let newScore = scoreFunc newConfig
      if newScore > bestScore then
          printfn "Optimized %s: %g -> %g (score: %g -> %g)" paramName currentValue newValue bestScore newScore
          // Continue optimizing recursively.
          optimizeParameter paramName newConfig newValue step scoreFunc boundCheck newScore newValue
      elif contDescent then
          printfn "Continue descending wit step size %f: %s: %g -> %g (score: %g -> %g)" step paramName currentValue newValue bestScore newScore
          // Continue optimizing recursively.
          optimizeParameter paramName newConfig newValue step scoreFunc boundCheck bestScore latestBestValue
      else
          // No further improvement; return current best.
          let config = updateParameter config paramName latestBestValue
          config, latestBestValue, bestScore


//-------------------------------------------------------------------------
// 3. Coordinate Descent
//-------------------------------------------------------------------------

/// Optimize V1TEMP (by decreasing it) and V2FRAC (by increasing it) and V2TEMP (by decreasing it)
/// until no further improvement is achieved.
let rec coordinateDescent (config: string) (step: float) (scoreFunc: string -> float) (bestScore:float) =

  // Extract current parameter values; fail if not found.
  let v1 = extractParameter "V1TEMP" config |> Option.defaultWith (fun () -> failwith "V1TEMP not found")
  let v2Frac = extractParameter "V2FRAC" config |> Option.defaultWith (fun () -> failwith "V2FRAC not found")
  let v2temp = extractParameter "V2TEMP" config |> Option.defaultWith (fun () -> failwith "V2TEMP not found")

   // Optimize each parameter in turn.
  let (newConfig, newV1, score) = optimizeParameter "V1TEMP" config v1 (-step) scoreFunc (fun x -> x > 0.5) bestScore v1
  let (newConfig, newV2Frac, score) = optimizeParameter "V2FRAC" newConfig v2Frac step scoreFunc (fun x -> x < 0.6) score v2Frac
  let (newConfig, newV2Temp, score) =
    if newV2Frac > 0.0 then
      optimizeParameter "V2TEMP" newConfig v2temp (-step) scoreFunc (fun x -> x > 0.5) score v2temp
    else
      (newConfig, newV2Frac, score)

  // If any parameter changed, run another round.
  if config <> newConfig then
       printfn "Recursively doing a new round of optimization since previous run changed optimal configuration..."
       printfn "New config: %s" newConfig
       printfn "Old config: %s" config

       coordinateDescent newConfig step scoreFunc score
  else
       //todo : reduce the step size if it is high and run again
      if step > 0.05 then
        printfn "Reducing step size by half and recursively run optimization..."
        let newStep = step / 2.0
        coordinateDescent newConfig newStep scoreFunc score
      else
        newConfig


//-------------------------------------------------------------------------
// 4. Example Score Functions and Policy/Value Tests
//-------------------------------------------------------------------------

/// Policy node score function: updates the engine config, runs the test, and returns accuracy.
let policyNodeScore (input: PuzzleInput) (callback: Action<Lichess>) (config: string)  : float =
    let engine = fst (Seq.head input.engines)
    engine.Options.["Network"] <- box config
    let res = runPolicyHeadTest (input, callback, CancellationToken.None)
    let score = res.[0]
    float score.Correct / float score.TotalNumber

/// Similarly, a value score function.
let valueScore (input: PuzzleInput) (callback: Action<Lichess>) (config: string)  : float =
    let engine = fst (Seq.head input.engines)
    engine.Options.["Network"] <- box config
    let res = runPolicyHeadTest (input, callback, CancellationToken.None)
    let score = res.[0]
    float score.Correct / float score.TotalNumber

let optimizeValueHeadTest (input : PuzzleInput, withHistory: bool) =
  let watch = Stopwatch.StartNew()
  let firstEngine = input.engines |> Seq.head
  let engineConfigs = ResizeArray<ChessLibrary.TypesDef.CoreTypes.EngineConfig * int>()
  engineConfigs.Add firstEngine
  let newInput = {input with engines = engineConfigs}
  // The initial configuration string.
  let initialConfig = "C:/Dev/Chess/Networks/CeresTrainNet/Official/C1-640-34.value3_L32_ZDeblundered_x1_d2.onnx.value3.onnx|V1TEMP=1.4;V2FRAC=0.0;V2TEMP=1.8"
  // Define the step size (e.g. 0.05)

  let step = 0.2
  let emptyCallback = fun (e:Lichess) -> ()
  let optimizer = valueScore newInput emptyCallback
  printfn "Initial configuration:\n%s" initialConfig
  printfn "Initial score: %g" (optimizer initialConfig)

  // Run coordinate descent to optimize both parameters.
  let optimizedConfig = coordinateDescent initialConfig step optimizer 0.0
  watch.Stop()
  let elapsed = watch.Elapsed
   //how long did it take to optimize
  let timing = sprintf "Time taken to optimize: %d minutes %d seconds" elapsed.Minutes elapsed.Seconds
  printfn "%s" timing
  printfn "Initial input: Sample size: %d, Nodes: %s, Rating: %d, Ratinggroups: %A" input.sampleSize input.nodes input.maxRating input.ratingGroups
  let msg = sprintf "\nOptimized configuration:\n%s" optimizedConfig
  msg

let optimizePolicyHeadTest (input : PuzzleInput, withHistory: bool) =
  //add timer for the optimization
  let watch = Stopwatch.StartNew()
  let firstEngine = input.engines |> Seq.head
  let engineConfigs = ResizeArray<ChessLibrary.TypesDef.CoreTypes.EngineConfig * int>()
  engineConfigs.Add firstEngine
  let newInput = {input with engines = engineConfigs}
  let initialConfig = "C:/Dev/Chess/Networks/CeresTrainNet/Official/C1-640-34.value3_L32_ZDeblundered_x1_d2.onnx.value3.onnx|V1TEMP=1.8;V2FRAC=0.0;V2TEMP=1.8"
  // Define the step size (e.g. 0.05)
  let step = 0.2
  let emptyCallback = fun (e:Lichess) -> ()
  let optimizer = policyNodeScore newInput emptyCallback
  printfn "Initial configuration:\n%s" initialConfig

  // Run coordinate descent to optimize both parameters.
  let optimizedConfig = coordinateDescent initialConfig step optimizer 0.0

  watch.Stop()
  let elapsed = watch.Elapsed
  //how long did it take to optimize
  let timing = sprintf "Time taken to optimize: %d minutes %d seconds" elapsed.Minutes elapsed.Seconds
  printfn "%s" timing
  printfn "Initial input: Sample size: %d, Nodes: %s, Rating: %d, Ratinggroups: %A" input.sampleSize input.nodes input.maxRating input.ratingGroups
  let msg = sprintf "\nOptimized configuration:\n%s" optimizedConfig
  msg
