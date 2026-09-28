namespace ChessLibrary

open System
open System.Diagnostics
open System.Threading.Tasks
open System.Text.RegularExpressions
open System.IO
open System.Collections.Generic
open System.Threading
open System.Text.Json
open Microsoft.FSharp.Core.Operators.Unchecked
open Configuration
open EngineProtocol
open RuntimeUtilities
open TypesDef.CoreTypes
open EngineTypes
open MoveTypes
open PositionTypes
open ChessLibrary.TimeControlTypes
open ChessLibrary.BoardUtils
open Microsoft.Extensions.Logging
open ChessLibrary.WinboardIntegration

/// How many games fit on this machine: memory per engine (measured once per engine config) and
/// the CPU threads the engines ask for.
module HardwareInfo =
  open Engine
  open EngineHelper
  open System.Collections.Concurrent

  let private footprintCache = ConcurrentDictionary<string, uint64>()

  let private footprintCacheKey (config: EngineConfig) =
        let hash =
            match config.Options.TryGetValue("Hash") with
            | true, v -> string v
            | _ -> ""
        let threads =
            match config.Options.TryGetValue("Threads") with
            | true, v -> string v
            | _ -> ""
        sprintf "%s|H=%s|T=%s" config.Path hash threads

  let estimateEngineRam (config: EngineConfig) : uint64 =
        let key = footprintCacheKey config
        footprintCache.GetOrAdd(key, fun _ ->
            let engine = createEngine (config, None)
            try
                initEngine 0 engine
                // snapshot working‐set
                uint64 engine.Process.WorkingSet64
            finally
                // initEngine can throw (WaitForReadyOk failure) — never leak the process
                try engine.StopProcess() with _ -> ())

  /// Sequentially walk your configs, measuring memory usage one at a time, after each measurement we GC to reclaim EVERYTHING.
  let footPrints (configs: EngineConfig seq) : (string * uint64)[] =
    let results = ResizeArray<string * uint64>()
    for cfg in configs do
        try
            let mem = estimateEngineRam cfg
            results.Add(cfg.Name, mem)
        with ex ->
            // log and continue if one engine fails
            printfn "⚠️  Measuring %s failed: %s" cfg.Name ex.Message
        // force native cleanup before next iteration
        GC.Collect()
        GC.WaitForPendingFinalizers()
    results.ToArray()
  
  let sumFootprints configs =
    footPrints configs |> Array.sumBy snd

  let concurrencyLevel (configs: EngineConfig seq) requested =
    //printfn "Requested concurrency: %d - calculated concurrency requirement" requested
    if requested = 1 then
        1
    else            
        let totalAvail = GC.GetGCMemoryInfo().TotalAvailableMemoryBytes |> uint64
        let headroomBytes = totalAvail / 2UL
        let footprints = 
            let sum = sumFootprints configs
            printfn "Sum of one copy each ≈ %d MB" (sum/1_048_576UL)
            sum
        
        let maxSets = int (headroomBytes / footprints)
        let concurrencyNum = min requested maxSets
        //printfn "Max sets of engines = %d" maxSets
        printfn "Using concurrency = %d" concurrencyNum
        concurrencyNum        
  
  let getThreads (engine:EngineConfig) =
        let totalCores = Environment.ProcessorCount - 1
        let threadValue = 
            if engine.Options.ContainsKey("Threads") then
                let value = engine.Options["Threads"]
                match value with
                | :? int as intVal -> intVal
                | :? string as strVal -> 
                    match Int32.TryParse(strVal) with
                    | true, num -> num
                    | _ -> 1
                | :? JsonElement as je when je.ValueKind = JsonValueKind.Number ->
                    je.GetInt32()                    
                | :? JsonElement as je when je.ValueKind = JsonValueKind.String ->
                    let str = je.GetString()
                    match System.Int32.TryParse str with
                    | (true, num) -> num
                    | _ -> 1
                | _ -> 1
            else
                1
        if threadValue > totalCores then totalCores else threadValue

  let sumEngineThreads (engines : ChessEngine seq) = engines |> Seq.sumBy(fun e -> getThreads e.Config)
  
  let assessMaxCpuConcurrencyLevel (engines : ChessEngine seq) =
    let totalThreads = sumEngineThreads engines
    let totalCores = Environment.ProcessorCount - 1
    let maxGames = totalCores / totalThreads
    maxGames
