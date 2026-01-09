open System
open BenchmarkDotNet.Running
open Benchmarks.ParserBenchmarks

[<EntryPoint>]
let main args =
    let showHelp () =
        printfn "PGN Parser Benchmarks"
        printfn "====================="
        printfn ""
        printfn "Usage: Benchmarks [command]"
        printfn ""
        printfn "Commands:"
        printfn "  quick       Run quick comparison (no BenchmarkDotNet, instant results)"
        printfn "  string      Run string parser benchmarks with BenchmarkDotNet"
        printfn "  file        Run file parser benchmarks with BenchmarkDotNet"
        printfn "  comments    Run comment-aware vs no-comments benchmarks"
        printfn "  real        Run real file benchmarks with BenchmarkDotNet"
        printfn "  all         Run all BenchmarkDotNet benchmarks"
        printfn "  help        Show this help message"
        printfn ""
        printfn "Examples:"
        printfn "  dotnet run -- quick"
        printfn "  dotnet run -c Release -- comments"
        printfn "  dotnet run -c Release -- all"
        0

    match args |> Array.map (fun s -> s.ToLowerInvariant()) |> Array.toList with
    | [] | ["help"] | ["--help"] | ["-h"] ->
        showHelp()
    
    | ["quick"] ->
        runQuickComparison()
        0
    
    | ["string"] ->
        printfn "Running String Parser Benchmarks..."
        BenchmarkRunner.Run<StringParserBenchmarks>() |> ignore
        0
    
    | ["file"] ->
        printfn "Running File Parser Benchmarks..."
        BenchmarkRunner.Run<FileParserBenchmarks>() |> ignore
        0
    
    | ["comments"] ->
        printfn "Running Comment Parser Benchmarks..."
        BenchmarkRunner.Run<CommentParserBenchmarks>() |> ignore
        0
    
    | ["real"] ->
        printfn "Running Real File Benchmarks..."
        BenchmarkRunner.Run<RealFileBenchmarks>() |> ignore
        0
    
    | ["all"] ->
        printfn "Running All Benchmarks..."
        printfn ""
        BenchmarkRunner.Run<StringParserBenchmarks>() |> ignore
        BenchmarkRunner.Run<FileParserBenchmarks>() |> ignore
        BenchmarkRunner.Run<CommentParserBenchmarks>() |> ignore
        BenchmarkRunner.Run<RealFileBenchmarks>() |> ignore
        0
    
    | unknown ->
        printfn "Unknown command: %s" (String.Join(" ", unknown))
        printfn ""
        showHelp()
