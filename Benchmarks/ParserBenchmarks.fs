namespace Benchmarks

open System
open System.IO
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Columns
open ChessLibrary
/// BenchmarkDotNet benchmarks comparing full PGN parser vs lightweight FastPGNParser
module ParserBenchmarks =

    /// Generate multiple games for benchmark testing
    let generateMultiGamePgn (gameCount: int) =
        let sb = System.Text.StringBuilder()
        for i in 1 .. gameCount do
            sb.AppendLine(sprintf "[Event \"Benchmark Game %d\"]" i) |> ignore
            sb.AppendLine("[Site \"Local\"]") |> ignore
            sb.AppendLine(sprintf "[Date \"2026.01.%02d\"]" (i % 28 + 1)) |> ignore
            sb.AppendLine(sprintf "[Round \"%d\"]" i) |> ignore
            sb.AppendLine(sprintf "[White \"Player%dW\"]" i) |> ignore
            sb.AppendLine(sprintf "[Black \"Player%dB\"]" i) |> ignore
            sb.AppendLine("[Result \"1/2-1/2\"]") |> ignore
            sb.AppendLine() |> ignore
            sb.AppendLine("1. e4 e5 2. Nf3 Nc6 (2... Nf6 3. Nxe5 d6) 3. Bb5 a6 4. Ba4 Nf6 5. O-O Be7") |> ignore
            sb.AppendLine("6. Re1 b5 7. Bb3 d6 8. c3 O-O 9. h3 {A common move} Na5 10. Bc2 c5") |> ignore
            sb.AppendLine("11. d4 Qc7 12. Nbd2 Nc6 13. dxc5 dxc5 14. Nf1 Be6 15. Ne3 Rad8 1/2-1/2") |> ignore
            sb.AppendLine() |> ignore
        sb.ToString()

    /// Benchmark class for comparing string-based parsing
    [<MemoryDiagnoser>]
    [<RankColumn>]
    type StringParserBenchmarks() =
        
        let mutable tempFilePath = ""
        let mutable multiGameContent = ""
        
        [<Params(1, 10, 50)>]
        member val GameCount = 1 with get, set
        
        [<GlobalSetup>]
        member this.Setup() =
            multiGameContent <- generateMultiGamePgn this.GameCount
            tempFilePath <- Path.GetTempFileName()
            File.WriteAllText(tempFilePath, multiGameContent)
        
        [<GlobalCleanup>]
        member _.Cleanup() =
            if File.Exists(tempFilePath) then
                File.Delete(tempFilePath)
       
    /// Benchmark class for file-based parsing at larger scales
    [<MemoryDiagnoser>]
    [<RankColumn>]
    type FileParserBenchmarks() =
        
        let mutable tempFilePath = ""
        
        [<Params(10, 50, 100)>]
        member val GameCount = 10 with get, set
        
        [<GlobalSetup>]
        member this.Setup() =
            let content = generateMultiGamePgn this.GameCount
            tempFilePath <- Path.GetTempFileName()
            File.WriteAllText(tempFilePath, content)
        
        [<GlobalCleanup>]
        member _.Cleanup() =
            if File.Exists(tempFilePath) then
                File.Delete(tempFilePath)
        
        [<Benchmark(Baseline = true, Description = "Full PGN File Parser")>]
        member _.FullFileParser() =
            let games = FullPGNParser.parsePgnFile tempFilePath |> Seq.toArray
            games.Length

        [<Benchmark(Description = "Full PGN File Parser with Raw")>]
        member _.FullFileParserWithRaw() =
            let games = FullPGNParser.parsePgnFileWithRaw tempFilePath |> Seq.toArray
            games.Length

    /// Benchmark class comparing comment-aware vs no-comments parsing
    [<MemoryDiagnoser>]
    [<RankColumn>]
    type CommentParserBenchmarks() =
        
        let mutable tempFilePath = ""
        let mutable multiGameContent = ""
        
        [<Params(10, 50, 100)>]
        member val GameCount = 10 with get, set
        
        [<GlobalSetup>]
        member this.Setup() =
            multiGameContent <- generateMultiGamePgn this.GameCount
            tempFilePath <- Path.GetTempFileName()
            File.WriteAllText(tempFilePath, multiGameContent)
        
        [<GlobalCleanup>]
        member _.Cleanup() =
            if File.Exists(tempFilePath) then
                File.Delete(tempFilePath)
        

    /// Benchmark using real PGN files from test data
    [<MemoryDiagnoser>]
    [<RankColumn>]
    type RealFileBenchmarks() =
        
        let mutable testFilePath = ""
        let mutable isTempFile = false
        
        [<GlobalSetup>]
        member _.Setup() =
            // Try to find the test data file
            let possiblePaths = [
                @"TestProject\TestData\SF_vs_Ceres_and_Lc0.pgn"
                @"..\TestProject\TestData\SF_vs_Ceres_and_Lc0.pgn"
                @"..\..\TestProject\TestData\SF_vs_Ceres_and_Lc0.pgn"
            ]
            match possiblePaths |> List.map Path.GetFullPath |> List.tryFind File.Exists with
            | Some path -> 
                testFilePath <- path
                isTempFile <- false
            | None ->
                // Create a fallback test file
                testFilePath <- Path.GetTempFileName()
                File.WriteAllText(testFilePath, generateMultiGamePgn 20)
                isTempFile <- true
        
        [<GlobalCleanup>]
        member _.Cleanup() =
            if isTempFile && File.Exists(testFilePath) then
                File.Delete(testFilePath)
        
        [<Benchmark(Baseline = true, Description = "Full Parser - Real File")>]
        member _.FullParserRealFile() =
            if not (String.IsNullOrEmpty(testFilePath)) && File.Exists(testFilePath) then
                let games = FullPGNParser.parsePgnFile testFilePath |> Seq.toArray
                games.Length
            else
                0
        
    /// Benchmark class comparing FullSpanParser (full features + span-based) vs PGNParser
    [<MemoryDiagnoser>]
    [<RankColumn>]
    type FullSpanParserBenchmarks() =
        
        let mutable tempFilePath = ""
        let mutable multiGameContent = ""
        
        [<Params(10, 50, 100)>]
        member val GameCount = 10 with get, set
        
        [<GlobalSetup>]
        member this.Setup() =
            multiGameContent <- generateMultiGamePgn this.GameCount
            tempFilePath <- Path.GetTempFileName()
            File.WriteAllText(tempFilePath, multiGameContent)
        
        [<GlobalCleanup>]
        member _.Cleanup() =
            if File.Exists(tempFilePath) then
                File.Delete(tempFilePath)
        
        [<Benchmark(Baseline = true, Description = "Original Full PGN Parser (file)")>]
        member _.OriginalFullParser() =
            let games = FullPGNParser.parsePgnFile tempFilePath |> Seq.toArray
            games.Length
        
        [<Benchmark(Description = "FullSpanParser (file)")>]
        member _.FullSpanParserFile() =
            let games = FullPGNParser.parsePgnFile tempFilePath |> Seq.toArray
            games.Length

        [<Benchmark(Description = "FullSpanParser (string)")>]
        member _.FullSpanParserString() =
            let games = FullPGNParser.parsePgnString multiGameContent |> Seq.toArray
            games.Length

        [<Benchmark(Description = "FullSpanParser with Raw (file)")>]
        member _.FullSpanParserWithRaw() =
            let games = FullPGNParser.parsePgnFileWithRaw tempFilePath |> Seq.toArray
            games.Length

    /// Quick comparison without BenchmarkDotNet (for rapid testing)
    let runQuickComparison () =
        printfn "Quick Parser Comparison"
        printfn "======================="
        printfn ""
        
        let iterations = 100
        let gameCount = 50
        let content = generateMultiGamePgn gameCount
        
        // Write to temp file for full parser
        let tempFile = Path.GetTempFileName()
        File.WriteAllText(tempFile, content)
        
        try
            // Warmup
            FullPGNParser.parsePgnFile tempFile |> Seq.length |> ignore
            FullPGNParser.parsePgnFile tempFile |> Seq.length |> ignore
            FullPGNParser.parsePgnString content |> Seq.length |> ignore
            FullPGNParser.parsePgnFileWithRaw tempFile |> Seq.length |> ignore
            
            // Full parser timing (file-based)
            let sw1 = System.Diagnostics.Stopwatch.StartNew()
            for _ in 1 .. iterations do
                FullPGNParser.parsePgnFile tempFile |> Seq.length |> ignore
            sw1.Stop()
            let fullTime = sw1.Elapsed.TotalMilliseconds

            // FullSpanParser timing (file-based)
            let swFull1 = System.Diagnostics.Stopwatch.StartNew()
            for _ in 1 .. iterations do
                FullPGNParser.parsePgnFile tempFile |> Seq.length |> ignore
            swFull1.Stop()
            let fullSpanFileTime = swFull1.Elapsed.TotalMilliseconds

            // FullSpanParser timing (string-based)
            let swFull2 = System.Diagnostics.Stopwatch.StartNew()
            for _ in 1 .. iterations do
                FullPGNParser.parsePgnString content |> Seq.length |> ignore
            swFull2.Stop()
            let fullSpanStringTime = swFull2.Elapsed.TotalMilliseconds

            // FullSpanParser with Raw timing (file-based)
            let swRaw = System.Diagnostics.Stopwatch.StartNew()
            for _ in 1 .. iterations do
                FullPGNParser.parsePgnFileWithRaw tempFile |> Seq.length |> ignore
            swRaw.Stop()
            let fullSpanRawTime = swRaw.Elapsed.TotalMilliseconds
            
            printfn "Parsing %d games x %d iterations:" gameCount iterations
            printfn ""
            printfn "  FULL PARSERS (with variations, comments, NAGs):"
            printfn "  FullSpanParser (file):            %8.2f ms total (%6.3f ms/iter)" fullSpanFileTime (fullSpanFileTime / float iterations)
            printfn "  FullSpanParser (string):          %8.2f ms total (%6.3f ms/iter)" fullSpanStringTime (fullSpanStringTime / float iterations)
            printfn "  FullSpanParser with Raw (file):   %8.2f ms total (%6.3f ms/iter)" fullSpanRawTime (fullSpanRawTime / float iterations)
            printfn ""
            
            // Memory comparison (rough estimate via GC)
            GC.Collect()
            GC.WaitForPendingFinalizers()
            let memBeforeFullSpan = GC.GetTotalMemory(true)
            FullPGNParser.parsePgnFile tempFile |> Seq.toArray |> ignore
            let memAfterFullSpan = GC.GetTotalMemory(false)

            GC.Collect()
            GC.WaitForPendingFinalizers()
            let memBeforeWithRaw = GC.GetTotalMemory(true)
            FullPGNParser.parsePgnFileWithRaw tempFile |> Seq.toArray |> ignore
            let memAfterWithRaw = GC.GetTotalMemory(false)
            
            printfn "Approximate memory usage for %d games:" gameCount
            printfn "  FullSpanParser:           ~%d KB" ((memAfterFullSpan - memBeforeFullSpan) / 1024L)
            printfn "  FullSpanParser with Raw:  ~%d KB" ((memAfterWithRaw - memBeforeWithRaw) / 1024L)
        finally
            if File.Exists(tempFile) then
                File.Delete(tempFile)
