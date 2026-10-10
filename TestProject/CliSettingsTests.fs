module CliSettingsTests

open System.IO
open Xunit
open CliSettings

let private fromFile (s: Settings) () = Ok s
let private never () : Result<Settings, string> = failwith "the settings file must not be read"

[<Fact>]
let ``--syzygy, then EB_SYZYGY_PATH, then eb-cli.json`` () =
  let file = fromFile { empty with SyzygyPath = Some "F:/file" }
  Assert.Equal(Ok(Some "D:/flag"), resolveSyzygyPath (Some "D:/flag") (Some "E:/env") never)
  Assert.Equal(Ok(Some "E:/env"), resolveSyzygyPath None (Some "E:/env") never)
  Assert.Equal(Ok(Some "F:/file"), resolveSyzygyPath None None file)
  // blank counts as not given
  Assert.Equal(Ok(Some "F:/file"), resolveSyzygyPath (Some " ") (Some "") file)
  Assert.Equal(Ok None, resolveSyzygyPath None None (fromFile empty))

[<Fact>]
let ``a broken file matters only when it is needed`` () =
  let broken () : Result<Settings, string> = Error "eb-cli.json: not a JSON object"
  Assert.Equal(Ok(Some "D:/flag"), resolveSyzygyPath (Some "D:/flag") None broken)
  Assert.Equal(Error "eb-cli.json: not a JSON object", resolveSyzygyPath None None broken)
  Assert.Equal(Ok "C:/defs/Lc0.json", resolveEngine ((=) "C:/defs/Lc0.json") "C:/defs/Lc0.json" broken)

[<Fact>]
let ``an engine is a file as given, else a bare name in engineDefsFolder`` () =
  let files = set [ "C:/elsewhere/sf.exe"; Path.Combine("D:/defs", "Lc0_BT4_332.json"); Path.Combine("D:/defs", "Ceres.json") ]
  let exists f = files.Contains f
  let defs = fromFile { empty with EngineDefsFolder = Some "D:/defs" }
  // a path that exists is used as it is, without reading the settings
  Assert.Equal(Ok "C:/elsewhere/sf.exe", resolveEngine exists "C:/elsewhere/sf.exe" never)
  // a bare name, with or without .json
  Assert.Equal(Ok(Path.Combine("D:/defs", "Lc0_BT4_332.json")), resolveEngine exists "Lc0_BT4_332" defs)
  Assert.Equal(Ok(Path.Combine("D:/defs", "Ceres.json")), resolveEngine exists "Ceres.json" defs)
  // an engine binary next to its def does not win over the def (Linux/macOS binaries have no extension)
  let both = set [ Path.Combine("D:/defs", "stockfish"); Path.Combine("D:/defs", "stockfish.json") ]
  Assert.Equal(Ok(Path.Combine("D:/defs", "stockfish.json")), resolveEngine both.Contains "stockfish" defs)
  // a missing path with a folder in it is not looked up anywhere else
  Assert.True(Result.isError (resolveEngine exists "C:/missing/Lc0_BT4_332.json" never))
  // not in the folder, or no folder set: the message says where it looked
  match resolveEngine exists "Stockfish" defs with
  | Error e -> Assert.Contains("D:/defs", e)
  | other -> Assert.Fail $"{other}"
  match resolveEngine exists "Lc0_BT4_332" (fromFile empty) with
  | Error e -> Assert.Contains("engineDefsFolder", e)
  | other -> Assert.Fail $"{other}"

[<Fact>]
let ``the file is read, and what it cannot use is said`` () =
  let all = """{ "engineDefsFolder": "D:/defs", "syzygyPath": "D:/sygyzy", "puzzleResultsFolder": "D:/puzzles" }"""
  Assert.Equal(Ok({ EngineDefsFolder = Some "D:/defs"; SyzygyPath = Some "D:/sygyzy"; PuzzleResultsFolder = Some "D:/puzzles" }, []), parse all)
  // comments and a trailing comma are allowed in a hand-written file
  Assert.Equal(Ok({ empty with SyzygyPath = Some "D:/a;E:/b" }, []), parse "{ // tables\n \"syzygyPath\": \"D:/a;E:/b\", }")
  Assert.Equal(Ok(empty, []), parse """{ "syzygyPath": "" }""")
  Assert.Equal(Ok(empty, []), parse "")
  Assert.Equal(Ok(empty, []), parse "  \r\n")
  // keys in any case, as EB's PascalCase engine defs write them
  Assert.Equal(Ok({ empty with SyzygyPath = Some "D:/sygyzy" }, []), parse """{ "SyzygyPath": "D:/sygyzy" }""")
  // a misspelt key is warned about, not silently ignored
  match parse """{ "syzygyFolder": "D:/sygyzy" }""" with
  | Ok(s, [ warning ]) ->
      Assert.Equal(None, s.SyzygyPath)
      Assert.Contains("syzygyFolder", warning)
  | other -> Assert.Fail $"{other}"
  Assert.True(Result.isError (parse """{ "engineDefsFolder": 6 }"""))
  Assert.True(Result.isError (parse """[ "D:/sygyzy" ]"""))
  Assert.True(Result.isError (parse """{ "syzygyPath": """))

[<Fact>]
let ``folders in the file are relative to the file, and no file is no settings`` () =
  Assert.Equal(Ok(empty, []), load (Path.Combine(Path.GetTempPath(), "eb_no_such_dir", fileName)))
  let dir = Directory.CreateTempSubdirectory("eb_cli_").FullName
  try
    let file = Path.Combine(dir, fileName)
    let absolute = if Path.PathSeparator = ';' then "D:/abs" else "/abs"
    File.WriteAllText(file, $"""{{ "engineDefsFolder": "defs", "syzygyPath": "syzygy{Path.PathSeparator}{absolute}", "puzzleResultsFolder": "res{Path.PathSeparator}x" }}""")
    match load file with
    | Ok(s, []) ->
        Assert.Equal(Some(Path.Combine(dir, "defs")), s.EngineDefsFolder)
        // only syzygyPath is a list; the other keys are one folder each (not split on the separator)
        Assert.Equal(Some(Path.Combine(dir, $"res{Path.PathSeparator}x")), s.PuzzleResultsFolder)
        Assert.Equal(Some(Path.Combine(dir, "syzygy") + string Path.PathSeparator + absolute), s.SyzygyPath)
    | other -> Assert.Fail $"{other}"
    File.WriteAllText(file, "{ not json")
    match load file with
    | Error e -> Assert.StartsWith(file, e)
    | other -> Assert.Fail $"{other}"
  finally Directory.Delete(dir, true)

[<Fact>]
let ``puzzle trend may leave its folder to eb-cli.json`` () =
  let specs = Registry.all |> List.map (fun e -> e.Spec)
  match Cli.Parse.parse specs [ "puzzle"; "trend"; "--arm"; "srv_deg4" ] with
  | Cli.Parse.Run p ->
      Assert.Equal(None, Cli.Parse.argOf p "folder")
      Assert.Equal(Some "srv_deg4", Cli.Parse.valueOf p "arm")
  | other -> Assert.Fail $"{other}"
  match Cli.Parse.parse specs [ "puzzle"; "trend"; "D:/results"; "--arm"; "srv_deg4" ] with
  | Cli.Parse.Run p -> Assert.Equal(Some "D:/results", Cli.Parse.argOf p "folder")
  | other -> Assert.Fail $"{other}"
