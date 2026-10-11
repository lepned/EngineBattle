/// The tournament configs the docs tell people to start from load and validate as written. Only
/// what points at the reader's machine (books, folders, engine defs) is pointed at test files; no
/// engine is started.
module DocTournamentConfigTests

open System
open System.IO
open System.Text.Json.Nodes
open Xunit
open ChessLibrary

let private repoFile (name: string) =
    // the repo root, from TestProject/bin/<config>/<tfm>
    File.ReadAllText(Path.Combine(AppContext.BaseDirectory, "..", "..", "..", "..", name)).Replace("\r\n", "\n")

/// The first fenced block after `heading`.
let private blockAfter (heading: string) (doc: string) =
    let start = doc.IndexOf heading
    Assert.True(start >= 0, $"no '{heading}' in the doc")
    let fence = doc.IndexOf("```", start)
    let bodyStart = doc.IndexOf('\n', fence) + 1
    doc.Substring(bodyStart, doc.IndexOf("\n```", bodyStart) - bodyStart)

/// The config with its machine-specific paths pointed at files in `dir`, loaded and validated.
let loadAndValidate (json: string) =
    let dir = Directory.CreateTempSubdirectory("eb_doc_").FullName
    try
        let node = JsonNode.Parse(json, Nullable(), Text.Json.JsonDocumentOptions(AllowTrailingCommas = true)).AsObject()
        let book = Path.Combine(dir, "book.pgn")
        File.WriteAllText(book, "[Event \"x\"]\n[Result \"*\"]\n\n1. e4 e5 *\n")
        // a config without an Opening block plays from the start position
        match node.["Opening"] with
        | :? JsonObject as opening -> opening.["OpeningsPath"] <- JsonValue.Create book
        | _ -> ()
        node.["PgnOutPath"] <- JsonValue.Create(Path.Combine(dir, "out.pgn"))
        node.["EngineSetup"].["EngineDefFolder"] <- JsonValue.Create dir
        // every def the config names, as a small one for an executable that exists
        for def in node.["EngineSetup"].["EngineDefList"].AsArray() do
            let name = def.GetValue<string>()
            let engine =
                sprintf """{ "Name": "%s", "Alias": "", "Version": "", "TimeControlID": 1, "Rating": 3000, "Dev": "", "LogoPath": "",
                            "Protocol": "UCI", "ContemptEnabled": false, "NegativeContemptAllowed": false,
                            "Path": %s, "NetworkPath": "", "Args": "", "Options": {} }"""
                    (Path.GetFileNameWithoutExtension name) (Text.Json.JsonSerializer.Serialize Environment.ProcessPath)
            let file = Path.Combine(dir, name)
            Directory.CreateDirectory(Path.GetDirectoryName file) |> ignore   // a def in a subfolder
            File.WriteAllText(file, engine)
        match node.["Adjudication"] with
        | :? JsonObject as adj when (adj.["TBAdj"] :? JsonObject) ->
            let tb = Directory.CreateDirectory(Path.Combine(dir, "tb")).FullName
            File.WriteAllText(Path.Combine(tb, "KQvK.rtbw"), "")
            adj.["TBAdj"].["TablebaseDirectory"] <- JsonValue.Create tb
        | _ -> ()
        let path = Path.Combine(dir, "tournament.json")
        File.WriteAllText(path, node.ToJsonString())
        match Configuration.JSON.tryReadTournamentJson path with
        | Error e -> failwith e
        | Ok t ->
            let engines = Configuration.JSON.readEngineDefs t.EngineSetup.EngineDefFolder t.EngineSetup.EngineDefList
            let t = { t with EngineSetup = { t.EngineSetup with Engines = engines } }
            match Configuration.Validation.validateTournament t with
            | Configuration.Validation.Errors messages -> failwith (String.Join("\n", messages))
            | _ -> t
    finally Directory.Delete(dir, true)

[<Fact>]
let ``the minimal tournament config in ConsoleTools.md loads and validates`` () =
    let t = loadAndValidate (blockAfter "### Minimal Tournament Configuration" (repoFile "ConsoleTools.md"))
    Assert.False t.Adjudication.DrawOption.Enabled

[<Fact>]
let ``the tournament example in TournamentConfig.md loads and validates`` () =
    let t = loadAndValidate (blockAfter "## Tournament.json example" (repoFile "TournamentConfig.md"))
    // the example names a tablebase folder and nothing else: tablebase adjudication is on
    Assert.True t.Adjudication.TBAdj.UseTBAdjudication
