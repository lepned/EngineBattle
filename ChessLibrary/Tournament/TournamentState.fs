module ChessLibrary.TournamentState

open System
open System.IO
open System.Text
open System.Text.Json
open ChessLibrary.CupTypes
open ChessLibrary.SwissTypes
open ChessLibrary.TournamentTypes

/// Normalize bracket JSON that may have leading quotes stripped
let private normalizeBracketJson (json: string) =
    if String.IsNullOrWhiteSpace json then
        json
    else
        let trimmed = json.TrimStart()
        if trimmed.StartsWith("\"", StringComparison.Ordinal) && json.TrimEnd().EndsWith("}", StringComparison.Ordinal) then
            "{" + json
        else
            json

/// Start a MailboxProcessor agent for reading/writing cup bracket state
let startCupBracketReaderWriter (filePath: string) =
    MailboxProcessor<CupBracketMessage>.Start(fun inbox ->
        async {
            let optionsWrite = JsonSerializerOptions(WriteIndented = true)
            let optionsRead = JsonSerializerOptions(PropertyNameCaseInsensitive = true, AllowTrailingCommas = true)
            let mutable inMemory = ""
            while true do
                let! message = inbox.Receive()
                match message with
                | DisposeCupBracket ->
                    return ()
                | WriteCupBracket (bracket, reply) ->
                    try
                        bracket.UpdatedUtc <- DateTime.UtcNow
                        let json = JsonSerializer.Serialize(bracket, optionsWrite)
                        if String.IsNullOrWhiteSpace filePath then
                            inMemory <- json
                        else
                            let tmpPath = filePath + ".tmp"
                            File.WriteAllText(tmpPath, json, Encoding.UTF8)
                            File.Move(tmpPath, filePath, true)
                    with ex ->
                        eprintfn "WriteCupBracket error: %s" ex.Message
                    reply.Reply()
                | ReadCupBracket reply ->
                    try
                        let json =
                            if String.IsNullOrWhiteSpace filePath then
                                inMemory
                            else
                                if File.Exists filePath then
                                    use stream = new FileStream(filePath, FileMode.Open, FileAccess.Read, FileShare.ReadWrite ||| FileShare.Delete)
                                    use reader = new StreamReader(stream, Encoding.UTF8)
                                    reader.ReadToEnd()
                                else
                                    ""
                        let json = normalizeBracketJson json
                        if String.IsNullOrWhiteSpace json then
                            reply.Reply None
                        else
                            let loaded = JsonSerializer.Deserialize<CupBracket>(json, optionsRead)
                            if obj.ReferenceEquals(loaded, null) then reply.Reply None else reply.Reply (Some loaded)
                    with
                    | _ -> reply.Reply None
        })

/// Start a MailboxProcessor agent for reading/writing Swiss state
let startSwissStateReaderWriter (filePath: string) =
    MailboxProcessor<SwissStateMessage>.Start(fun inbox ->
        async {
            let optionsWrite = JsonSerializerOptions(WriteIndented = true)
            let optionsRead = JsonSerializerOptions(PropertyNameCaseInsensitive = true, AllowTrailingCommas = true)
            let mutable inMemory = ""
            while true do
                let! message = inbox.Receive()
                match message with
                | DisposeSwissState ->
                    return ()
                | WriteSwissState (state, reply) ->
                    try
                        state.UpdatedUtc <- DateTime.UtcNow
                        let json = JsonSerializer.Serialize(state, optionsWrite)
                        if String.IsNullOrWhiteSpace filePath then
                            inMemory <- json
                        else
                            let tmpPath = filePath + ".tmp"
                            File.WriteAllText(tmpPath, json, Encoding.UTF8)
                            File.Move(tmpPath, filePath, true)
                    with ex ->
                        eprintfn "WriteSwissState error: %s" ex.Message
                    reply.Reply()
                | ReadSwissState reply ->
                    try
                        let json =
                            if String.IsNullOrWhiteSpace filePath then
                                inMemory
                            else
                                if File.Exists filePath then
                                    use stream = new FileStream(filePath, FileMode.Open, FileAccess.Read, FileShare.ReadWrite ||| FileShare.Delete)
                                    use reader = new StreamReader(stream, Encoding.UTF8)
                                    reader.ReadToEnd()
                                else
                                    ""
                        let json = normalizeBracketJson json
                        if String.IsNullOrWhiteSpace json then
                            reply.Reply None
                        else
                            let loaded = JsonSerializer.Deserialize<SwissState>(json, optionsRead)
                            if obj.ReferenceEquals(loaded, null) then reply.Reply None else reply.Reply (Some loaded)
                    with
                    | _ -> reply.Reply None
        })
