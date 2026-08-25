module OutputFolderTests

open System
open System.IO
open Xunit
open ChessLibrary.PuzzleDataUtils

// A puzzle run used to guard every writer with Directory.Exists, so a missing
// output folder produced a complete run with no files and no message. These cover
// the helper that replaced that guard.

let private tempName () =
    Path.Combine(Path.GetTempPath(), "eb_out_" + Guid.NewGuid().ToString "N")

[<Theory>]
[<InlineData("")>]
[<InlineData("   ")>]
[<InlineData(null)>]
let ``an unset folder is NotConfigured, not a failure`` (folder: string) =
    match ensureOutputFolder folder with
    | NotConfigured -> ()
    | other -> failwithf "expected NotConfigured, got %A" other

[<Fact>]
let ``a missing folder is created and reported as created`` () =
    let path = tempName ()
    Assert.False(Directory.Exists path)
    try
        match ensureOutputFolder path with
        | Ready(resolved, created) ->
            Assert.True(created, "should report that it made the folder")
            Assert.True(Directory.Exists resolved, "folder must exist afterwards")
        | other -> failwithf "expected Ready, got %A" other
    finally
        if Directory.Exists path then Directory.Delete(path, true)

[<Fact>]
let ``an existing folder is reported as not created`` () =
    let path = tempName ()
    Directory.CreateDirectory path |> ignore
    try
        match ensureOutputFolder path with
        | Ready(_, created) -> Assert.False(created, "should not claim to have created an existing folder")
        | other -> failwithf "expected Ready, got %A" other
    finally
        Directory.Delete(path, true)

[<Fact>]
let ``nested folders are created in one call`` () =
    let root = tempName ()
    let nested = Path.Combine(root, "a", "b", "c")
    try
        match ensureOutputFolder nested with
        | Ready(resolved, created) ->
            Assert.True created
            Assert.True(Directory.Exists resolved)
        | other -> failwithf "expected Ready, got %A" other
    finally
        if Directory.Exists root then Directory.Delete(root, true)

[<Fact>]
let ``the resolved path is absolute even for a relative input`` () =
    let rel = "eb_out_rel_" + Guid.NewGuid().ToString "N"
    let expected = Path.GetFullPath rel
    try
        match ensureOutputFolder rel with
        | Ready(resolved, _) ->
            Assert.True(Path.IsPathRooted resolved, "callers report this path to the user")
            Assert.Equal(expected, resolved)
        | other -> failwithf "expected Ready, got %A" other
    finally
        if Directory.Exists expected then Directory.Delete(expected, true)

[<Fact>]
let ``surrounding whitespace is tolerated`` () =
    let path = tempName ()
    try
        match ensureOutputFolder ("  " + path + "  ") with
        | Ready(resolved, _) -> Assert.True(Directory.Exists resolved)
        | other -> failwithf "expected Ready, got %A" other
    finally
        if Directory.Exists path then Directory.Delete(path, true)

[<Fact>]
let ``an unusable path fails with a reason instead of throwing`` () =
    // a null character can never be part of a path on any platform
    match ensureOutputFolder "C:/eb\u0000bad" with
    | Failed(_, message) -> Assert.False(String.IsNullOrWhiteSpace message, "the reason is shown to the user")
    | other -> failwithf "expected Failed, got %A" other
