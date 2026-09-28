/// The public surface of the engine layer (modules Engine, EngineHelper and HardwareInfo), pinned
/// as text. The Engine.fs rewrite keeps this API unchanged - the WebGUI's C# compiles against it -
/// and this test is the proof: every public type, member and signature is listed in
/// TestData/EngineApi.txt. A deliberate change updates that file in the same commit; an accidental
/// one fails here. On a mismatch the current surface is written next to the test run, to diff.
module EngineApiSurfaceTests

open System
open System.IO
open System.Reflection
open Xunit

let rec private typeName (t: Type) : string =
    if t.IsGenericParameter then t.Name
    elif t.IsArray then typeName (t.GetElementType()) + "[]"
    elif t.IsByRef then typeName (t.GetElementType()) + "&"
    elif t.IsGenericType then
        let def = t.GetGenericTypeDefinition().FullName
        let baseName = def.Substring(0, def.IndexOf '`')
        sprintf "%s<%s>" baseName (t.GetGenericArguments() |> Array.map typeName |> String.concat ", ")
    else
        match t.FullName with
        | null -> t.Name
        | n -> n.Replace('+', '.')

let private flags =
    BindingFlags.Public ||| BindingFlags.Instance ||| BindingFlags.Static ||| BindingFlags.DeclaredOnly

let private describeMember (m: MemberInfo) =
    match m with
    | :? ConstructorInfo as c ->
        Some (sprintf "  new(%s)" (c.GetParameters() |> Array.map (fun p -> typeName p.ParameterType) |> String.concat ", "))
    | :? MethodInfo as mi when not mi.IsSpecialName ->
        let ps = mi.GetParameters() |> Array.map (fun p -> sprintf "%s: %s" p.Name (typeName p.ParameterType)) |> String.concat ", "
        Some (sprintf "  %s%s(%s) : %s" (if mi.IsStatic then "static " else "") mi.Name ps (typeName mi.ReturnType))
    | :? PropertyInfo as p ->
        let acc = (if p.CanRead && p.GetMethod.IsPublic then "get" else "") + (if p.CanWrite && p.SetMethod <> null && p.SetMethod.IsPublic then " set" else "")
        let static' = if (p.GetMethod <> null && p.GetMethod.IsStatic) then "static " else ""
        Some (sprintf "  %sprop %s : %s { %s }" static' p.Name (typeName p.PropertyType) (acc.Trim()))
    | _ -> None

let rec private describeType (t: Type) : string list =
    let header = sprintf "type %s" (typeName t)
    let members =
        t.GetMembers(flags)
        |> Array.choose describeMember
        |> Array.sort
        |> Array.toList
    let nested =
        t.GetNestedTypes(BindingFlags.Public)
        |> Array.sortBy (fun n -> n.FullName)
        |> Array.toList
        |> List.collect describeType
    header :: members @ nested

/// The engine layer's public surface, one line per member, stable across runs.
let engineApiSurface () =
    let asm = typeof<ChessLibrary.Engine.ChessEngine>.Assembly
    [ "ChessLibrary.Engine"; "ChessLibrary.EngineHelper"; "ChessLibrary.HardwareInfo" ]
    |> List.collect (fun name -> describeType (asm.GetType(name, true)))
    |> String.concat "\n"

[<Fact>]
let ``The engine layer's public API matches the pinned surface`` () =
    let expectedPath = Path.Combine(AppContext.BaseDirectory, "TestData", "EngineApi.txt")
    let expected = File.ReadAllText(expectedPath).Replace("\r\n", "\n").TrimEnd()
    let actual = engineApiSurface ()
    if expected <> actual then
        let dump = Path.Combine(AppContext.BaseDirectory, "EngineApi.actual.txt")
        File.WriteAllText(dump, actual)
        Assert.Fail(sprintf "The engine API changed. Current surface written to %s - diff it against TestData/EngineApi.txt." dump)
