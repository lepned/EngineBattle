namespace ChessLibrary

open System.Reflection

/// Which EngineBattle this is: the version the release stamps into every assembly
/// (release.yml publishes with -p:Version from the tag) and the commit the SDK adds to the
/// informational version ("1.8.2+<sha>"). A build without a version - a local build - carries the
/// SDK's 1.0.0, which says nothing, so it reads as a dev build.
module BuildInfo =

  type private Marker = class end

  let private informational =
    let a = typeof<Marker>.Assembly.GetCustomAttribute<AssemblyInformationalVersionAttribute>()
    if isNull a then "" else a.InformationalVersion

  /// The release version ("1.8.2", "1.9.0-rc1"), or None for a build without one.
  let version =
    let v = match informational.IndexOf '+' with | -1 -> informational | i -> informational.Substring(0, i)
    if v = "" || v = "1.0.0" then None else Some v

  /// The commit it was built from, short, when the build knows it.
  let commit =
    match informational.IndexOf '+' with
    | -1 -> None
    | i ->
      let sha = informational.Substring(i + 1)
      if sha = "" then None else Some(if sha.Length > 7 then sha.Substring(0, 7) else sha)

  /// "1.8.2 (abc1234)", "dev build (abc1234)", "1.8.2" or "dev build".
  let describe () =
    let v = defaultArg version "dev build"
    match commit with
    | Some c -> $"{v} ({c})"
    | None -> v
