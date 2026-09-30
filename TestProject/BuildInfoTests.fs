/// BuildInfo: the version release.yml stamps and the commit the SDK adds, as `match -version`,
/// the WebGUI's startup line and the desktop shell's About show them.
module BuildInfoTests

open System.Text.RegularExpressions
open Xunit
open ChessLibrary

[<Fact>]
let ``describe is a version or dev build, with the short commit when known`` () =
    let d = BuildInfo.describe ()
    Assert.Matches(Regex(@"^(dev build|\d+\.\d+\.\d+(-[0-9A-Za-z.-]+)?)( \([0-9a-f]{1,7}\))?$"), d)
    // a test build carries no version: the SDK's 1.0.0 must not pass for one
    Assert.Equal(None, BuildInfo.version)
    Assert.StartsWith("dev build", d)
