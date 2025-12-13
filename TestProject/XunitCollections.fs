namespace TestProject

open Xunit

[<CollectionDefinition("NonParallelParser", DisableParallelization = true)>]
type NonParallelParserCollection() =
  class
  end

