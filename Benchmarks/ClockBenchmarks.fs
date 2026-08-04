namespace Benchmarks

open System
open BenchmarkDotNet.Attributes

/// Is TimeOnly actually cheaper than TimeSpan for the game clock?
///
/// Both are readonly structs wrapping a single int64 of ticks, so neither allocates and
/// both are eight bytes. The difference is in the operations: TimeOnly's constructor
/// validates that the value is inside a day, and its AddTicks wraps modulo 24 hours, which
/// is two extra divisions. TimeSpan's operators are a plain add with an overflow check.
///
/// The shapes below mirror GameHelpers.updateClocks — remaining + increment - measured
/// elapsed — because that is the only place a clock value is computed, and it runs once per
/// move. A game is roughly a hundred of them, which is the unit used here: a single struct
/// operation is far below BenchmarkDotNet's resolution and would measure the loop instead.
module ClockBenchmarks =

    /// A plausible move time, taken as measured elapsed from a Stopwatch.
    let private elapsed = TimeSpan.FromMilliseconds 850.0
    let private incrementTicks = TimeSpan.FromSeconds(2.0).Ticks
    let private startTicks = TimeSpan.FromMinutes(10.0).Ticks

    [<MemoryDiagnoser>]
    type ClockUpdateBenchmarks() =

        [<Params(100)>]
        member val Moves = 0 with get, set

        /// Exactly what GameHelpers.updateClocks does today: tick arithmetic, clamp at zero
        /// because TimeOnly cannot hold a negative, then back through the validating ctor.
        [<Benchmark(Baseline = true)>]
        member this.TimeOnly_AsWrittenToday() =
            let mutable remaining = TimeOnly startTicks
            for _ in 1 .. this.Moves do
                let ticks = max (remaining.Ticks + incrementTicks - elapsed.Ticks) 0L
                remaining <- TimeOnly ticks
            remaining.Ticks

        /// The idiomatic TimeOnly API, which is what you reach for if you are not thinking
        /// in ticks. Add is where the wrap-around modulo lives.
        [<Benchmark>]
        member this.TimeOnly_Add() =
            let delta = TimeSpan(incrementTicks - elapsed.Ticks)
            let mutable remaining = TimeOnly startTicks
            for _ in 1 .. this.Moves do
                remaining <- remaining.Add delta
            remaining.Ticks

        /// What the same line becomes with a duration type. No clamp: running past zero is
        /// representable, which is the correctness reason for the change rather than a
        /// speed one.
        [<Benchmark>]
        member this.TimeSpan_Natural() =
            let increment = TimeSpan incrementTicks
            let mutable remaining = TimeSpan startTicks
            for _ in 1 .. this.Moves do
                remaining <- remaining + increment - elapsed
            remaining.Ticks

        /// Clamped as well, so the comparison against the current code is like for like.
        /// Compares Ticks rather than the structs: F#'s generic `<` boxes a struct that
        /// implements IComparable, which allocated 48 bytes per iteration here and measured
        /// the language idiom instead of the type.
        [<Benchmark>]
        member this.TimeSpan_Clamped() =
            let increment = TimeSpan incrementTicks
            let mutable remaining = TimeSpan startTicks
            for _ in 1 .. this.Moves do
                let next = remaining + increment - elapsed
                remaining <- if next.Ticks < 0L then TimeSpan.Zero else next
            remaining.Ticks

        /// Same tick arithmetic as the current code, only the wrapper type differs — this is
        /// what isolates the type from the style the two happen to be written in.
        [<Benchmark>]
        member this.TimeSpan_SameStyleAsToday() =
            let mutable remaining = TimeSpan startTicks
            for _ in 1 .. this.Moves do
                let ticks = max (remaining.Ticks + incrementTicks - elapsed.Ticks) 0L
                remaining <- TimeSpan ticks
            remaining.Ticks

        /// Raw int64 milliseconds — the floor any wrapper is measured against.
        [<Benchmark>]
        member this.Int64Milliseconds() =
            let increment = 2000L
            let elapsedMs = 850L
            let mutable remaining = 600_000L
            for _ in 1 .. this.Moves do
                remaining <- max (remaining + increment - elapsedMs) 0L
            remaining
