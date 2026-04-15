module ChessLibrary.Scheduler.Shared

open System
open ChessLibrary.TypesDef.CoreTypes
open ChessLibrary.TypesDef.Tournament
open ChessLibrary.PGNTypes
open ChessLibrary.RuntimeUtilities

/// Salt composed from the PGN output path and the sorted engine names.
/// Preserved for backward compatibility with any caller that wants
/// salt-based shuffling explicitly; the tournament runners no longer use
/// this (they pass `Opening.Seed` directly to `shuffleOpeningsWithSeed`).
let tournamentSalt (pgnOutPath: string) (engines: EngineConfig list) =
    let names = engines |> List.map (fun e -> e.Name) |> List.sort |> String.concat ","
    pgnOutPath + "|" + names

/// Deterministic shuffle derived from an arbitrary salt string (MD5-seeded RNG).
let shuffleOpenings (salt: string) (openings: PgnGame list) =
    let arr = openings |> List.toArray
    let bytes = System.Text.Encoding.UTF8.GetBytes($"{arr.Length}|{salt}")
    let hashBytes = System.Security.Cryptography.MD5.HashData(bytes)
    let seed = abs (BitConverter.ToInt32(hashBytes, 0))
    let rng = Random(seed)
    rng.Shuffle(arr)
    arr |> Array.toList

/// Deterministic shuffle from an explicit integer seed. When `Opening.Seed`
/// is left at its default (0), callers get a fixed, reproducible order;
/// setting any other value in tournament.json picks a different permutation.
let shuffleOpeningsWithSeed (seed: int) (openings: PgnGame list) =
    let arr = openings |> List.toArray
    let rng = Random(seed)
    rng.Shuffle(arr)
    arr |> Array.toList

/// Apply the tournament's opening shuffle using `Opening.Seed`. Logs the
/// effective seed so resume behavior is transparent.
let shuffleOpeningsForTournament (opening: Opening) (openings: PgnGame list) =
    ConsoleUtils.printInColor ConsoleColor.Cyan
        (sprintf "Opening shuffle: using Opening.Seed = %d (stable across engine-list changes)." opening.Seed)
    shuffleOpeningsWithSeed opening.Seed openings

/// Move the head element to the tail. Used for per-round opponent rotation
/// in Gauntlet when PreventMoveDeviation is enabled.
let rotateListByOne (lst: 'a list) : 'a list =
    match lst with
    | [] -> []
    | head :: tail -> tail @ [ head ]

/// Berger-style rotation: the first element stays in place; the rest rotates.
/// Used by Round Robin pairing.
let rotateOnce (players: 'a list) : 'a list =
    match players with
    | [] -> []
    | [ x ] -> [ x ]
    | head :: tail ->
        match List.rev tail with
        | [] -> players
        | last :: revRest ->
            let rest = List.rev revRest
            head :: last :: rest
