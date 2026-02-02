module ChessLibrary.UciOption

open System.Collections.Generic
open System

type UciOptionType =
    | Check of bool  // default value
    | Spin of int64 * int64 * int64  // (min, max, default)
    | Combo of string list * string  // (options, default)
    | Button
    | String of string  // default value
    | IdAndAuthor of string * string * string
    | Unknown

type UciOption = {
    Name: string
    OptionType: UciOptionType
}

let parseIdAndAuthor (input:string) =
    // Split the input string by spaces (ignore empty entries)
    let parts = input.Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries)
    if parts.Length < 3 then None
    else
        let id = parts.[0]
        let name = parts.[1]
        let restOfString = String.Join(" ", parts.[2..])
        Some (id, name, restOfString)

let parseOption (line: string) =
    let parts = line.Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries)

    // case-insensitive keyword search
    let tryFindIndex (keyword: string) =
        parts |> Array.tryFindIndex (fun part -> part.Equals(keyword, StringComparison.OrdinalIgnoreCase))

    let getValue (keyword: string) =
        match tryFindIndex keyword with
        | Some index when index + 1 < parts.Length -> Some parts.[index + 1]
        | _ -> None

    let getAllValues (keyword: string) =
        match tryFindIndex keyword with
        | Some index when index + 1 < parts.Length ->
            parts.[index + 1..] |> String.concat " " |> Some
        | _ -> None

    let getValuesAfter (keyword: string) =
        match tryFindIndex keyword with
        | Some index when index + 1 < parts.Length -> Some (parts.[index + 1 ..] |> Array.toList)
        | _ -> None

    let tryParseInt64 (s:string) =
        match Int64.TryParse s with
        | true, v -> Some v
        | _ -> None

    let name =
        match tryFindIndex "name", tryFindIndex "type" with
        | Some ni, Some ti when ni + 1 < ti -> String.Join(" ", parts.[ni + 1 .. ti - 1])
        | Some ni, None when ni + 1 < parts.Length -> String.Join(" ", parts.[ni + 1 ..])
        | _ -> ""

    let optionType =
        match getValue "type" with
        | Some t when t.Equals("check", StringComparison.OrdinalIgnoreCase) ->
            let defaultVal = getValue "default" |> Option.map (fun v -> v.Equals("true", StringComparison.OrdinalIgnoreCase)) |> Option.defaultValue false
            Check defaultVal
        | Some t when t.Equals("spin", StringComparison.OrdinalIgnoreCase) ->
            let min = getValue "min" |> Option.bind tryParseInt64 |> Option.defaultValue 0L
            let max = getValue "max" |> Option.bind tryParseInt64 |> Option.defaultValue 0L
            let defaultVal = getValue "default" |> Option.bind tryParseInt64 |> Option.defaultValue 0L
            Spin (min, max, defaultVal)
        | Some t when t.Equals("combo", StringComparison.OrdinalIgnoreCase) ->
            let defaultVal = getValue "default" |> Option.defaultValue ""
            let values = getValuesAfter "var" |> Option.defaultValue []
            Combo (values, defaultVal)
        | Some t when t.Equals("button", StringComparison.OrdinalIgnoreCase) -> Button
        | Some t when t.Equals("string", StringComparison.OrdinalIgnoreCase) ->
            let defaultVal = getAllValues "default" |> Option.defaultValue ""
            String defaultVal
        | _ -> Unknown

    match optionType with
    | Unknown -> None
    | _ -> Some { Name = name; OptionType = optionType }

let addOptionToMap (optionsMap:Dictionary<string, UciOption>) (line: string) =
    if not (String.IsNullOrEmpty line) then
        match parseOption line with
        | Some option -> optionsMap.[option.Name] <- option
        | None ->
            match parseIdAndAuthor line with
            | Some (id, name, rest) when id.Equals("id", StringComparison.OrdinalIgnoreCase) ->
                let optionId = { Name = id + name; OptionType = UciOptionType.IdAndAuthor(id, name, rest) }
                optionsMap.[name] <- optionId
            | _ -> ()

// helper for case-insensitive equality
let equalsCI (a:string) (b:string) = String.Equals(a, b, StringComparison.OrdinalIgnoreCase)

let tryFindOption (optionsMap: Dictionary<string, UciOption>) (name: string) =
    optionsMap
    |> Seq.tryFind (fun kvp -> equalsCI kvp.Key name)
    |> Option.map _.Value


// Function to validate an option (case-insensitive value checks; preserve provided value text)
let validateOption (optionsMap:Dictionary<string, UciOption>) (name: string, value: string) =
    match optionsMap.TryGetValue(name) with
    | true, option ->
        match option.OptionType with
        | Check _ -> equalsCI value "true" || equalsCI value "false"
        | Spin (min, max, _) ->
            match System.Int64.TryParse(value) with
            | true, intValue -> intValue >= min && intValue <= max
            | _ -> false
        | Combo (options, _) -> options |> List.exists (fun v -> v.Equals(value, StringComparison.OrdinalIgnoreCase))
        | Button -> true
        | String _ -> true
        | _ -> false
    | _ -> false

let parseSetOptionCommand (command: string) =
    let parts = command.Trim().Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries)
    if parts.Length < 3
       || not (parts.[0].Equals("setoption", StringComparison.OrdinalIgnoreCase))
       || not (parts.[1].Equals("name", StringComparison.OrdinalIgnoreCase)) then
        None    
    else    
        let nameStart = 2
        let valueIndex = parts |> Array.tryFindIndex (fun t -> t.Equals("value", StringComparison.OrdinalIgnoreCase))
        match valueIndex with
        | Some vi when vi > nameStart ->
            let name = parts.[nameStart..(vi - 1)] |> String.concat " "
            let value = parts.[vi + 1..] |> String.concat " "
            // preserve original casing for both name and value
            Some (name, value)
        | _ ->
            let name = parts.[nameStart..] |> String.concat " "
            Some (name, String.Empty)
    
let validateSetOption  (optionsMap:Dictionary<string, UciOption>) (name: string, value: string) =
    match optionsMap.TryGetValue(name) with
    | true, option ->
        match option.OptionType with
        | Check _ -> equalsCI value "true" || equalsCI value "false"
        | Spin (min, max, _) ->
            match System.Int64.TryParse(value) with
            | true, intValue -> intValue >= min && intValue <= max
            | _ -> false
        | Combo (legalValues, _) -> legalValues |> List.exists (fun v -> v.Equals(value, StringComparison.OrdinalIgnoreCase))
        | Button -> false
        | String _ -> true
        | _ -> false
    | _ -> false

let getNoneDefaultSetOption  (optionsMap:Dictionary<string, UciOption>) (name: string, value: string) =
    // preserve 'value' exactly as provided; do case-insensitive comparisons against defaults
    match optionsMap.TryGetValue(name) with
    | true, option ->
        match option.OptionType with
        | Check v ->
            if not (String.Equals(v.ToString(), value, StringComparison.OrdinalIgnoreCase)) then Some (name, v.ToString(), value) else None
        | Spin (min, max, v) ->
            match System.Int64.TryParse(value) with
            | true, intValue -> if intValue <> v then Some (name, v.ToString(), value) else None
            | _ -> None
        | Combo (legalValues, v) ->
            if not (String.Equals(v, value, StringComparison.OrdinalIgnoreCase)) then Some (name, v, value) else None
        | String v ->
            if not (String.Equals(v, value, StringComparison.OrdinalIgnoreCase)) then Some (name, v, value) else None
        | _ -> None
    | _ -> None