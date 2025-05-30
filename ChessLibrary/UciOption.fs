module ChessLibrary.UciOption

open System.Collections.Generic
open System

type UciOptionType =
    | Check of bool  // default value
    | Spin of int * int * int  // (min, max, default)
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
    // Split the input string by spaces
    let parts = input.Split(' ')

    // Check if the input has at least three parts (id, name, and something else)
    if parts.Length < 3 then
        // Return the original input string if there are not enough parts
        None
    else
        // Extract the id and name
        let id = parts.[0]
        let name = parts.[1]

        // Get the rest of the string
        let restOfString = String.Join(" ", parts.[2..])

        // Return the id, name, and the rest of the string as Some tuple
        Some (id, name, restOfString)

let parseOption (line: string) =
    
    let parts = line.Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries)

    let tryFindIndex (keyword: string) =
        parts |> Array.tryFindIndex (fun part -> part = keyword)

    let getValue (keyword: string) =
        match tryFindIndex keyword with
        | Some index when index + 1 < parts.Length -> Some parts.[index + 1]
        | _ -> None

    let getAllValues (keyword: string) =
        match tryFindIndex keyword with
        | Some index when index + 1 < parts.Length -> 
            let res = parts.[index + 1..]
            //concat all values after keyword
            res |> String.concat " " |> Some
        | _ -> None

    let getValuesAfter (keyword: string) =
        match tryFindIndex keyword with
        | Some index when index + 1 < parts.Length -> Some (parts.[index + 1 ..] |> Array.toList)
        | _ -> None

    let name =
        match tryFindIndex "name", tryFindIndex "type" with
        | Some ni, Some ti when ni + 1 < ti -> String.Join(" ", parts.[ni + 1 .. ti - 1])
        | _ -> ""

    let optionType =        
          match getValue "type" with
          | Some "check" ->
              let defaultVal = getValue "default" |> Option.map (fun v -> v = "true") |> Option.defaultValue false
              Check defaultVal
          | Some "spin" ->
              let min = getValue "min" |> Option.map int |> Option.defaultValue 0
              let max = getValue "max" |> Option.map int |> Option.defaultValue 0
              let defaultVal = getValue "default" |> Option.map int |> Option.defaultValue 0
              Spin (min, max, defaultVal)
          | Some "combo" ->
              let defaultVal = getValue "default" |> Option.defaultValue ""
              let values = getValuesAfter "var" |> Option.defaultValue []
              Combo (values, defaultVal)
          | Some "button" -> Button
          | Some "string" ->
              let defaultVal = getAllValues "default" |> Option.defaultValue ""
              String defaultVal
          | _ -> Unknown        
    match optionType with
    | Unknown -> None
    | _ -> Some { Name = name; OptionType = optionType }


let addOptionToMap (optionsMap:Dictionary<string, UciOption>) (line: string) =
    if String.IsNullOrEmpty line |> not then
      let option = parseOption line
      match option with
      | Some option -> optionsMap.[option.Name] <- option
      | None -> 
         match parseIdAndAuthor line with
         | Some (id, name, rest) when id = "id" -> 
            let optionId = { Name = id+name; OptionType = UciOptionType.IdAndAuthor(id,name,rest) }
            optionsMap.[name] <- optionId
         | _ -> ()   


// Function to query an option
let getOption (optionsMap:Dictionary<string, UciOption>) (name: string) =
    if optionsMap.ContainsKey(name) then
        Some (optionsMap.[name])
    else
        None

let tryFindOption (optionsMap: Dictionary<string, UciOption>) (name: string) =
    optionsMap 
    |> Seq.tryFind (fun kvp -> kvp.Key.Contains(name) || kvp.Key.ToLower().Contains(name.ToLower()))
    |> Option.map _.Value

// Function to validate an option
let validateOption (optionsMap:Dictionary<string, UciOption>) (name: string, value: string) =
    match getOption optionsMap name with
    | Some option ->
        match option.OptionType with
        | Check _ -> value = "true" || value = "false"
        | Spin (min, max, _) -> 
            let intValue = int value
            intValue >= min && intValue <= max
        | Combo (options, _) -> List.contains value options
        | Button -> true  // No validation needed for button
        | String _ -> true  // No specific validation for strings
        | _ -> false
    | None -> false


let parseSetOptionCommand (command: string) =
    let parts = command.Split([| ' ' |], System.StringSplitOptions.RemoveEmptyEntries)
    if parts.[0] = "setoption" && parts.[1] = "name" then
        let nameIndex = 2
        let valueIndex = Array.findIndex (fun part -> part = "value") parts
        let name = System.String.Join(" ", parts.[nameIndex..valueIndex-1])
        let value = System.String.Join(" ", parts.[valueIndex+1..])
        Some (name, value)
    else
        None

let validateSetOption  (optionsMap:Dictionary<string, UciOption>) (name: string, value: string) =
    match optionsMap.TryGetValue(name) with
    | true, option ->
        match option.OptionType with
        | Check _ -> value = "true" || value = "false"
        | Spin (min, max, _) -> 
            match System.Int32.TryParse(value) with
            | true, intValue -> intValue >= min && intValue <= max
            | _ -> false
        | Combo (legalValues, _) -> List.contains value legalValues
        | Button -> false  // Button can't be set via setoption
        | String _ -> true  // Assume any string is valid for simplicity
        | _ -> false
    | _ -> false


let getNoneDefaultSetOption  (optionsMap:Dictionary<string, UciOption>) (name: string, value: string) =
    match optionsMap.TryGetValue(name) with
    | true, option ->
        match option.OptionType with
        | Check v -> if v.ToString().ToLower() <> value.ToLower() then Some (name, v.ToString(), value) else None
        | Spin (min, max, v) -> 
            match System.Int32.TryParse(value) with
            | true, intValue -> if v.ToString().ToLower() <> value.ToLower() then Some (name, v.ToString(), value) else None
            | _ -> None
        | Combo (legalValues, v) -> if v.ToString().ToLower() <> value.ToLower() then Some (name, v, value) else None        
        | String v -> if v.ToString().ToLower() <> value.ToLower() then Some (name, v, value) else None
        | _ -> None
    | _ -> None


