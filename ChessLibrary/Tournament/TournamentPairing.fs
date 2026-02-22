module ChessLibrary.TournamentPairing
open System
open System.Text
open System.Collections.Generic
open Microsoft.Extensions.Logging

open PGNTypes
open TypesDef.CoreTypes
open ChessUtilities

module PairingHelper =

  /// Build a salt that is unique per tournament (output path + sorted engine names)
  /// and stable on resume (neither changes between restarts).
  let tournamentSalt (pgnOutPath: string) (engines: EngineConfig list) =
    let names = engines |> List.map (fun e -> e.Name) |> List.sort |> String.concat ","
    pgnOutPath + "|" + names

  /// Shuffle openings with a deterministic seed so resume produces the same permutation.
  /// The salt (e.g. from tournamentSalt) makes the seed unique per tournament, so different
  /// tournaments using the same opening book get different shuffles.
  let shuffleOpenings (salt: string) (openings: PGNTypes.PgnGame list) =
    let arr = openings |> List.toArray
    let bytes = System.Text.Encoding.UTF8.GetBytes($"{arr.Length}|{salt}")
    let hashBytes = System.Security.Cryptography.MD5.HashData(bytes)
    let seed = abs (BitConverter.ToInt32(hashBytes, 0))
    let rng = Random(seed)
    rng.Shuffle(arr)
    arr |> Array.toList

  type CupSeedingStrategy =
    | Random
    | ByRating

  let tcecSeedOrder (players: EngineConfig list) (groupCount: int) =
    let sorted = players |> List.sortByDescending (fun p -> p.Rating)
    let total = sorted.Length
    if total = 0 then
      []
    else
      let groups = Dictionary<int, ResizeArray<EngineConfig>>()
      let numGroups = Math.Max(1, groupCount)
      for i in 0 .. numGroups - 1 do
        groups.[i] <- ResizeArray<EngineConfig>()
      let mutable engineIndex = 0
      for group in 0 .. numGroups - 1 do
        let groupSize = int (Math.Ceiling(float (total - engineIndex) / float (numGroups - group)))
        for _ in 0 .. groupSize - 1 do
          if engineIndex < total then
            groups.[group].Add(sorted.[engineIndex])
            engineIndex <- engineIndex + 1
      let ordered = ResizeArray<EngineConfig>()
      let mutable rank = 0
      let mutable seed = 1
      while seed <= total do
        for group in 0 .. numGroups - 1 do
          if rank < groups.[group].Count then
            ordered.Add(groups.[group].[rank])
            seed <- seed + 1
            if seed > total then
              ()
        rank <- rank + 1
      ordered |> Seq.toList

  let seedOrder (size: int) =
    let rec build order n =
      if n >= size then
        order
      else
        let nextSize = n * 2
        let expanded =
          order
          |> List.collect (fun seed -> [ seed; (nextSize + 1 - seed) ])
        build expanded nextSize
    if size <= 1 then
      [ 1 ]
    else
      build [ 1; 2 ] 2

  let autoSeedBands (total: int) =
    if total <= 0 then
      []
    else
      let bands = ResizeArray<int list>()
      let mutable seed = 1
      bands.Add([ seed ])
      seed <- seed + 1
      if seed <= total then
        bands.Add([ seed ])
        seed <- seed + 1
      let mutable bandSize = 2
      while seed <= total do
        let size = Math.Min(bandSize, total - seed + 1)
        bands.Add([ seed .. seed + size - 1 ])
        seed <- seed + size
        bandSize <- bandSize * 2
      bands |> Seq.toList

  let seedByBands (players: EngineConfig list) (seedBands: int list list) (randomizeWithinBands: bool) =
    let sorted = players |> List.sortByDescending (fun p -> p.Rating)
    let total = sorted.Length
    if total = 0 then
      []
    else
      let order = seedOrder total
      let slots : EngineConfig option array = Array.create total None
      let used = System.Collections.Generic.HashSet<int>()
      let tryPlace seedNumber =
        if seedNumber >= 1 && seedNumber <= total && used.Contains seedNumber |> not then
          let slotIndex = order |> List.findIndex (fun s -> s = seedNumber)
          slots.[slotIndex] <- Some sorted.[seedNumber - 1]
          used.Add seedNumber |> ignore
      let tryPlaceBand (band: int list) =
        let seeds =
          band
          |> List.filter (fun seedNumber -> seedNumber >= 1 && seedNumber <= total && used.Contains seedNumber |> not)
        if seeds.Length = 0 then
          ()
        elif seeds.Length = 1 || randomizeWithinBands |> not then
          for seedNumber in seeds do
            tryPlace seedNumber
        else
          let slotIndices =
            seeds
            |> List.map (fun seedNumber -> order |> List.findIndex (fun s -> s = seedNumber))
          let seedPlayers = seeds |> List.map (fun seedNumber -> sorted.[seedNumber - 1])
          let shuffled = seedPlayers |> List.toArray
          Random.Shuffle(shuffled)
          for i in 0 .. slotIndices.Length - 1 do
            slots.[slotIndices.[i]] <- Some shuffled.[i]
          for seedNumber in seeds do
            used.Add seedNumber |> ignore
      for band in seedBands do
        tryPlaceBand band
      let remaining =
        sorted
        |> List.mapi (fun idx player -> (idx + 1, player))
        |> List.filter (fun (seedNumber, _) -> used.Contains seedNumber |> not)
        |> List.map snd
      let mutable remainingIndex = 0
      for i in 0 .. total - 1 do
        if slots.[i].IsNone then
          slots.[i] <- Some remaining.[remainingIndex]
          remainingIndex <- remainingIndex + 1
      slots |> Array.choose id |> Array.toList

  let gamesPerMatchForRound (gamesPerMatch: int) (roundPairIncrements: int list) (roundNumber: int) =
    let normalized =
        if gamesPerMatch < 2 then 2
        elif gamesPerMatch % 2 = 1 then gamesPerMatch + 1
        else gamesPerMatch
    match roundPairIncrements with
    | [] -> normalized
    | _ ->
        let idx = Math.Max(0, roundNumber - 1)
        let pairs =
            if idx < roundPairIncrements.Length then roundPairIncrements.[idx]
            else roundPairIncrements.[roundPairIncrements.Length - 1]
        let pairs = Math.Max(1, pairs)
        pairs * 2

  let nextUnusedOpeningIndex (usedOpeningHashes: Set<string>) (openings: PGNTypes.PgnGame list) (startIndex: int) =
    if openings.IsEmpty then
        0
    else
        let total = openings.Length
        let inline openingHash (opening: PGNTypes.PgnGame) = Hash.computeOpeningHashFromGame opening
        let rec loop offset =
            if offset >= total then
                startIndex % total
            else
                let idx = (startIndex + offset) % total
                let hash = openingHash openings.[idx]
                if usedOpeningHashes.Contains hash then
                    loop (offset + 1)
                else
                    idx
        loop 0

  let buildRemainingCupPairings
    (matchInfo: CupTypes.CupMatch)
    (playerA: EngineConfig)
    (playerB: EngineConfig)
    (matchOpenings: PGNTypes.PgnGame list)
    (currentOpening: PGNTypes.PgnGame)
    (currentPlayOrder: (EngineConfig * EngineConfig) list)
    (gamesRemaining: int)
    (localOpeningIndex: int) =
      let baseIndex = matchInfo.Games.Count
      let mutable remaining = gamesRemaining
      let mutable nextIndex = localOpeningIndex
      let planned = ResizeArray<Pairing>()
      let addPairing idx white black opening =
        let openingHash = Hash.computeOpeningHashFromGame opening
        planned.Add
          { Opening = opening
            White = white
            Black = black
            GameNr = 0
            RoundNr = $"{matchInfo.RoundNumber}.{baseIndex + idx + 1}"
            OpeningHash = openingHash }
      let mutable usedHashes =
        matchInfo.Games
        |> Seq.map (fun g -> g.OpeningHash)
        |> Set.ofSeq
      let currentHash = Hash.computeOpeningHashFromGame currentOpening
      usedHashes <- usedHashes.Add currentHash
      let mutable idxOffset = 0
      for (white, black) in currentPlayOrder do
        if remaining > 0 then
          addPairing idxOffset white black currentOpening
          idxOffset <- idxOffset + 1
          remaining <- remaining - 1
      let chooseNextOpening () =
        if List.isEmpty matchOpenings then
          currentOpening
        elif usedHashes.Count < matchOpenings.Length then
          let idx = nextUnusedOpeningIndex usedHashes matchOpenings nextIndex
          nextIndex <- idx + 1
          matchOpenings.[idx]
        else
          let idx = nextIndex % matchOpenings.Length
          nextIndex <- idx + 1
          matchOpenings.[idx]
      while remaining > 0 do
        let opening = chooseNextOpening ()
        let openingHash = Hash.computeOpeningHashFromGame opening
        usedHashes <- usedHashes.Add openingHash
        for (white, black) in [ (playerA, playerB); (playerB, playerA) ] do
          if remaining > 0 then
            addPairing idxOffset white black opening
            idxOffset <- idxOffset + 1
            remaining <- remaining - 1
      planned

  let swissPairKey (a: string) (b: string) =
    if String.CompareOrdinal(a, b) <= 0 then
      $"{a}|{b}"
    else
      $"{b}|{a}"

  let swissRoundPairingsGroupedOnly
    (players: EngineConfig list)
    (seedOrder: EngineConfig list)
    (scores: Map<string, float>)
    (priorPairs: Set<string>)
    (byeSet: Set<string>) =
      let seedMap =
        seedOrder
        |> List.mapi (fun idx p -> p.Name, idx + 1)
        |> Map.ofList
      let scoreFor name =
        scores |> Map.tryFind name |> Option.defaultValue 0.0
      // Bye selection: Choose the player who should receive the bye this round.
      // Priority: (1) Lowest score first, (2) Among ties, weakest player (highest seed number).
      // This ensures the bye goes to a trailing engine, and among equals, the lower-rated one.
      // Players who have already received a bye are skipped when possible.
      let byeCandidate =
        if players.Length % 2 = 0 then
          None
        else
          let ordered =
            players
            |> List.sortBy (fun p ->
                let seed = seedMap.[p.Name]
                scoreFor p.Name, -seed)  // -seed: higher seed number (weaker) sorts first
          let preferred =
            ordered |> List.tryFind (fun p -> byeSet.Contains p.Name |> not)
          preferred |> Option.orElse (ordered |> List.tryHead)
      let (pairingPlayers: EngineConfig list), byePlayer =
        match byeCandidate with
        | None -> players, None
        | Some bye -> players |> List.filter (fun p -> p.Name <> bye.Name), Some bye
      let groups: (float * EngineConfig list) list =
        pairingPlayers
        |> List.groupBy (fun p -> scoreFor p.Name)
        |> List.sortByDescending fst
      let rec matchTopBottom
        (score: float)
        (top: EngineConfig list)
        (bottom: EngineConfig list)
        (acc: (EngineConfig * EngineConfig * float) list) =
          match top with
          | [] -> Some (List.rev acc)
          | (player: EngineConfig) :: rest ->
              let rec tryOpps (options: EngineConfig list) =
                match options with
                | [] -> None
                | (opp: EngineConfig) :: tail ->
                    if priorPairs.Contains (swissPairKey player.Name opp.Name) then
                      tryOpps tail
                    else
                      let nextBottom = bottom |> List.filter (fun p -> p.Name <> opp.Name)
                      match matchTopBottom score rest nextBottom ((player, opp, score) :: acc) with
                      | Some pairs -> Some pairs
                      | None -> tryOpps tail
              tryOpps bottom
      let rec matchGroup
        (remaining: EngineConfig list)
        (acc: (EngineConfig * EngineConfig * float) list) =
          match remaining with
          | [] -> Some (List.rev acc)
          | (player: EngineConfig) :: rest ->
              let rec tryOpps (options: EngineConfig list) =
                match options with
                | [] -> None
                | (opp: EngineConfig) :: tail ->
                    if priorPairs.Contains (swissPairKey player.Name opp.Name) then
                      tryOpps tail
                    else
                      let nextRemaining = rest |> List.filter (fun p -> p.Name <> opp.Name)
                      match matchGroup nextRemaining ((player, opp, scoreFor player.Name) :: acc) with
                      | Some pairs -> Some pairs
                      | None -> tryOpps tail
              tryOpps rest
      let buildGroupedPairs () =
        let mutable carry = List.empty<EngineConfig>
        let groupedPairs = ResizeArray<(EngineConfig * EngineConfig * float)>()
        for (score, (groupPlayers: EngineConfig list)) in groups do
          let group: EngineConfig list =
            (carry @ groupPlayers)
            |> List.sortBy (fun p -> seedMap.[p.Name])
          carry <- []
          let mutable groupList: EngineConfig list = group
          if groupList.Length % 2 = 1 then
            carry <- [ groupList.[groupList.Length - 1] ]
            groupList <- groupList |> List.take (groupList.Length - 1)
          let half = groupList.Length / 2
          let top: EngineConfig list = groupList |> List.take half
          let bottom: EngineConfig list = groupList |> List.skip half
          let matched =
            match matchTopBottom score top bottom [] with
            | Some pairs -> pairs
            | None ->
                match matchGroup groupList [] with
                | Some pairs -> pairs
                | None -> failwith "Swiss pairing failed: no valid non-repeat pairings found for this round."
          for pair in matched do
            groupedPairs.Add pair
        if carry.Length > 0 then
          let remaining = carry
          if remaining.Length >= 2 then
            match matchGroup remaining [] with
            | Some pairs ->
                for pair in pairs do
                  groupedPairs.Add pair
            | None -> failwith "Swiss pairing failed: no valid non-repeat pairings found for this round."
        groupedPairs
      let roundPairs = buildGroupedPairs ()
      match byePlayer with
      | Some bye ->
          let byeEngine = { EngineConfig.Empty with Name = "BYE" }
          roundPairs.Add((bye, byeEngine, scoreFor bye.Name))
      | None -> ()
      let seedFor name =
        seedMap |> Map.tryFind name |> Option.defaultValue Int32.MaxValue
      roundPairs
      |> Seq.sortBy (fun (a, b, score) ->
          let seedA = seedFor a.Name
          let seedB = seedFor b.Name
          let minSeed = if seedA < seedB then seedA else seedB
          score, -minSeed)
      |> Seq.map (fun (a, b, _) -> (a, b))
      |> Seq.toList

  let swissRoundPairings
    (players: EngineConfig list)
    (seedOrder: EngineConfig list)
    (scores: Map<string, float>)
    (priorPairs: Set<string>)
    (byeSet: Set<string>) =
      try
        swissRoundPairingsGroupedOnly players seedOrder scores priorPairs byeSet
      with _ ->
        let seedMap =
          seedOrder
          |> List.mapi (fun idx p -> p.Name, idx + 1)
          |> Map.ofList
        let scoreFor name =
          scores |> Map.tryFind name |> Option.defaultValue 0.0
        // Bye selection (fallback path): same logic as main path.
        // Priority: (1) Lowest score, (2) Weakest player (highest seed number) among ties.
        let byeCandidate =
          if players.Length % 2 = 0 then
            None
          else
            let ordered =
              players
              |> List.sortBy (fun p ->
                  let seed = seedMap.[p.Name]
                  scoreFor p.Name, -seed)  // -seed: higher seed number (weaker) sorts first
            let preferred =
              ordered |> List.tryFind (fun p -> byeSet.Contains p.Name |> not)
            preferred |> Option.orElse (ordered |> List.tryHead)
        let pairingPlayers, byePlayer =
          match byeCandidate with
          | None -> players, None
          | Some bye -> players |> List.filter (fun p -> p.Name <> bye.Name), Some bye
        let orderedPlayers =
          pairingPlayers
          |> List.sortBy (fun p -> (-(scoreFor p.Name), seedMap.[p.Name]))
        let rec findPairs
          (remaining: EngineConfig list)
          (acc: (EngineConfig * EngineConfig * float) list) =
            match remaining with
            | [] -> Some (List.rev acc)
            | (player: EngineConfig) :: rest ->
                let candidates =
                  rest
                  |> List.filter (fun (opp: EngineConfig) -> priorPairs.Contains (swissPairKey player.Name opp.Name) |> not)
                  |> List.sortBy (fun (opp: EngineConfig) ->
                      let scoreDiff = abs (scoreFor player.Name - scoreFor opp.Name)
                      scoreDiff, seedMap.[opp.Name])
                let rec tryCandidate options =
                  match options with
                  | [] -> None
                  | (opp: EngineConfig) :: tail ->
                      let nextRemaining = rest |> List.filter (fun p -> p.Name <> opp.Name)
                      match findPairs nextRemaining ((player, opp, scoreFor player.Name) :: acc) with
                      | Some pairs -> Some pairs
                      | None -> tryCandidate tail
                tryCandidate candidates
        let gamePairs =
          match findPairs orderedPlayers [] with
          | Some pairs ->
              // Sort pairs by score (lowest first) to ensure weakest groups play first
              let seedFor name =
                seedMap |> Map.tryFind name |> Option.defaultValue System.Int32.MaxValue
              pairs
              |> List.sortBy (fun (a, b, score) ->
                  let seedA = seedFor a.Name
                  let seedB = seedFor b.Name
                  let minSeed = if seedA < seedB then seedA else seedB
                  score, -minSeed)
              |> List.map (fun (a, b, _) -> (a, b))
          | None -> failwith "Swiss pairing failed: no valid non-repeat pairings found for this round."
        match byePlayer with
        | Some bye ->
            let byeEngine = { EngineConfig.Empty with Name = "BYE" }
            gamePairs @ [ (bye, byeEngine) ]
        | None -> gamePairs

  let addPlannedPairings
    (planned: ResizeArray<Pairing>)
    (whiteFirst: EngineConfig)
    (blackFirst: EngineConfig)
    (openings: PGNTypes.PgnGame list)
    (gamesPerMatch: int)
    (startIndex: int) =
      if openings.IsEmpty then
        startIndex
      else
        let gamesPerPair = Math.Max(1, gamesPerMatch / 2)
        let mutable index = startIndex
        for _ in 0 .. gamesPerPair - 1 do
          let opening = openings.[index % openings.Length]
          let openingHash = Hash.computeOpeningHashFromGame opening
          planned.Add(
            { Opening = opening
              White = whiteFirst
              Black = blackFirst
              GameNr = 0
              RoundNr = $"{opening.GameNumber}.{planned.Count + 1}"
              OpeningHash = openingHash })
          planned.Add(
            { Opening = opening
              White = blackFirst
              Black = whiteFirst
              GameNr = 0
              RoundNr = $"{opening.GameNumber}.{planned.Count + 1}"
              OpeningHash = openingHash })
          index <- index + 1
        index

  /// Generate cup pairings for N games per match, using a new opening per two-game mini-match.
  // Key generator
  let pairingKey (openingHash: string) (fen: string) (white: string) (black: string) =
        $"{openingHash}|{fen}|{white.Trim()}|{black.Trim()}"

    // Preprocess once
  let playedSet (gamesAlreadyPlayed : PgnGame array) =
        gamesAlreadyPlayed
        |> Array.map (fun e ->
            pairingKey
                (if String.IsNullOrEmpty e.GameMetaData.OpeningHash then e.GameNumber.ToString() else e.GameMetaData.OpeningHash)
                e.GameMetaData.Fen
                e.GameMetaData.White
                e.GameMetaData.Black)
        |> Set.ofArray

    // Fast check
  let hasPlayedBefore (pairing: Pairing) (playedSet: Set<string>) =
        let key = pairingKey pairing.OpeningHash pairing.Opening.GameMetaData.Fen pairing.White.Name pairing.Black.Name
        playedSet.Contains key

  /// Rotates a list by moving the first element to the end.
  /// Used in Berger round robin pairing to rotate the player list each round.
  let rotateListByOne (lst: 'a list) : 'a list =
      match lst with
      | [] -> []
      | head :: tail -> tail @ [head]

  /// Rotates a list by moving the last element to the second position.
  /// For [a; b; c; d] returns [a; d; b; c].
  /// This is the standard Berger rotation for round robin tournaments.
  let rotateOnce (players: 'a list) : 'a list =
      match players with
      | [] | [_] -> players
      | h :: t ->
          match List.rev t with
          | [] -> players
          | last :: revRest ->
              let rest = List.rev revRest
              h :: last :: rest


  let gauntletSingleRoundPerOpening (challengers: EngineConfig list) (opponents: EngineConfig list) (opening: PGNTypes.PgnGame) =

      [ let mutable subIndex = 0
        for o in opponents do
          for p in challengers do
            let openingHash = Hash.computeOpeningHashFromGame opening
            subIndex <- subIndex + 1
            let roundString = $"{opening.GameNumber}.{subIndex}"
            let roundMatches : Pairing list= [{Opening = opening; White=p; Black=o; GameNr = 0; RoundNr= roundString; OpeningHash = openingHash }] // Main player has white pieces
            yield roundMatches
      ] |> List.concat


  let gauntletSingleRound doNotDeviate (challengers: EngineConfig list) (opponents: EngineConfig list) (openings: PGNTypes.PgnGame list) =
    [
      let mutable opponents = opponents
      for opening in openings do
        if doNotDeviate then
          opponents <- rotateListByOne opponents
        let games = gauntletSingleRoundPerOpening challengers opponents opening
        yield games
    ] |> List.concat |> List.mapi(fun i e -> {e with GameNr = i + 1})

  let gauntletDoubleRoundPerOpening (challengers: EngineConfig list) (opponents: EngineConfig list) (opening: PGNTypes.PgnGame) =
      let singleMatches = gauntletSingleRoundPerOpening challengers opponents opening
      let reverseColors (pair:Pairing) (idx:int)  : Pairing =
        {Opening=pair.Opening; White=pair.Black; Black= pair.White; OpeningHash = pair.OpeningHash; GameNr = pair.GameNr; RoundNr = $"{pair.Opening.GameNumber}.{idx}" }
      let fromIdx = singleMatches.Length + 1
      let reverseGames = singleMatches |> List.mapi (fun idx p -> reverseColors p (fromIdx + idx))
      (singleMatches @ reverseGames)

  let gauntletDoubleRound doNotDeviate (challengers: EngineConfig list) (opponents: EngineConfig list) (openings: PGNTypes.PgnGame list) =
    [
      let mutable opponents = opponents
      for opening in openings do
        if doNotDeviate then
          opponents <- rotateListByOne opponents
        let games = gauntletDoubleRoundPerOpening challengers opponents opening
        yield games
    ] |> List.concat |> List.mapi(fun i e -> {e with GameNr = i + 1})

  let getRotatedLists (players: EngineConfig list) =
    let padded =
        if List.length players % 2 = 1 then EngineConfig.Empty :: players else players
    let rounds = List.length padded - 1
    let rec rotate acc n current =
        if n = 0 then List.rev acc
        else
            let next = rotateOnce current
            rotate (next :: acc) (n - 1) next
    rotate [padded] (rounds - 1) padded

  let createRRPairs (players: EngineConfig list) (opening: PgnGame) round =
    let evenRound = round % 2 = 0
    let players =
        if List.length players % 2 = 1 then EngineConfig.Empty :: players else players
    let half = List.length players / 2
    let first, second = players |> List.splitAt half
    let zipped = List.zip first (List.rev second)
    [
        for idx, (w, b) in zipped |> List.indexed do
            if w.Name <> EngineConfig.Empty.Name && b.Name <> EngineConfig.Empty.Name then
                let (white, black) =
                    if idx = 0 then
                        if evenRound then (b, w) else (w, b)
                    else if idx % 2 = 1 then
                        (w, b)
                    else
                        (b, w)
                let openingHash = Hash.computeOpeningHashFromGame opening
                let roundString = $"{opening.GameNumber}.{1}"
                yield {
                    Opening = opening
                    White = white
                    Black = black
                    OpeningHash = openingHash
                    GameNr = 0
                    RoundNr = roundString
                }
    ]

  let getPairingsPerOpening (players: EngineConfig list) opening =
    let lists = getRotatedLists players
    let mutable round = 1
    [for rotatedList in lists do
      let pairings = createRRPairs rotatedList opening round
      round <- round + 1
      yield! pairings ]

  let generateAllRoundRobinSingleRounds (players: EngineConfig list) (openings: PGNTypes.PgnGame list) =
    [
      for opening in openings do
        let games = getPairingsPerOpening players opening
        yield games
    ] |> List.concat |> List.mapi(fun i e -> {e with GameNr = i + 1})

  let generateAllRoundRobinDoubleRounds (players: EngineConfig list) (openings: PGNTypes.PgnGame list) =
    let reverseColors (pair:Pairing) (idx:int) : Pairing =
        { Opening = pair.Opening; White = pair.Black; Black = pair.White; OpeningHash = pair.OpeningHash; GameNr = 0;  RoundNr = $"{pair.Opening.GameNumber}.{idx}" }
    [
      for opening in openings do
        let games =
            getPairingsPerOpening players opening
            |> List.mapi (fun idx p -> {p with RoundNr = $"{opening.GameNumber}.{idx+1}"})

        yield! games
        let fromIdx = games.Length + 1
        let reverseGames = games |> List.mapi (fun idx p -> reverseColors p (fromIdx + idx))
        yield! reverseGames
    ]|> List.mapi(fun i e -> {e with GameNr = i + 1})

  let printAllOpeningPairs (logger: ILogger) (pairings: Pairing list) =
    let sb = StringBuilder()
    sb.AppendLine() |> ignore
    pairings
    |> List.iteri (fun idx p ->
        let openingName = PGNHelper.getOpeningInfo p.Opening
        let opName =
            if openingName.Contains "No opening name" && not (String.IsNullOrEmpty p.Opening.Fen) then
                p.Opening.Fen
            else
                openingName
        let msg = $"Round: {p.RoundNr}  ({idx + 1}): {p.GameNr}. {opName}, {p.White.Name} vs {p.Black.Name}"
        sb.AppendLine(msg) |> ignore
    )
    logger.LogInformation(sb.ToString())

  let getAllOpeningPairs (pairings: Pairing list) =
    let sb = StringBuilder()
    sb.AppendLine() |> ignore
    pairings
    |> List.iteri (fun idx p ->
        let openingName = PGNHelper.getOpeningInfo p.Opening
        let opName =
            if openingName.Contains "No opening name" then
                p.Opening.Fen
            else
                openingName
        let msg = $"Round: {p.RoundNr} ({p.GameNr}): {idx + 1}. {opName}, {p.White.Name} vs {p.Black.Name}"
        sb.AppendLine(msg) |> ignore
    )
    sb.ToString()
