# Console Benchmarking

Important: run these commands from the `Console` folder. Open a terminal in the `Console` directory (or change into it from the repo root) before running the example below.

Use the console benchmark verb to run a set of UCI option combinations against an engine configuration and a batch of positions:

```
dotnet run -c release benchmark benchmark-options.json
```

The JSON file describes:

- `engineConfigPath`: path to the `EngineDef.json` describing the engine binary and UCI tuning.
- `durationSeconds`: how long each engine+position search should run (seconds). Default 20 when omitted or 0.
- `optionSets`: a list of `{ "optionKey": "<UCI option>", "values": [ ... ] }` entries. The values of all entries are combined as a Cartesian product (two options with 3 and 2 values give 6 combinations), and every combination is evaluated in sequence. With no option sets the engine's own configuration runs once as the baseline.
- `positions`: a list of `{ "name": "...", "fen": "..." }` entries that the benchmark searches in turn. An empty `fen` means the start position, and with no positions at all the start position alone is used.
- `summaryOutputPath` (optional): where to write the summary log.

Example:

```json
{
  "engineConfigPath": "C:/Dev/Chess/Engines/EngineDefs/Lc0.json",
  "durationSeconds": 20,
  "optionSets": [
    { "optionKey": "MinibatchSize", "values": [128, 256, 512] },
    { "optionKey": "Threads", "values": [2, 4] }
  ],
  "positions": [
    { "name": "Start", "fen": "" },
    { "name": "Middlegame", "fen": "r1bq1rk1/pp2bppp/2n1pn2/3p4/2PP4/2N1PN2/PP3PPP/R1BQKB1R w KQ - 0 7" }
  ]
}
```

To keep the numbers comparable, the benchmark removes `SyzygyPath` from the engine configuration (no tablebase probes) and sets `SmartPruningFactor` and `MoveOverhead` to 0 when the engine has those options; option sets cannot override the last two.

For each combination, the runner gathers EPS/NPS from `EngineProtocol.Regex.getEssentialDataWithEPS`, prints per-position stats, and writes a summary log to `logs/benchmark-summary-<timestamp>.txt` unless `summaryOutputPath` is configured. The console output highlights the best combination (EPS first, NPS second).

Use the log file to compare EPS/NPS trade-offs between combinations or to archive benchmark runs for future reference.