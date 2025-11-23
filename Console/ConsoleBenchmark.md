# Console Benchmarking

Important: run these commands from the `Console` folder. Open a terminal in the `Console` directory (or change into it from the repo root) before running the example below.

Use the console benchmark verb to run a set of UCI option combinations against an engine configuration and a batch of positions:

```
dotnet run -c release benchmark benchmark-options.json
```

The JSON file describes:

- `engineConfigPath`: path to the `EngineDef.json` describing the engine binary and UCI tuning.
- `durationSeconds`: how long each engine+position search should run (seconds).
- `optionSets`: a list of uci option dictionaries. Every unique combination is evaluated in sequence.
- `positions`: named FEN/EPD strings that the benchmark will search through.

For each combination, the runner gathers EPS/NPS from `Utilities.Regex.getEssentialDataWithEPS`, prints per-position stats, and writes a summary log to `logs/benchmark-summary-<timestamp>.txt` unless `summaryOutputPath` is configured. The console output highlights the best combination (EPS first, NPS second).

Use the log file to compare EPS/NPS trade-offs between combinations or to archive benchmark runs for future reference.