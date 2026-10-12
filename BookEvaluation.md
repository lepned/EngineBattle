# Out-of-Book Evaluation

This tool evaluates opening positions right after book exits using one or more engines. Use it to filter out openings that are too drawish or too one-sided for engine matches, helping you curate balanced opening books.

## How It Works

1. Load an opening book (PGN or EPD file)
2. Each position is evaluated by the configured engines
3. Positions are filtered by eval range and eval agreement between engines
4. Passing positions are saved to a new file, the most disputed first (largest eval difference between the engines; with one engine, the largest absolute eval)

## Page Location

WebGUI: **Tools → Book Evaluation** (`/tools/book-evaluation`)

## Setup

### Engines

Add one or more engines using the **+ Add Engine** button, which opens a file browser for engine definition JSON files. The page also auto-loads engines from your Global Settings (Default Engine and Secondary Engine) on startup.

For each engine you can configure:
- **Mode**: `Nodes` or `Time` (milliseconds)
- **Value**: The search limit (e.g. 10000 nodes or 3000 ms)

### Parameters

| Parameter | Default | Description |
|-----------|---------|-------------|
| **Number of openings** | 500 | Maximum number of positions to evaluate from the source file |
| **Min eval (pawns)** | 0.8 | Minimum absolute eval in pawns. Every engine's absolute eval must reach it, or the position is filtered out (too drawish). |
| **Max eval (pawns)** | 1.0 | Maximum absolute eval in pawns. Positions with any engine eval above this are filtered out (too one-sided). |
| **Max eval diff (pawns)** | 0.4 | Maximum allowed eval difference between engines, signs included. Positions where engines disagree by more than this - or on which side is better - are filtered out. |
| **Output folder** | The Openings folder from Global Settings | Folder where results are saved (inside a `BookEvals` subfolder) |

### Eval Filtering Logic

A position passes if:
- Every engine's absolute eval is ≥ **Min eval**
- No engine's absolute eval exceeds **Max eval**
- The difference between the highest and lowest engine eval, with their signs, is < **Max eval diff** (with a single engine there is no difference to check). The evals are from White's view, as everywhere in EngineBattle, so +0.9 from one engine and -0.9 from another is a difference of 1.8: the engines disagree on who is better, and the position is filtered out.

This ensures the position is competitive (not dead drawn) but not busted (not clearly winning for one side), and that engines roughly agree on the assessment.

## Running

1. Click **Pick opening book** to select a `.pgn` or `.epd` file
2. Click **Evaluate and sort**
3. A progress bar shows positions evaluated
4. Click **Stop** to cancel (partial results are saved)

PGN openings that end in the same position (transpositions) are evaluated once; the summary says how many were dropped. If an engine fails - it does not start, crashes, or gives no score (a search too short to report one) - the run stops, the page names the engine and the reason, and what passed before is saved.

## Output

Results are saved to `<output folder>/BookEvals/`:

- **PGN input** → `BookEval_<book>_<engines>.pgn` — filtered PGN games with a `[MaxEval]` header added showing the largest eval (in pawns or a mate such as M1 / -M2, White's view: negative when Black is better), which engine produced it, and the best move
- **EPD input** → `BookEval_<book>_<engines>.epd` — filtered EPD lines with each engine's eval (in pawns or a mate such as M1 / -M2, White's view) and best move

The results summary shows:
- Positions analyzed and passed (with pass rate)
- Engines used and their search limits
- Eval range and max diff settings
- Duration and output file path

## From the Command Line

The same run is `eb-cli book eval`, for books too large to babysit in the browser:

```bash
eb-cli book eval book.pgn --engine Stockfish19.json,movetime=300 --engine Lc0.json,nodes=2000
eb-cli book eval book.epd --engine Stockfish19.json --min-eval 0.3 --max-eval 1.2 --count 1000 -o balanced.epd
```

| Option | Default | Description |
|--------|---------|-------------|
| `--engine ENGINE[,nodes=N\|,movetime=MS]` | (required) | An engine (a def, an exe or a def name), with its own limit after a comma; repeat for more |
| `--nodes N` / `--movetime MS` | 10000 nodes | The limit of every engine without its own |
| `--min-eval`, `--max-eval`, `--max-diff` | 0.8, 1.0, 0.4 | The filter above, in pawns |
| `--count N` | the whole book | Openings to read from the book |
| `-o, --out FILE` | `BookEvals/BookEval_<book>_<engines>.<ext>` beside the book | Output file; an existing one needs `--force` |

When the run ends, the command prints the same Results table as the page. Ctrl+C stops the run and writes what passed (a second Ctrl+C ends it at once). The exit code is 1 when an engine stopped the run.

## Tips

- Use **two engines** (e.g. Stockfish + Lc0) for more reliable filtering — the max eval diff catches positions where one engine is wrong
- Start with a larger eval range and tighten it based on results
- For tournament opening books, a min eval of 0.3–0.8 and max eval of 0.8–1.2 pawns is a reasonable starting point
- Use the **Number of openings** parameter to limit evaluation time when working with large books
