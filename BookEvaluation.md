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
| **Min eval (cp)** | 80 | Minimum absolute eval in centipawns. Every engine's absolute eval must reach it, or the position is filtered out (too drawish). |
| **Max eval (cp)** | 100 | Maximum absolute eval in centipawns. Positions with any engine eval above this are filtered out (too one-sided). |
| **Max eval diff (cp)** | 40 | Maximum allowed eval difference between engines, signs included. Positions where engines disagree by more than this - or on which side is better - are filtered out. |
| **Output folder** | The Openings folder from Global Settings | Folder where results are saved (inside a `BookEvals` subfolder) |

### Eval Filtering Logic

A position passes if:
- Every engine's absolute eval is ≥ **Min eval**
- No engine's absolute eval exceeds **Max eval**
- The difference between the highest and lowest engine eval, with their signs, is < **Max eval diff** (with a single engine there is no difference to check). The evals are from the side to move, the same for every engine, so +90 from one engine and -90 from another is a difference of 180: the engines disagree on who is better, and the position is filtered out.

This ensures the position is competitive (not dead drawn) but not busted (not clearly winning for one side), and that engines roughly agree on the assessment.

## Running

1. Click **Pick opening book** to select a `.pgn` or `.epd` file
2. Click **Evaluate and sort**
3. A progress bar shows positions evaluated
4. Click **Stop** to cancel (partial results are saved)

PGN openings that end in the same position (transpositions) are evaluated once; the summary says how many were dropped. If an engine fails - it does not start, crashes, or gives no score (a search too short to report one) - the run stops, the page names the engine and the reason, and what passed before is saved.

## Output

Results are saved to `<output folder>/BookEvals/`:

- **PGN input** → `BookEval_<book>_<engines>.pgn` — filtered PGN games with a `[MaxEval]` header added showing the highest eval, which engine produced it, and the best move
- **EPD input** → `BookEval_<book>_<engines>.epd` — filtered EPD lines with eval summaries

The results summary shows:
- Positions analyzed and passed (with pass rate)
- Engines used and their search limits
- Eval range and max diff settings
- Duration and output file path

## From the Command Line

The same run is the `bookeval` verb (alias `be`), for books too large to babysit in the browser:

```bash
eb-cli bookeval book.pgn --engine Stockfish19.json --movetime 300 --engine Lc0.json --nodes 2000
eb-cli bookeval book.epd --engine Stockfish19.json --min 30 --max 120 --count 1000 --out balanced.epd
```

| Option | Default | Description |
|--------|---------|-------------|
| `--engine <def.json\|exe>` | (required) | An engine; repeat for more |
| `--nodes N` / `--movetime MS` | 10000 nodes | Right after an `--engine`: that engine's limit. Before the first `--engine`: every engine's default |
| `--min`, `--max`, `--maxdiff` | 80, 100, 40 | The filter above, in centipawns |
| `--count N` | the whole book | Openings to read from the book |
| `--out F` | `BookEvals/BookEval_<book>_<engines>.<ext>` beside the book | Output file |

When the run ends, the verb prints the same Results table as the page. Ctrl+C stops the run and writes what passed (a second Ctrl+C ends it at once). The exit code is 1 when an engine stopped the run.

## Tips

- Use **two engines** (e.g. Stockfish + Lc0) for more reliable filtering — the max eval diff catches positions where one engine is wrong
- Start with a larger eval range and tighten it based on results
- For tournament opening books, a min eval of 30–80 cp and max eval of 80–120 cp is a reasonable starting point
- Use the **Number of openings** parameter to limit evaluation time when working with large books
