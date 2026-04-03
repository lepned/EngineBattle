# Out-of-Book Evaluation

This tool evaluates opening positions right after book exits using one or more engines. Use it to filter out openings that are too drawish or too one-sided for engine matches, helping you curate balanced opening books.

## How It Works

1. Load an opening book (PGN or EPD file)
2. Each position is evaluated by the configured engines
3. Positions are filtered by eval range and eval agreement between engines
4. Passing positions are saved to a new file, sorted by eval balance

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
| **Min eval (cp)** | 80 | Minimum absolute eval in centipawns. Positions with all engine evals below this are filtered out (too drawish). |
| **Max eval (cp)** | 100 | Maximum absolute eval in centipawns. Positions with any engine eval above this are filtered out (too one-sided). |
| **Max eval diff (cp)** | 40 | Maximum allowed eval difference between engines. Positions where engines disagree by more than this are filtered out. |
| **Output folder** | From Global Settings | Folder where results are saved (inside a `BookEvals` subfolder) |

### Eval Filtering Logic

A position passes if:
- At least one engine's absolute eval is ≥ **Min eval**
- No engine's absolute eval exceeds **Max eval**
- The difference between the highest and lowest engine eval is ≤ **Max eval diff**

This ensures the position is competitive (not dead drawn) but not busted (not clearly winning for one side), and that engines roughly agree on the assessment.

## Running

1. Click **Pick opening book** to select a `.pgn` or `.epd` file
2. Click **Evaluate and sort**
3. A progress bar shows positions evaluated
4. Click **Stop** to cancel (partial results are saved)

## Output

Results are saved to `<output folder>/BookEvals/`:

- **PGN input** → `BookEval_<book>_<engines>.pgn` — filtered PGN games with a `[MaxEval]` header added showing the highest eval, which engine produced it, and the best move
- **EPD input** → `BookEval_<book>_<engines>.epd` — filtered EPD lines with eval summaries

The results summary shows:
- Positions analyzed and passed (with pass rate)
- Engines used and their search limits
- Eval range and max diff settings
- Duration and output file path

## Tips

- Use **two engines** (e.g. Stockfish + Lc0) for more reliable filtering — the max eval diff catches positions where one engine is wrong
- Start with a larger eval range and tighten it based on results
- For tournament opening books, a min eval of 30–80 cp and max eval of 80–120 cp is a reasonable starting point
- Use the **Number of openings** parameter to limit evaluation time when working with large books
