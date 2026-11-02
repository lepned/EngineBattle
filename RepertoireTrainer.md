# Repertoire Trainer

Learn an opening repertoire by playing it. The board plays the opponent's moves, the ones people play most often coming up most often, and you find the answers. A move you miss comes back sooner than one you know.

## Page Location

WebGUI: **Play & Analysis → Repertoire Trainer** (`/play/repertoire`).

## The repertoire

The trainer reads a PGN made with [`eb-cli book repertoire`](eb-cli.md#book-repertoire): one game with variations, the moves of one side and the opponent's replies. **Browse** picks the file; the page opens it again next time.

- **Your side** comes from the game's Event (`Repertoire for Black`); the board is turned to it.
- **How often a move is played** comes from its comment: the human net's share (`Maia-1900 34%.`, made with `--human`) or the book's (`Book 40%.`). A move without one counts as 5%.
- **The opponent's mistakes** are the replies whose `[%eval]` gives winning chances away against the best reply, by the same rule the repertoire uses (`?` and `??`).
- **Moves without a comment** are not trained: the opening's first moves (the `--moves` the repertoire was made from) are played for you, and where a line goes on with the engine's continuation it is played out and the line ends.

## Training

1. The board plays the opening and the opponent's first move. Your move.
2. Play the repertoire's move:
   - **Right:** the opponent's next move follows after a moment, and the comment of the move you answered is shown (how often it is played, what is better).
   - **Also good:** a move the repertoire keeps beside its own ("Also good.", "Objective best.") is accepted; the repertoire's move is played instead, so the line goes on as written.
   - **Wrong:** your move is taken back and the repertoire's move is shown with a green arrow and its eval. Play it to go on.
3. **Show** plays the repertoire's move for you (it counts as missed). **Next line** starts the next line at any time.

### Modes

| Mode | Lines |
|---|---|
| **All lines** | Every line. The opponent's moves are picked by how often they are played; a line with a position that is due weighs four times as much, one you have seen to the end and that is not due a quarter. |
| **Traps** | Only lines where the opponent makes a mistake to punish. Once the mistake is on the board the line goes on as it comes. |
| **Due** | Only lines with a position that is due again. |

## The page

A bar on top holds the repertoire (**Browse**), the modes and **Train** | **Build**. The board has the answer (or the build) under it, and beside it the note on the opponent's move, the counts, the **Engine** drawer and the move list with each move's eval. The eval of the opponent's move you are to answer shows once you have answered: before, it would tell a mistake.

## Engine

The **Engine** drawer, closed at first, holds an engine to look at the position on the board: the broadcast page's compact kibitzer, with the default engine from **Settings** (any engine definition can be picked in it), a time per position and 1, 3 or 5 lines. Open it and press ▶: it then analyses every position you go to. It looks only when the board is free - in Build and after a line - and is paused during a line, where it would give the answer away, and while a build runs (the two would share the engine's GPU or cores).

The drawer and Build use **one engine**: picking it in either changes both. Both pickers list the engine folder's definitions with a filter (type a part of the name; arrows and Enter pick); **Other…** in Build takes a definition from another folder. Closed, the drawer shows the engine's eval in its title.

## Build

The **Build** tab makes a repertoire on the page, the same as [`eb-cli book repertoire`](eb-cli.md#book-repertoire), from the position on the board.

1. Play the opening on the board (in Build the board is free: arrows and the move list navigate). The repertoire starts from the position at the cursor.
2. Pick what it is made with:
   - **For** - the side the repertoire is for; at first the side that made the last move (after 3...c5, Black).
   - **Engine** - the engine that judges every move, the same as the Engine drawer's; at first the default engine from **Settings**.
   - **People** - a net that plays like people for the opponent's moves (the engine definitions in the engine folder with *maia* in their name), or none.
   - **Moves ahead** - how far from the position, in moves of each side (5 moves = 10 half-moves, eb-cli's `--plies 10`); where the opponent has one good move and where an exchange is under way, a line runs longer.
   - **Search** - how the engine searches a position: **Nodes** (the default: the same result every run), **Time (ms)** (depends on the machine's speed and load) or **Depth**.
   - **File** - the PGN, named after the side and the moves, next to the open repertoire (or in the PGN output folder).
   - **More options** - replies explored, their window, traps and risk, critical lines only, and a Polyglot book whose moves are answered too.
3. **Build** runs in the background: the build goes on when you leave the page, and the page shows it when you come back. It shows the positions searched and the time.
4. **Stop** ends it. The searches are kept next to the PGN (`.judge.cache`, `.human.cache`), so **Build** again with the same settings goes on where it stopped; eb-cli with the same engine and search (`--nodes`, `--movetime`, `--depth`) uses the same caches.
5. **Train it** opens the finished repertoire in **Train**.

One build runs at a time. A repertoire of 10 plies takes minutes to hours, depending on the search and the opening.

## Progress

Every position where you move has a box from 1 to 5 (Leitner). Found at the first try, it moves up a box; missed, it goes back to box 1. A position comes back after this many lines:

| Box | 1 | 2 | 3 | 4 | 5 |
|---|---|---|---|---|---|
| Lines until due | 0 | 2 | 5 | 12 | 30 |

A position found after a miss in the same line stays in box 1. The progress is kept next to the PGN as `<file>.pgn.train.json`, by position: a repertoire made again keeps it for the positions it still has.

The table under the buttons shows right and wrong answers this session, the lines trained, the positions of yours in the repertoire, how many are due and how many you have not seen yet.
