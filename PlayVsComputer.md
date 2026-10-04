# Play vs Computer

A game against any UCI engine, with clocks, take back, premoves, saved games and a direct way into Game Review.

## Page Location

WebGUI: **Play & Analysis → Play vs Computer** (`/play/computer`). The same game, with Lc0's contempt settings beside it, is the **Train** mode of [Lc0 Contempt](Lc0Contempt.md).

## Setup

- **Engine:** the default engine from **Global Settings**, or **Load Engine** to pick an engine definition. A file that cannot be read leaves the current engine in place. The engine starts at the first game (or when loaded) and is reused for every game after it.
- **Time Control:** the presets (1+0 to 30+0) or **Custom**:
  - **Time** - minutes and increment, the same for both sides or separate for White and Black.
  - **Nodes** - the engine searches a fixed number of nodes per move; there is no clock.
  - **Infinite** - no clock; the engine is told it has 300 minutes.
- **Play As:** White or Black. Between games, rotating the board also picks the side.

Time control and side are locked while a game is running.

## Playing

**Start Game** (or **Restart**) begins a new game from the start position; with Black, the engine moves first. The clock starts once the engine is ready, not while it loads.

- **Moves:** drag or click a piece. Legal destinations show as dots when **Show Legal Moves** is on (**Appearance** page).
- **Premoves:** with **Premoves** on (**Appearance** page), a move made while the engine thinks is queued and played the moment your turn comes, if it is still legal. A click on the board cancels it.
- **Clocks:** run in real time. Each side gets its increment after its move. The engine is told about 200 ms less than its own remaining time, so the time between its bestmove and the move appearing on the board does not make it lose on time.
- **During a game** the board accepts moves only on your turn, and the move list, the arrow buttons and the arrow keys do not navigate (a step back would look like a move). After the game they work as usual.

### Buttons

| Button | What it does |
|--------|--------------|
| **Force Move** | The engine stops thinking and plays its best move so far |
| **Take Back** | Back to your last turn: one move while the engine thinks, your move and the engine's reply on your turn. The clocks go back to what they showed in that position |
| **Abort** | Ends the game without a result |
| **Resign** | Ends the game as a loss |

## How a Game Ends

- Checkmate, stalemate, insufficient material, threefold repetition or the 50-move rule.
- A flag: the side whose clock runs out loses.
- Resign or Abort.
- **Aborted - Engine:** the engine exited or stopped answering, played a move that is not legal, gave no move, or a different engine was loaded during the game. The status line says which. The next game starts the engine again.

Every finished game is added to the **Results** table.

## Saving and Reviewing

- **Save PGN** appends the game to the file named next to it (default: **Settings → Analysis Games**). The Results table lists the games already in that file.
- **Review Game** opens the game in [Game Review](GameReview.md).
