# Ratings

Every standings table in EngineBattle - the console's, the GUI's, the Ratings page's and
`eb-cli pgn elo` - rates all the players of a set of games at once, with the model the Ordo
program uses, computed by EngineBattle itself. Nothing has to be installed or pointed to.

## What the columns mean

- **Elo** - the player's rating, relative to the reference: with two players the first one is
  the reference at 0; with more, the pool average is 0. On the Ratings page and with
  `pgn elo --anchor NAME` any player can be the reference. Ratings are only comparable within
  one table.
- **Error** - the 95 % error bar of the rating, relative to the reference (so the reference has
  none, shown as `----`). With two players the other player's error is the paired-openings
  (pentanomial) one, the same the match's Elo line gives, from the first finished pair on.
- **CFS%** - the confidence that this player is stronger than the player on the next row; the
  last row has none (`---`). Every table is in the same order, rating and then points per game,
  so the row below is the one meant. 50 means the two cannot be told apart, 95 means the order is
  as sure as the error bars allow.
- **White advantage** (under the table) - what playing White was worth in these games, with its
  error. Information only: the ratings count a score as an Elo whatever the colours, the scale of
  the pentanomial line, SPRT, fastchess, cutechess and Ordo without `-W`, so a gauntlet's table
  compares with each pair's pentanomial numbers and a match's table equals its Elo line. An
  unbalanced book (UHO) shows a large advantage; an engine that had White more often than Black
  gets some of it counted as strength (Ordo's compromise too).
- **Draw rate (equal opponents)** - the draw model's rate for two players of equal strength and
  no colour advantage. It is the rate the model needs to reproduce the number of draws played,
  which with a large white advantage is a steep extrapolation (an event with half its games
  drawn can show 90 %); Ordo prints the same number. It shapes the error bars, not the ratings.

## The model

- **Expected score** of a player `d` Elo stronger than the opponent: `E = 1 / (1 + 10^(-d/400))`,
  standard Elo, with no colour term (see White advantage above).
- **Ratings** are the maximum-likelihood ones with a draw counted as half a win: the solution
  where every player's expected score over its games equals the score it made. That is the
  solution Ordo computes.
- **Draws** follow Ordo's draw model: the draw probability at expected score `E` is the positive
  root of `a D² + 2 D - 4 E (1 - E) = 0` with `a = (1 - 2 d₀) / d₀²`, `d₀` the draw rate for equal
  opponents; win and loss are `E - D/2` and `1 - E - D/2`.
- **Errors** come from the information matrix of the fit under the draw model (the sandwich
  `H⁻¹ J H⁻¹`), the quantity Ordo estimates by replaying the games a thousand times; computed
  exactly instead. The CFS between two players follows from the covariance of their ratings.
- **Two weak priors** keep the numbers finite where the games alone cannot: a Gaussian prior on
  the reported white advantage (standard deviation 300 Elo, so six games of nothing but White
  wins give a number, and an unbalanced book's 200-270 Elo are read within a few percent after a
  few dozen games) and one on every rating (2 000 Elo; 0.1 Elo on a rated player). Without the
  latter a field whose games split it into two camps - a four-engine round robin after its first
  round - has no finite ratings at all.
- **A perfect score** (every game won, or lost) has no rating of its own: the player gets one
  virtual drawn game against the opponent it met most (half a game with each colour, half a
  point in all), its real games stay as played, and a line under the table says the rating is a
  floor (at least) or a ceiling (at most). Ordo converts one of the real games to a draw
  instead, which changes the opponent's result as well.
- **Players the games do not connect** (two groups that never met) are rated as separate pools,
  each with its own average at 0; a line under the table says so, and the CFS between players of
  different pools is not given.

## Checked against Ordo

The fit is tested against Ordo's output for real PGNs (a 1 104-game match, an 18-engine ladder
with and without an anchor, a TCEC superfinal, a CCC event, two of them with Ordo's `-W`):
ratings agree within 0.2 Elo, errors within 10 % of Ordo's simulated ones, the draw rate to
0.05. Replayed game by game over a gauntlet and a round robin, the tables agree with Ordo's
within 0.3 Elo from the twentieth game on; before that Ordo's numbers are not finite, and
EngineBattle's are the priors' (large error bars say so).
