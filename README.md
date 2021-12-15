# Advent of Code 2012 Solutions in Erlang

This repo contains the code I used to solve the puzzles in the [Advent
of Code 2012](https://adventofcode.com/2021/day/12).

To build and run them, you'll need [Erlang](https://www.erlang.org)
installed. I'm using [Erlang 24](https://www.erlang.org/downloads/24),
but much of this code may work with earlier
releases. [Rebar3](http://www.rebar3.org) will make it easier to build
and run everything.

To build:

```
git checkout https://github.com/beerriot/advent-of-code-2021.git
cd advent-of-code-2021
rebar3 compile
```

To run:

```
erl -pa `rebar3 path`
puzzle12:solveA(). %% example: solve the first part of the Day 12 puzzle
```

In general, the code is structured such that `puzzleNN:solveA()` will
produce the answer to the first part of Day `NN`'s puzzle, and
`puzzleNN:solveB()` will produce the answer to the second part.

Puzzle inputs are in the `puzzles/` directory. I suppose those files
are technically copyright Advent of Code owners, but I hope they won't
mind me reposting them here, to make it easier to run these examples.

Explanations of these solutions are slowly showing up on [my blog](https://blog.beerriot.com/category/adventofcode/):

 * [Day 1](https://blog.beerriot.com/2021/12/12/advent-of-code-day-1/)
 * [Day 2](https://blog.beerriot.com/2021/12/13/advent-of-code-day-2/)
 * [Day 3](https://blog.beerriot.com/2021/12/14/advent-of-code-day-3/)
 * [Day 12](https://blog.beerriot.com/2021/12/12/advent-of-code-day-12/)
 * [Day 13](https://blog.beerriot.com/2021/12/13/advent-of-code-day-13/)
 * [Day 14](https://blog.beerriot.com/2021/12/14/advent-of-code-day-14/)