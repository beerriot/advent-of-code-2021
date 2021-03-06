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
 * [Day 4](https://blog.beerriot.com/2021/12/15/advent-of-code-day-4/)
 * [Day 5](https://blog.beerriot.com/2021/12/16/advent-of-code-day-5/)
 * [Day 6](https://blog.beerriot.com/2021/12/16/advent-of-code-day-6/)
 * [Day 7](https://blog.beerriot.com/2021/12/17/advent-of-code-day-7/)
 * [Day 8](https://blog.beerriot.com/2021/12/18/advent-of-code-day-8/)
 * [Day 9](https://blog.beerriot.com/2021/12/20/advent-of-code-day-9/)
 * [Day 10](https://blog.beerriot.com/2021/12/23/advent-of-code-day-11/)
 * [Day 10](https://blog.beerriot.com/2021/12/20/advent-of-code-day-10/)
 * [Day 12](https://blog.beerriot.com/2021/12/12/advent-of-code-day-12/)
 * [Day 13](https://blog.beerriot.com/2021/12/13/advent-of-code-day-13/)
 * [Day 14](https://blog.beerriot.com/2021/12/14/advent-of-code-day-14/)
 * [Day 15](https://blog.beerriot.com/2021/12/15/advent-of-code-day-15/)
 * [Day 16](https://blog.beerriot.com/2021/12/16/advent-of-code-day-16/)
 * [Day 17](https://blog.beerriot.com/2021/12/17/advent-of-code-day-17/)
 * [Day 18](https://blog.beerriot.com/2021/12/18/advent-of-code-day-18/)
 * [Day 19](https://blog.beerriot.com/2021/12/19/advent-of-code-day-19/)
 * [Day 20](https://blog.beerriot.com/2021/12/20/advent-of-code-day-20/)
 * [Day 21](https://blog.beerriot.com/2021/12/21/advent-of-code-day-21/)
 * [Day 22](https://blog.beerriot.com/2021/12/22/advent-of-code-day-22/)
 * [Day 23](https://blog.beerriot.com/2021/12/25/advent-of-code-day-23/)
 * [Day 24](https://blog.beerriot.com/2021/12/28/advent-of-code-day-24/)
