# advent-2022 🎄

![AoC Stars](https://img.shields.io/badge/50-%F0%9F%8C%9F-yellow)

Learning Clojure via 2022 Advent of Code.

UPDATE 1/3/23: Completed!!

## Usage

My inputs are "gitignored" from this repo. To run, they are expected at paths like `inputs/25_input.txt`. See Makefile for context.
```
day=01 make run-example
day=14 make run

// directly
clj -M -m advent-2022.day-19 <path to input file>

// development
make test
make lint
make wc
```
