# advent-2022 🎄

![AoC Stars](https://img.shields.io/badge/48-%F0%9F%8C%9F-yellow)

Learning Clojure via 2022 Advent of Code.

## Usage

My inputs are not stored in this repo. `run` targets expect them at paths like `inputs/25_input.txt`. See Makefile.
```
day=01 make run-example
day=14 make run

// directly
clj -M -m advent-2022.day-19 <path to input file>
```
