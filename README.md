# Advent-tools

Tools for solving [Advent of Code puzzles](https://adventofcode.com/) (and solutions to some of them).

## Getting started

1. Clone project

```bash
git clone git@github.com:/Primetalk/advent-tools.git
cd advent-tools
```

2. Build

```bash
mill advent.compile
```

3. Generate IntelliJ Idea project

```bash
mill mill.scalalib.GenIdea/idea
```

4. REPL

```bash
mill -i advent.repl
```

5. Loading daily input

```bash
curl https://adventofcode.com/2019/day/2/input -o advent/src/main/resources/org/primetalk/advent2019/day2.txt
```

Unfortunately, this doesn't work. It requires authentication.
