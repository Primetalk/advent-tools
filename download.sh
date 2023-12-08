#!/bin/env bash
echo Downloading Day$((2300+$1)).txt
curl -H "Cookie: session=$TOKEN" https://adventofcode.com/2023/day/$1/input > advent21/src/main/resources/org/primetalk/advent2023/Day$((2300+$1)).txt
