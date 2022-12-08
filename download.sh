#!/bin/env bash
echo Downloading Day$((2200+$1)).txt
curl -H "Cookie: session=$TOKEN" https://adventofcode.com/2022/day/$1/input > advent21/src/main/resources/org/primetalk/advent2022/Day$((2200+$1)).txt
