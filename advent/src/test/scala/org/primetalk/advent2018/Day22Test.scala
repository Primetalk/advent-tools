package org.primetalk.advent2018

import org.primetalk.advent2018.Day22.{inputDepth, Eval, inputModulo, inputTarget}

class Day22Test extends BaseTest {

  behavior of "Day22Test"

  it should "eval" in {
    Eval(510, (10, 10), inputModulo).apply shouldBe 114
  }

//  it should "timeToTarget" in {
//    Eval(510, (10, 10), inputModulo).timeToTarget shouldBe 45
//  }


  /*
M=.|=.|.|=.|=|=. .=.|=.|.|=.|=|=
.|=|=|||..|.=... ..===|..|..==.=
.==|....||=..|== ..|.=.=.=..=..|
=.|....|.==.|==. ==|=..=.====|..
=|..==...=.|==.. =.===.|.|==|=|.
=||.=.=||=|=..|= ==.|=.|===.==.|
|.=.===|||..=..| |..=|.|||.=..=|
|..==||=.|==|=== |=.||||||.||...
.=..===..=|.|||. ..=.|||.|.===.|
.======|||=|=.|= .|=.|||..||===.
.===|=|===T===|| .|====||.|.....
=|||...|==..|=.| =.|===|.=......
=.=|=.=..=.||==| =.=|=..=|=.....
||=|=...|==.=|== ||||=|.........
|=.=||===.|||=== |==|.||..=.....
|| .|==.|.|.||=||

.=.|=.|.|=.|=|=
..===|..|..==.=
..|.=.=.=..=..|
==|=..=.====|..
=.===.|.|==|=|.
==.|=.|===.==.|
|..=|.|||.=..=|
|=.||||||.||...
..=.|||.|.===.|
.|=.|||..||===.
.|====||.|.....
=.|===|.=......
=.=|=..=|=.....
||||=|.........
|==|.||..=.....
   */
}