package org.primetalk.advent
import fastparse._, NoWhitespace._
import Day20ParseRegex._

class Day20ParseRegexTest extends BaseTest {

  behavior of "ParseRegexTest"

  it should "expr" in {
    import Day20ParseRegex._
    val Parsed.Success(res, _) = parse("^WN$", completeExpression(_))
    res shouldBe PlainPath("WN")
  }

  it should "parseRegex" in {
    val res = parseRegex("^WN$")
    res shouldBe PlainPath("WN")
  }

  it should "parseRegex from example 1" in {
    val res = parseRegex("^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$")
    val expected =
      Sequence(Seq(
        PlainPath("WSSEESWWWNW"),
        Branch(Seq(
          PlainPath("S"),
          Sequence(Seq(
            PlainPath("NENNEEEENN"),
            Branch(Seq(
              Sequence(Seq(
                PlainPath("ESSSSW"),
                Branch(Seq(
                  PlainPath("NWSW"),
                  PlainPath("SSEN")
                ))
              )),
              Sequence(Seq(
                PlainPath("WSWWN"),
                Branch(Seq(
                  PlainPath("E"),
                  Sequence(Seq(
                    PlainPath("WWS"),
                    Branch(Seq(
                      PlainPath("E"),
                      PlainPath("SS")
                    ))
                  ))
                ))
              ))
            ))
          ))
        ))
      ))
    res shouldBe expected

  }

  "shortestPathToFurthestRoom" should "be correct in test example" in {
    Day20.shortestPathToFurthestRoom(parseRegex("^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$")) shouldBe 23
    Day20.shortestPathToFurthestRoom(parseRegex("^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$")) shouldBe 31
  }

  "shortestToFurthest" should "be correct in test example" in {
    Day20.shortestToFurthest("^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$") shouldBe 23
    Day20.shortestToFurthest("^ENWWW(NEEE|SSE(EE|N))$") shouldBe 10
    Day20.shortestToFurthest("^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$") shouldBe 18
    Day20.shortestToFurthest("^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$") shouldBe 31
  }
}
