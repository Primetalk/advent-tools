package org.primetalk.advent

import org.scalacheck.{Arbitrary, Gen}

import scala.reflect.ClassTag

trait Generators {

  def displayGen[T: Numeric : ClassTag]: Gen[Display[T]] =
    for{
      a <- Gen.choose(-1000,1000)
      b <- Gen.choose(-1000,1000)
      c <- Gen.choose(0,1000)
      d <- Gen.choose(0,1000)
      display = Display((a,b), (c, d))()
    } yield display
  implicit def arbitraryDisplay[T: Numeric : ClassTag]: Arbitrary[Display[T]] = Arbitrary(displayGen)

  def intDisplayGen: Gen[Display[Int]] =
    for{
      display <- displayGen[Int]
      points = display.points
      values <- Gen.listOfN(points.size, Gen.choose(Int.MinValue, Int.MaxValue))
      _ = { points.zip(values).foreach{ case (pp, v) => display(pp) = v } }
    } yield display


}
