package org.primetalk.advent.tools

case class Complex(re: Int, im: Int)

object Complex {

  implicit class ComplexOps(c: Complex) {
    def *(o: Complex): Complex = Complex(c.re * o.re - c.im * o.im, c.re * o.im + c.im * o.re)
    def +(o: Complex): Complex = Complex(c.re + o.re, c.im + o.im)
    def -(o: Complex): Complex = Complex(c.re - o.re, c.im - o.im)
    def /(o: Complex): Complex = {
      val or2 = o.re * o.re + o.im * o.im
      Complex((c.re * o.re + c.im * o.im)/or2, (c.im * o.re - c.re * o.im)/or2)
    }
  }

  implicit class IntOps(i: Int) {
    def lift: Complex = Complex(i, 0)
    def imaginary: Complex = Complex(0, i)
  }

  val Zero = Complex(0, 0)

  val Im = Complex(0, 1)
  val Re1 = Complex(1, 0)

  val angle90: Complex = Im

  val anglesBy90 = Array(Re1, Im, Zero - Re1, Zero - Im)
  @inline
  def angle90power(i: Int): Complex = anglesBy90(i % 4)
}
