package org.primetalk.advent3.tools

object TimeMeasurementUtils:

  type TimeMeasure[A] = (Long, A)

  def measure[A](f: => A): TimeMeasure[A] =
    val start = System.nanoTime()
    val res = f
    val end = System.nanoTime()
    ((end - start) / 1000000, res)
  
  extension [A](m:TimeMeasure[A])
    def report(f: Long => Unit): A =
      f(m._1)
      m._2
