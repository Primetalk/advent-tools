package org.primetalk.advent3.tools

import org.scalatest.{OptionValues, Inside, Inspectors}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

abstract class UnitSpec extends AnyFlatSpec with should.Matchers 
  with OptionValues with Inside with Inspectors
