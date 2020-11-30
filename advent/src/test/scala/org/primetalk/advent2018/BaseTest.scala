package org.primetalk.advent2018

import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

abstract class BaseTest extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks
