package org.primetalk.advent

import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}

abstract class BaseTest extends FlatSpec with Matchers with PropertyChecks
