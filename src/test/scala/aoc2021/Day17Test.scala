package aoc2021

import org.scalatest.OptionValues
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers

class Day17Test extends AnyFunSuite with Matchers with OptionValues {

  import aoc2021.Day17._

  val example = "target area: x=20..30, y=-10..-5"

  test("step1") {
    step1(example) must be(45)
  }

  test("step2") {
    step2(example) must be(112)
  }

}
