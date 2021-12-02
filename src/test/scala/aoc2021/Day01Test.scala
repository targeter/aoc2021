package aoc2021

import org.scalatest.OptionValues
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers

class Day01Test extends AnyFunSuite with Matchers with OptionValues {

  import aoc2021.Day01._
  private val exampleList = List(199, 200, 208, 210, 200, 207, 240, 269, 260, 263)

  test("step1") {
    step1(exampleList) must be(7)
  }

  test("step2") {
    step2(exampleList) must be(5)
  }

}
