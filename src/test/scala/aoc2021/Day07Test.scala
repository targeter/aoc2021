package aoc2021

import org.scalatest.OptionValues
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers

class Day07Test extends AnyFunSuite with Matchers with OptionValues {

  import aoc2021.Day07._

  private val exampleList = List(16,1,2,0,4,2,7,1,2,14)

  test("step1") {
    step1(exampleList) must be(37)
  }

  test("step2") {
    step2(exampleList) must be(168)
  }

}
