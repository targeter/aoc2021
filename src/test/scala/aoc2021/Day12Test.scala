package aoc2021

import org.scalatest.OptionValues
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers

class Day12Test extends AnyFunSuite with Matchers with OptionValues {

  import aoc2021.Day12._

  private val exampleList = List(
      "dc-end",
      "HN-start",
      "start-kj",
      "dc-start",
      "dc-HN",
      "LN-dc",
      "HN-end",
      "kj-sa",
      "kj-HN",
      "kj-dc"
  )

  test("step1") {
    println(parse(exampleList))
    step1(exampleList) must be(19)
  }

  test("step2") {
    step2(exampleList) must be(103)
  }

}
