package aoc2021

import org.scalatest.OptionValues
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers

class Day02Test extends AnyFunSuite with Matchers with OptionValues {

  import aoc2021.Day02._
  private val exampleList = List(
  "forward 5",
  "down 5",
  "forward 8",
  "up 3",
  "down 8",
  "forward 2"
  )

  test("step1") {
    process(parse(exampleList)) must be(Position(15, 10))
    step1(exampleList) must be(150)
  }

  test("step2") {
    val Position2(h, d, _) = process2(parse(exampleList))
    h must be(15)
    d must be(60)

    step2(exampleList) must be(900)


  }

}
