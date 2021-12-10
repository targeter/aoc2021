package aoc2021

import org.scalatest.OptionValues
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers

class Day10Test extends AnyFunSuite with Matchers with OptionValues {

  import aoc2021.Day10._

  private val exampleList = List(
    "[({(<(())[]>[[{[]{<()<>>",
    "[(()[<>])]({[<{<<[]>>(",
    "{([(<{}[<>[]}>{[]{[(<()>",
    "(((({<>}<{<{<>}{[]{[]{}",
    "[[<[([]))<([[{}[[()]]]",
    "[{[{({}]{}}([{[{{{}}([]",
    "{<[[]]>}<{[{[{[]{()[[[]",
    "[<(<(<(<{}))><([]([]()",
    "<{([([[(<>()){}]>(<<{{",
    "<{([{{}}[<[[[<>{}]]]>[]]"
  )

  test("step1") {
    exampleList.map(findScore).count(_.isLeft) must be(5)
    step1(exampleList) must be(26397)
  }

  test("step2") {
    exampleList.map(findScore).count(_.isRight) must be(5)
    step2(exampleList) must be(288957)
  }

}
