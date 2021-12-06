package aoc2021

import org.scalatest.OptionValues
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers

class Day06Test extends AnyFunSuite with Matchers with OptionValues {

  import aoc2021.Day06._

  private val exampleList = List(3, 4, 3, 1, 2)
  val generationsList = LazyList.iterate(parse(exampleList))(tick)

  test("step1") {
    parse(exampleList) must contain theSameElementsAs (List(Generation(4, 1), Generation(3, 2), Generation(2, 1), Generation(1, 1)))
    generationsList(1).filterNot(_.numberOfFish == 0) must contain theSameElementsAs (List(Generation(3, 1), Generation(2, 2), Generation(1, 1), Generation(0, 1)))

    countFish(generationsList(18)) must be(26)
    countFish(generationsList(80)) must be(5934)
  }

  test("step2") {
    countFish(generationsList(256)) must be(26984457539L)
  }

}
