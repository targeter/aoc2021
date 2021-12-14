package aoc2021

import org.scalatest.OptionValues
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers

class Day14Test extends AnyFunSuite with Matchers with OptionValues {

  import aoc2021.Day14._

  private val exampleList = List(
    "NNCB",
    "",
    "CH -> B",
    "HH -> N",
    "CB -> H",
    "NH -> C",
    "HB -> C",
    "HC -> B",
    "HN -> C",
    "NN -> C",
    "BH -> H",
    "NC -> B",
    "NB -> B",
    "BN -> B",
    "BB -> N",
    "BC -> B",
    "CC -> N",
    "CN -> C"
  )

  test("step1") {
//    val (template, rules) = parse2(exampleList)
//    val results = LazyList.iterate(template)(applyRules(_, rules))
//    def pairCount(in: String) = in.sliding(2).toList.groupBy(identity).mapValues(_.size).toMap
//
//    results(1) must be(pairCount("NCNBCHB"))
//    results(2) must be(pairCount("NBCCNBBBCBHCB"))
//    results(3) must be(pairCount("NBBBCNCCNBBNBNBBCHBHHBCHB"))
//    results(4) must be(pairCount("NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB"))
    step2(exampleList) must be(1588)
  }

  test("step2") {
    step2(exampleList) must be(0)
  }

}
