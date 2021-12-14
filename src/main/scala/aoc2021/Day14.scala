package aoc2021

import shared._

case object Day14 extends AocTools(day = 14) {

  case class Rule(insert: String) {
    def apply(input: String): String = input.head + insert
  }

  def parse(input: Seq[String]): (String, Map[String, Rule]) = {
    val template = input.head
    val rules = input.tail.tail.map { case s"$pair -> $insert" =>
      pair -> Rule(insert)
    }
    (template, rules.toMap)
  }

  def parse2(input: Seq[String]): (List[(Pair, Long)], Map[Char, Long], Rules) = {
    val templatePairs = input.head.sliding(2).map(p => (p.head, p.last)).toList
    val pairCounts = templatePairs.groupBy(identity).map{case (k,v) => k -> v.size.toLong}.toList
    val rules = input.tail.tail.map { case s"$pair -> $insert" =>
      (pair.head, pair.last) -> insert.head
    }
    val elemCounts = input.head.groupBy(identity).view.mapValues(_.length.toLong).toMap

    (pairCounts, elemCounts, rules.toMap)
  }

  type Rules = Map[(Char, Char), Char]
  type Pair = (Char, Char)

  def applyRules(input: String, rules: Map[String, Rule]): String = {
    input.sliding(2).foldLeft("") { (str, pair) => str + rules.get(pair).map(_.apply(pair)).getOrElse(pair.head.toString) } + input.last
  }

  def applyRules(pairCounts: List[(Pair, Long)], elementCount: Map[Char, Long], rules: Rules) = {
    val newPairCounts =
      pairCounts
        .flatMap { case (pair, pairCount) =>
          val (a, b) = pair
          val c = rules(pair)
          List((a, c) -> pairCount, (c, b) -> pairCount)
        }
        .groupBy(_._1)
        .view
        .mapValues(_.map(_._2).sum)
        .toList

    val newElemCounts = pairCounts.foldLeft(elementCount)({ case (acc, (pair, count)) =>
      val newElement = rules(pair)
      acc.updated(newElement, acc.getOrElse(newElement, 0L) + count)
    })

    (newPairCounts, newElemCounts)
  }

  def parseRule(s: String): ((Char, Char), Char) = {
    val Seq(left, right) = s.split(" -> ", 2).toSeq
    (left(0), left(1)) -> right(0)
  }

  def step1: Int = step1(inputLines)

  def step1(input: Seq[String]): Int = {
    val (template, rules) = parse(input)
    val results = LazyList.iterate(template)(applyRules(_, rules))
    val resultAt10 = results(10).groupBy(identity).view.mapValues(_.length).values.toList
    resultAt10.max - resultAt10.min

  }

  def step2: Long = step2(inputLines)

  def step2(input: Seq[String]): Long = {
    val (pairCount, elemCount, rules) = parse2(input)
    val results: Seq[(List[(Pair, Long)], Map[Char, Long])] = LazyList.iterate((pairCount, elemCount))(a => applyRules(a._1, a._2, rules))
    val resultAt40 = results(40)._2.values

    resultAt40.max - resultAt40.min

  }

  def main(args: Array[String]): Unit = {
    println("Step 1: " + step1) // 3408
    println("Step 2: " + step2) // 3724343376942
  }

}
