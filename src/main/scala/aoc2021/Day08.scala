package aoc2021

import shared._

import scala.annotation.tailrec

case object Day08 extends AocTools(day = 8) {

  val uniqueSizeMapping = Map(
    1 -> 2,
    4 -> 4,
    7 -> 3,
    8 -> 7
  )

  def parse(input: Seq[String]): Seq[(List[String], List[String])] = input.map(parse)

  def parse(line: String): (List[String], List[String]) = {
    val List(input, output) = line.split(""" \| """).toList
    input.split(" ").toList.map(_.sorted) -> output.split(" ").toList.map(_.sorted)
  }

  @tailrec
  def deduce(line: Seq[String], knownPatterns: Map[Int, String], knownChars: Map[Char, Char]): Map[String, Int] = {
    if (knownPatterns.isEmpty) {
      deduce(line, uniqueSizeMapping.view.mapValues(size => line.find(_.length == size).get).toMap, knownChars)
    } else if (!knownChars.contains('a')) {
      val a: Char = (knownPatterns(7).toSet -- knownPatterns(1).toSet).head
      deduce(line, knownPatterns, Map('a' -> a))
    } else if (!knownPatterns.contains(5)) { // 5 is degene met length 5 (3, 2, 5) en b (vanuit 4). Allen hebben d.  bd = 4 -1
      val target = (knownPatterns(4).toSet -- knownPatterns(1).toSet).toList
      val tuple = (target.head, target(1))

      val candidates = line.filter(_.length == 5)

      val count = candidates.count(_.contains(target.head))
      val (b, d) = if (count == 1) tuple else tuple.swap
      val five = candidates.find(_.contains(b)).get
      deduce(line, knownPatterns.updated(5, five), knownChars.updated('b', b).updated('d', d))
    } else if (!knownChars.contains('f')) { // f is het segment uit 1 dat ook in 5 zit
      val matches = (knownPatterns(1) intersect knownPatterns(5))
      assert(matches.length == 1)
      deduce(line, knownPatterns, knownChars.updated('f', matches.head))
    } else if (!knownChars.contains('c')) { // c is het segment uit 1 dat niet f is
      val matches = knownPatterns(1).toSet - knownChars('f')
      assert(matches.size == 1)
      deduce(line, knownPatterns, knownChars.updated('c', matches.head))
    } else if (!knownPatterns.contains(3)) { // 3 is degene met 5 die niet f heeft
      val result = line.filter(l => l.length == 5 && l != knownPatterns(5)).filter(_.contains(knownChars('f')))
      assert(result.size == 1)
      deduce(line, knownPatterns.updated(3, result.head), knownChars)
    } else if (!knownPatterns.contains(0)) { // 0 is die van 6 zonder d
      val result = line.filter(l => l.length == 6 && !l.contains(knownChars('d')))
      assert(result.size == 1)
      deduce(line, knownPatterns.updated(0, result.head), knownChars)
    } else if (!knownPatterns.contains(9)) { // 6 and 9 to go. Verschil is c en e. c is bekend.
      val result = line.filter(l => l.length == 6 && l != knownPatterns(0))
      val resultNine = result.filter(_.contains(knownChars('c')))
      val resultSix = result.filterNot(_.contains(knownChars('c')))
      assert(resultNine.size == 1)
      assert(resultSix.size == 1)

      deduce(line, knownPatterns.updated(6, resultSix.head).updated(9, resultNine.head), knownChars)
    } else {
      val two = line.toSet -- knownPatterns.values.toSet
      assert(two.size == 1)
      knownPatterns.updated(2, two.head).map(_.swap)
    }

  }

  def step1: Int = step1(inputLines)

  def step1(input: Seq[String]): Int = {
    val targets = Set(2, 4, 3, 7)
    val outputs = input.map(parse).flatMap(_._2)
    outputs.map(_.length).count(targets)
  }

  def step2: Int = step2(inputLines)

  def step2(input: Seq[String]): Int = {
    val parsed = parse(input)

    val outputs = parsed.map { case (inputs, outputs) =>
      val lookup = deduce(inputs, Map.empty, Map.empty)
      outputs.map(lookup).mkString.toInt
    }

    outputs.sum
  }

  def main(args: Array[String]): Unit = {
    println("Step 1: " + step1) // 237
    println("Step 2: " + step2) // 1009098
  }

}
