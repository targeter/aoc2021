package aoc2021

import shared._

import scala.annotation.tailrec

case object Day10 extends AocTools(day = 10) {

  private val allOpen = "([{<"
  private val allClosing = ")]}>"

  private val pairs = allOpen zip allClosing
  private val pairMap = pairs.toMap

  val scores = Map(
    ')' -> 3,
    ']' -> 57,
    '}' -> 1197,
    '>' -> 25137
  )

  val closingScores = Map(
    '(' -> 1,
    '[' -> 2,
    '{' -> 3,
    '<' -> 4
  )

  def findScore(line: String): (List[Char], Int) = {
    line.foldLeft((List.empty[Char], 0)) { case ((stack: Seq[Char], score: Int), c: Char) =>
      if (score > 0) (stack, score)
      else if (pairMap.contains(c)) (c +: stack, score)
      else if (c == pairMap(stack.head)) (stack.tail, score)
      else (List.empty, scores(c))
    }
  }

  def score(unclosed: Seq[Char]): Long = unclosed.foldLeft(0L) { (score, c) =>
    score * 5 + closingScores(c)
  }

  def step1: Int = step1(inputLines)

  def step1(input: Seq[String]): Int = input.map(findScore).map(_._2).sum

  def step2: Long = step2(inputLines)

  def step2(input: Seq[String]): Long = {
    val scores = input.map(findScore).collect { case (unclosed, 0) =>
      score(unclosed)
    }

    scores.sorted.apply(scores.size / 2)
  }

  def main(args: Array[String]): Unit = {
    println("Step 1: " + step1)
    println("Step 2: " + step2)
  }

}
