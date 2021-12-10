package aoc2021

import shared._

import scala.annotation.tailrec

case object Day10 extends AocTools(day = 10) {

  private val pairMap = ("([{<" zip ")]}>").toMap

  val pointsForMismatch = Map(
    ')' -> 3,
    ']' -> 57,
    '}' -> 1197,
    '>' -> 25137
  )

  val pointsForClosing = Map(
    '(' -> 1,
    '[' -> 2,
    '{' -> 3,
    '<' -> 4
  )

  def findScore(line: String): Either[Int, Seq[Char]] = {
    @tailrec
    def recurse(remaining: Seq[Char], stack: Seq[Char]): Either[Int, Seq[Char]] = {
      if (remaining.isEmpty) Right(stack)
      else if (pairMap.contains(remaining.head)) recurse(remaining.tail, remaining.head +: stack)
      else if (remaining.head == pairMap(stack.head)) recurse(remaining.tail, stack.tail)
      else Left(pointsForMismatch(remaining.head))
    }
    recurse(line.toSeq, Seq.empty)
  }

  def score(unclosed: Seq[Char]): Long = unclosed.foldLeft(0L) { (score, c) =>
    score * 5 + pointsForClosing(c)
  }

  def step1: Int = step1(inputLines)

  def step1(input: Seq[String]): Int = input.map(findScore).flatMap(_.left.toSeq).sum

  def step2: Long = step2(inputLines)

  def step2(input: Seq[String]): Long = {
    val scores = input
      .map(findScore)
      .flatMap(_.toSeq)
      .map(score)

    scores.sorted.apply(scores.size / 2)
  }

  def main(args: Array[String]): Unit = {
    println("Step 1: " + step1)
    println("Step 2: " + step2)
  }

}
