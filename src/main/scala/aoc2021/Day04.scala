package aoc2021

import shared._

import scala.annotation.tailrec
import mouse.boolean._
import shared.ops.StringOps

case object Day04 extends AocTools(day = 4) {

  case class Board(numbers: List[List[Int]], winningNumber: Option[Int], round: Int = 0) {
    // Mark number, determine win, record round
    def mark(called: Int): Board = winningNumber.map(_ => this).getOrElse {
      val newNumbers = numbers.map(_.map(n => if (n == called) 0 else n))
      val won = (newNumbers ++ newNumbers.transpose).exists(_.forall(_ == 0))

      copy(newNumbers, won.option(called), round + 1)
    }

    def score: Int = winningNumber.map(_ * numbers.flatten.sum).getOrElse(0)
  }

  def parse(input: Seq[String]): (List[Int], List[Board]) = {
    val numbers = input.head.split(",").toList.map(_.toInt)
    val boards = input.tail
      .grouped(6)
      .map(_.drop(1))
      .map(_.toList.map(_.parseInts))
      .toList

    numbers -> boards.map(Board(_, None))
  }

  @tailrec
  def bingo(numbers: List[Int], boards: List[Board]): List[Board] = {
    if (numbers.isEmpty) boards
    else bingo(numbers.tail, boards.map(_.mark(numbers.head)))
  }

  def step1: Int = step1(inputLines)
  def step1(input: List[String]): Int = {
    val (numbers, boards) = parse(input)
    bingo(numbers, boards).minBy(_.round).score
  }

  def step2: Int = step2(inputLines)
  def step2(input: List[String]): Int = {
    val (numbers, boards) = parse(input)
    bingo(numbers, boards).maxBy(_.round).score
  }

  def main(args: Array[String]): Unit = {
    println("Step 1: " + step1) // 58374
    println("Step 2: " + step2) // 11377
  }
}
