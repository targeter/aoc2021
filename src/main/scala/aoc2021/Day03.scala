package aoc2021

import shared._

import scala.annotation.tailrec

case object Day03 extends AocTools(day = 3) {

  def columns(input: List[String]): List[List[Char]] = input.map(_.toList).transpose

  def step1: Int = step1(inputLines)
  def step1(input: List[String]): Int = {
    val (most, least) = findMostLeast(columns(input))
    most * least
  }

  def findMostLeast(cols: Seq[List[Char]]): (Int, Int) = {
    val (most, least) = cols.map(findMostLeastLine).unzip
    (Integer.parseInt(most.mkString, 2), Integer.parseInt(least.mkString, 2))
  }

  @tailrec
  def reduceMost(input: List[String], idx: Int = 0): Int = {
    val (mostTarget, _) = findMostLeastLine(columns(input)(idx))
    val mostO = input.filter(_(idx) == mostTarget)
    if (mostO.size == 1) Integer.parseInt(mostO.head.mkString, 2) else reduceMost(mostO, idx + 1)

  }

  @tailrec
  def reduceLeast(input: List[String], idx: Int = 0): Int = {
    val (_, leastTarget) = findMostLeastLine(columns(input)(idx))
    val mostO = input.filter(_(idx) == leastTarget)
    if (mostO.size == 1) Integer.parseInt(mostO.head.mkString, 2) else reduceLeast(mostO, idx + 1)

  }

  def findMostLeastLine(line: List[Char]): (Char, Char) = {
    val (zeroes, ones) = line.partition(_ == '0')
    if (zeroes.size > ones.size) '0' -> '1' else '1' -> '0'
  }

  def step2: Int = step2(inputLines)
  def step2(input: List[String]): Int = {
    reduceMost(input) * reduceLeast(input)
  }

  def main(args: Array[String]): Unit = {
    println("Step 1: " + step1) // 3901196
    println("Step 2: " + step2) // 4412188
  }
}
