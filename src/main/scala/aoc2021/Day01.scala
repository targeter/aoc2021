package aoc2021

import shared._

case object Day01 extends AocTools(day = 1) {

  def step1: Int = step1(inputInts)

  def step1(measurements: Seq[Int]): Int = measurements
    .foldLeft((0, Integer.MAX_VALUE)) {
      case ((count, prev), curr) if curr > prev => count + 1 -> curr
      case ((count, _), curr)                   => count -> curr
    }
    ._1

  def step2: Int = step1(inputInts.sliding(3).map(_.sum).toSeq)

  def main(args: Array[String]): Unit = {
    println("Step 1: " + step1)
    println("Step 2: " + step2)
  }
}
