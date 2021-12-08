package aoc2021

import mouse.all.anySyntaxMouse
import shared._

case object Day07 extends AocTools(day = 7) {

  def linearCost(target: Int, origin: Int): Int = (target - origin).abs

  def increasingCost(linearCost: Int): Int = linearCost + (1 until linearCost).sum

  def findMinFuelCostLinear(input: Seq[Int]): Int =
    (1 to input.max).map(target => input.map(linearCost(target, _)).sum).min

  def findMinFuelCostIncreasing(input: Seq[Int]): Int =
    (1 to input.max).map { target =>
      input.map(linearCost(target, _) |> increasingCost).sum
    }.min

  def step1: Int = step1(inputLineInts)
  def step1(input: Seq[Int]): Int = findMinFuelCostLinear(input)

  def step2: Int = step2(inputLineInts)
  def step2(input: Seq[Int]): Int = findMinFuelCostIncreasing(input)

  def main(args: Array[String]): Unit = {
    println("Step 1: " + step1) // 325528
    println("Step 2: " + step2) // 85015836
  }

}
