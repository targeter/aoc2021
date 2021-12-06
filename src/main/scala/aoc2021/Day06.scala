package aoc2021

import mouse.all.anySyntaxMouse
import shared._

case object Day06 extends AocTools(day = 6) {

  type School = Seq[Generation]

  case class Generation(countDown: Int, numberOfFish: Long) {
    def tick: Generation = copy(countDown = if (countDown <= 0) 6 else countDown - 1)
  }

  def tick(school: School): School = {
    val nextGen = school.map(_.tick) ++ school.filter(_.countDown == 0).map(_.copy(countDown = 8))
    val (gen6, tng) = nextGen.partition(_.countDown == 6)
    val mergedGen6 = Generation(6, gen6.map(_.numberOfFish).sum)

    tng :+ mergedGen6
  }

  def countFish(school: School): Long = school.map(_.numberOfFish).sum

  def parse(input: Seq[Int]): School = (0 to 6).map(i => Generation(i, input.count(_ == i).toLong)).filter(_.numberOfFish > 0)

  def step1: Long = step1(inputLineInts)
  def step1(input: Seq[Int]): Long = {
    val school = parse(input)
    LazyList.iterate(school)(tick)(80) |> countFish
  }

  def step2: Long = step2(inputLineInts)
  def step2(input: Seq[Int]): Long = {
    val school = parse(input)
    LazyList.iterate(school)(tick)(256) |> countFish
  }

  def main(args: Array[String]): Unit = {
    println("Step 1: " + step1) // 390923
    println("Step 2: " + step2) // 1749945484935
  }

}
