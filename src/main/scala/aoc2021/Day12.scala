package aoc2021

import shared._

case object Day12 extends AocTools(day = 12) {

  case class Cave(name: String, connects: Set[Cave]) {
    val big = name == name.toLowerCase
  }

  def parse(input: Seq[String]): Seq[(String, String)] = input.map { case s"$a-$b" =>
    a -> b
  }

  def buildMap(input: Seq[(String, String)]): Map[String, Set[String]] = {
    val bothWays = (input ++ input.map(_.swap))
    bothWays.groupBy(_._1).view.mapValues(_.map(_._2).toSet).toMap
  }

  def findPathsToEnd2(caveMap: Map[String, Set[String]], revisit: Boolean = false): Seq[Seq[String]] = {
    def paths(currentMap: Map[String, Set[String]], current: String, path: Seq[String], revisit: Boolean): Seq[Seq[String]] = {
      val connections = currentMap.getOrElse(current, Set.empty)
      if (current == "end") Seq(path) // end!
      else if (connections.isEmpty) Seq.empty // _dead_ end!
      else {
        val isSmall = current.toLowerCase == current
        val revisitPaths =
          if (revisit && isSmall)
            connections.toSeq.flatMap(nextCave => paths(currentMap, nextCave, path :+ nextCave, revisit = false))
          else Seq.empty

        val newMap = if (isSmall) currentMap.view.mapValues(_ - current).toMap else currentMap
        val regularPaths = connections.toSeq.flatMap(conn => paths(newMap, conn, path :+ conn, revisit))
        revisitPaths ++ regularPaths
      }
    }

    paths(caveMap.view.mapValues(_ - "start").toMap, "start", Seq("start"), revisit)
  }

  def step1: Int = step1(inputLines)

  def step1(input: Seq[String]): Int = {
    findPathsToEnd2(buildMap(parse(input))).filterNot(_.isEmpty).size
  }

  def step2: Int = step2(inputLines)

  def step2(input: Seq[String]): Int = {
    val paths = findPathsToEnd2(buildMap(parse(input)), true).filterNot(_.isEmpty)
    paths.distinct.size
  }

  def main(args: Array[String]): Unit = {
    println("Step 1: " + step1)
    println("Step 2: " + step2)
  }

}
