package aoc21

import scala.annotation.tailrec

object Day03A extends App {
  val input    = scala.io.Source.fromResource("aoc21/day03.txt").getLines().toSeq
  val gammaStr = input.transpose.map(arr => arr.groupBy(identity).maxBy(_._2.size)._1).mkString
  val gamma    = Integer.parseInt(gammaStr, 2)
  val epsilon  = gamma ^ Integer.parseInt("1" * gammaStr.length, 2)
  println(gamma * epsilon)
}

object Day03B extends App {
  val input  = scala.io.Source.fromResource("aoc21/day03.txt").getLines().map(_.toCharArray).toArray
  val oxygen = Integer.parseInt(filter(0, input).mkString, 2)
  val carbon = Integer.parseInt(filter(0, input, false).mkString, 2)
  println(oxygen * carbon)

  @tailrec
  def filter(i: Int, matrix: Array[Array[Char]], isMax: Boolean = true): Array[Char] =
    if (matrix.length == 1) {
      matrix.head
    } else {
      val target   = find(i, matrix, isMax)
      val filtered = matrix.filter(arr => arr(i) == target)
      filter(i + 1, filtered, isMax)
    }

  def find(i: Int, matrix: Array[Array[Char]], isMax: Boolean): Char = {
    val transpose = matrix.transpose
    val grouped   = transpose(i).groupBy(identity).map(x => (x._1, x._2.length))
    if (isMax) {
      if (grouped.getOrElse('0', 0) > grouped.getOrElse('1', 0)) '0' else '1'
    } else {
      if (grouped.getOrElse('1', 0) < grouped.getOrElse('0', 0)) '1' else '0'
    }
  }
}
