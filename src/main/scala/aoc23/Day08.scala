package aoc23

import scala.annotation.tailrec

object Day08A extends App {
  val input = scala.io.Source.fromResource("aoc23/run.txt").getLines().toSeq
  val (instructions, nodes) = (input.head.toCharArray, input.tail.tail)

  val nodeMap = nodes.foldLeft(Map.empty[String, (String, String)]) {
    case (map, line) =>
      line match {
        case s"$key = ($left, $right)" => map + (key -> (left, right))
      }
  }

  @tailrec
  def findPath(curr: String, count: Long): Long = {
    if (curr == "ZZZ") count
    else {
      val options = nodeMap(curr)
      val ind = count % instructions.length
      val next = instructions(ind.toInt) match {
        case 'L' => options._1
        case 'R' => options._2
      }
      findPath(next, count + 1)
    }
  }

  val path = findPath("AAA", 0)

  println(path)
}

object Day08B extends App {
  val input = scala.io.Source.fromResource("aoc23/run.txt").getLines().toSeq
  val (instructions, nodes) = (input.head.toCharArray, input.tail.tail)

  val nodeMap = nodes.foldLeft(Map.empty[String, (String, String)]) {
    case (map, line) =>
      line match {
        case s"$key = ($left, $right)" => map + (key -> (left, right))
      }
  }

  val starts = nodeMap.keys.filter("\\b\\w{2}A\\b".r.matches).toSeq
  val ends = nodeMap.keys.filter("\\b\\w{2}Z\\b".r.matches).toSeq

  val edges = starts.map(s => findPath(s, 0)).reduce(lcm)

  println(edges)

  @tailrec
  def gcd(a: Long, b: Long): Long = if (b == 0) a else gcd(b, a % b)

  def lcm(a: Long, b: Long): Long = if (a == 0 || b == 0) 0 else Math.abs(a * b) / gcd(a, b)

  @tailrec
  def findPath(curr: String, count: Long): Long = {
    if (ends.contains(curr)) count
    else {
      val options = nodeMap(curr)
      val ind = count % instructions.length
      val next = instructions(ind.toInt) match {
        case 'L' => options._1
        case 'R' => options._2
      }
      findPath(next, count + 1)
    }
  }
}
