package aoc21

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object Day10 {
  val bad = Map(
    ')' -> 3,
    ']' -> 57,
    '}' -> 1197,
    '>' -> 25137
  )

  @tailrec
  def checkValid(str: String, queue: Queue[Char], index: Int): (Option[Char], Queue[Char]) =
    if (index == str.length) {
      (None, queue)
    } else if (!bad.keySet.contains(str.charAt(index))) {
      val c = str.charAt(index)
      checkValid(str, c +: queue, index + 1)
    } else {
      val c = str.charAt(index)
      val (last, rem) = queue.dequeue
      c match {
        case ')' if last == '(' => checkValid(str, rem, index + 1)
        case ']' if last == '[' => checkValid(str, rem, index + 1)
        case '}' if last == '{' => checkValid(str, rem, index + 1)
        case '>' if last == '<' => checkValid(str, rem, index + 1)
        case _ => (Some(c), Queue.empty)
      }
    }
}

object Day10A extends App {

  val input = scala.io.Source.fromResource("aoc21/day10.txt").getLines().toSeq
  val invalid = input.map(line => Day10.checkValid(line, Queue.empty[Char], 0)._1).flatten
  val score = invalid.map(ch => Day10.bad.getOrElse(ch, 0)).sum
  println(score)
}

object Day10B extends App {
  val input = scala.io.Source.fromResource("aoc21/day10.txt").getLines().toSeq
  val scores = input.map { line =>
    val (error, rem) = Day10.checkValid(line, Queue.empty[Char], 0)
    Option.when(error.isEmpty){
      rem.foldLeft(0L){ case (sum, left) =>
        val total = sum*5
        left match {
          case '(' => total + 1
          case '[' => total + 2
          case '{' => total + 3
          case _ => total + 4
        }
      }
    }
  }.flatten.sorted
  val middle = scores(scores.length/2)
  println(middle)
}
