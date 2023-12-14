package aoc23

import scala.annotation.tailrec

object Day13A extends App {
  val input: Array[Seq[Seq[Char]]] = scala.io.Source.fromResource("aoc23/run.txt").mkString
    .split("\n\n").map(_.split('\n').toSeq.map(_.toCharArray.toSeq))

  val sum = input.foldLeft(0L){
    case (acc, arr) =>
      val rows = arr.zipWithIndex
      val r = collectMirror(rows)

      r match {
        case Some(value) => acc + (value * 100)
        case None =>
          val cols = arr.transpose.zipWithIndex
          val c = collectMirror(cols)
          acc + c.getOrElse(0)
      }
  }

  def collectMirror(seq: Seq[(Seq[Char], Int)]): Option[Int] = {
    seq.tail.collectFirst {
      case (x, ind) if seq.head._1 == x && checkMirror(ind, seq.map(_._1)) =>
        (ind - 1) / 2 + 1
    }.orElse {
      seq.take(seq.length - 1).collectFirst {
        case (x, ind) if seq.last._1 == x && checkMirror(ind, seq.map(_._1), isHead = false) =>
          seq.length - 1 - ((seq.length - ind) / 2) + 1
      }
    }
  }

  def checkMirror(ind: Int, seq: Seq[Seq[Char]], isHead: Boolean = true): Boolean = {
    if (isHead) {
      val mirror = (ind - 1) / 2
      val top = seq.slice(0, mirror + 1).reverse
      val bottom = seq.slice(mirror + 1, 2 * mirror + 1)
      val cond = top.zip(bottom).forall(a => a._1 == a._2)
      (ind - 1) % 2 == 0 && cond
    } else {
      val diff = (seq.length - ind) / 2
      val mirror = seq.length - 1 - diff
      val top = seq.slice(mirror + 1 - diff, mirror + 1).reverse
      val bottom = seq.slice(mirror + 1, seq.length)
      val cond = top.zip(bottom).forall(a => a._1 == a._2)
      (seq.length - ind) % 2 == 0 && cond
    }
  }

  println(sum)

}

object Day13B extends App {
  val input: Array[Seq[Seq[Char]]] = scala.io.Source.fromResource("aoc23/run.txt").mkString
    .split("\n\n").map(_.split('\n').toSeq.map(_.toCharArray.toSeq))

  val sum = input.foldLeft(0L) {
    case (acc, arr) =>
      val r = trySmudge(0, arr)

      r match {
        case Some(value) => acc + (value * 100)
        case None =>
          val cols = arr.transpose
          val c = trySmudge(0, cols)
          acc + c.getOrElse(0)
      }
  }

  @tailrec
  def trySmudge(mirror: Int, seq: Seq[Seq[Char]]): Option[Int] = {
    if (mirror == seq.length) None
    else {
      val buffer = Math.min(mirror+1, seq.length-(mirror+1))
      val top = seq.slice((mirror+1 - buffer), mirror + 1).reverse
      val bottom = seq.slice(mirror + 1, mirror + buffer + 1)
      val matching = top.zip(bottom).map {
        case (a, b) => a.zip(b).count(c => c._1 != c._2)
      }.sum
      if (matching == 1) {
        Some(mirror+1)
      } else trySmudge(mirror+1, seq)
    }
  }

  println(sum)

}
