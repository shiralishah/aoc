package aoc23

import scala.annotation.tailrec

object Day14A extends App {
  val input = scala.io.Source.fromResource("aoc23/run.txt").mkString.split('\n').map(_.toCharArray)
  val inputT = input.transpose
  val loads = inputT.map { col =>
    val (sum, _) = col.zipWithIndex.foldLeft(0L, col.length) {
      case ((s, w), (char, ind)) =>
        char match {
          case '#' => (s, col.length - (ind + 1))
          case '.' => (s, w)
          case 'O' => (s + w, w - 1)
        }
    }
    sum
  }
  val load = loads.sum

  println(load)

}

object Day14B extends App {
  val input = scala.io.Source.fromResource("aoc23/run.txt").mkString.split('\n').toSeq.map(_.toCharArray.toSeq)
  val (rotations, startDiff) = cycleN(input, Map(input -> 0))
  val cycleLength = rotations.size
  val rem = (1000000000 - startDiff) % cycleLength
  val state = rotations.find(x => x._2 == rem).get._1
  val support = getSupport(state)

  println(support)

  def getSupport(inp: Seq[Seq[Char]]): Long = {
    inp.transpose.map {
      col =>
        col.reverse.zipWithIndex.foldLeft(0L) {
          case (s, (char, ind)) =>
            char match {
              case '#' => (s)
              case '.' => (s)
              case 'O' => (s + ind + 1)
            }
        }
    }.sum
  }

  @tailrec
  def cycleN(inp: Seq[Seq[Char]], tracker: Map[Seq[Seq[Char]], Long], count: Int = 1): (Map[Seq[Seq[Char]], Long], Long) = {
    val latest = cycle(inp)
    tracker.get(latest) match {
      case Some(num) =>
        val updated = tracker.collect{
        case (arr, n) if n >= num => arr -> (n - num)
      }
        (updated,  num)
      case None =>
        cycleN(latest, tracker + (latest -> count), count + 1)
    }
  }

  def cycle(inp: Seq[Seq[Char]]): Seq[Seq[Char]] = {
    val north = tilt(inp.transpose).transpose
    val west = tilt(north)
    val south = tilt(west.transpose, reverse = true).transpose
    tilt(south, reverse = true)
  }

  def tilt(inp: Seq[Seq[Char]], reverse: Boolean = false): Seq[Seq[Char]] = {
    inp.map { line =>
      val modified = if (reverse) line.reverse else line
      val (c, buffer, moved) = modified.zipWithIndex.foldLeft(0, 0, "") {
        case ((circ, buff, m), (char, ind)) =>
          char match {
            case '#' => (0, 0,
              m + "O".repeat(circ) + ".".repeat(buff) + "#")
            case '.' => (circ, buff + 1, m)
            case 'O' => (circ + 1, buff, m)
          }
      }
      val output = (moved + "O".repeat(c) + ".".repeat(buffer)).toCharArray
      if (reverse) output.reverse else output
    }
  }

}
//trait Rock
//final case class Ball(x: Int, y: Int, originX: Int, originY: Int, length: Option[Int]) extends Rock
//final case class Cube(x: Int, y: Int) extends Rock
//final case class Empty(x: Int, y: Int) extends Rock
//final case class Platform(inp: Array[Array[Rock]]) {
//  def tilt(): Platform = {
//    val arr =
//    Platform(this.inp)
//  }
//}
//
//object Day14C extends App {
//  val input = scala.io.Source.fromResource("aoc23/test.txt").mkString.split('\n').map(_.toCharArray)
//  val arr = input.zipWithIndex.foldLeft(Array.empty[Array[Rock]]){
//    case (rocks, (row, y)) =>
//      val r = row.zipWithIndex.foldLeft(Array.empty[Rock]) {
//      case (rox, (ch, x)) =>
//        ch match {
//        case 'O' => rox :+ Ball(x, y, x, y, None)
//        case '#' => rox :+ Cube(x, y)
//        case '.' => rox :+ Empty(x, y)
//        }
//    }
//    rocks :+ r
//  }
//
//  val platform = Platform(arr)
//
//  def getSupport(inp: Array[Array[Char]]): Long = {
//    inp.transpose.map{
//      col =>
//        col.zipWithIndex.foldLeft(0L) {
//          case (s, (char, ind)) =>
//            char match {
//              case '#' => (s)
//              case '.' => (s)
//              case 'O' => (s + col.length - (ind+1))
//            }
//        }
//    }.sum
//  }
//
////  def cycle(inp: Array[Array[Char]]): Array[Array[Char]] = {
////    val north = tilt(inp.transpose).transpose
////    val west = tilt(north)
////    val south = tilt(west.transpose, reverse = true).transpose
////    tilt(south, reverse = true)
////  }
//
//  def tilt(inp: Array[Array[Char]], rocks: Seq[Rock], reverse: Boolean = false): Array[Array[Char]] = {
//    inp.zipWithIndex.foldLeft(Seq.empty[Rock])
//    { case(rx, (line, y)) =>
//      val modified = if (reverse) line.reverse else line
//      val (c, buffer, moved, rs, t) = modified.zipWithIndex.foldLeft(0, 0, "", Seq.empty[Rock], 0) {
//        case ((circ, buff,  m, rox, top), (char, x)) =>
//          char match {
//            case '#' => (0, 0,
//              m + "O".repeat(circ) + ".".repeat(buff) + "#",
//              rox,
//              x+1
//            )
//            case '.' => (circ, buff+1, m, rox, top)
//            case 'O' =>
//              val r = rocks.find(e => e.x == x && e.y == y).get.copy(x = )
//              (circ+1, buff, m, rox :+ r, top+1)
//          }
//      }
//      val output = (moved + "O".repeat(c) + ".".repeat(buffer)).toCharArray
//      val arr = if (reverse) output.reverse else output
//
//
//    }
//  }
//
//}
