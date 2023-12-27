package aoc23

import zio.{ZIO, ZIOAppDefault}

import scala.util.Random

object HelloWorld extends ZIOAppDefault {
  override def run = program
    .tapErrorCause(ZIO.logErrorCause(_)).exitCode

  def program = ZIO.scoped {
    IOHelper.fileStream("test.txt") >>> (IOHelper.fileSink("output.txt"))
  }

}

object Fragers extends App {
  val input = scala.io.Source.fromResource("aoc23/test.txt").getLines()
  val random = Random

  val (startGraph, startEdges) = input.foldLeft(Map.empty[String, Set[String]], Set.empty[(Set[String], Set[String])]) {
    case ((g, e), line) =>
      line match {
        case s"$name: $neighStr" =>
          val neighbors = neighStr.split(" ")
          val (updateG, updateE) = neighbors.foldLeft(Map.empty[String, Set[String]], Set.empty[(Set[String], Set[String])]) {
            case ((m, s), n) =>
              (m + (n -> (g.getOrElse(n, Set.empty) + name)), s + ((Set(name), Set(n))))
          }
          (g + (name -> neighbors.toSet) ++ updateG, e ++ updateE)
      }
  }

  def contract(stEdges: Set[(Set[String], Set[String])]): Set[(Set[String], Set[String])] = {
    (1 to startGraph.size - 2).foldLeft(stEdges) {
      case (edges, _) =>
        val num = random.nextInt(edges.size)
        val edge: (Set[String], Set[String]) = edges.toSeq(num)
        val combined = edge._1 ++ edge._2
        val updated = edges.foldLeft(Set.empty[(Set[String], Set[String])]) {
          case (acc, (left, right)) =>
            if ((left ++ right).forall(combined.contains)) {
              // self loop
              acc
            } else if (left == edge._1 || left == edge._2) {
              acc + ((combined, right))
            } else if (right == edge._1 || right == edge._2) {
              acc + ((left, combined))
            } else acc + ((left, right))
        }
        updated
    }
  }

  def runContraction: (Set[String], Set[String]) = {
    var grps = (Set.empty[String], Set.empty[String])
    var min = Integer.MAX_VALUE
    while (min != 3) {

      val result = contract(startEdges)
      val lastEdge = result.head
      val cutEdges = startEdges.collect {
        case (left, right) if (left.forall(lastEdge._1.contains) && right.forall(lastEdge._2.contains)) ||
          (left.forall(lastEdge._2.contains) && right.forall(lastEdge._1.contains)) =>
          (left, right)
      }
      if (cutEdges.size < min) {
        min = cutEdges.size
        grps = lastEdge
      }
    }
    grps
  }

  val groups = runContraction
  val answer = (groups._1.size * groups._2.size)

  println(answer)
}