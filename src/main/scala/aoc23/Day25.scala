package aoc23

import org.jgrapht.alg.StoerWagnerMinimumCut
import org.jgrapht.graph.{DefaultEdge, SimpleGraph}

object Day25A extends App {

  val input = scala.io.Source.fromResource("aoc23/run.txt").getLines()
  val graph = new SimpleGraph[String, DefaultEdge](classOf[DefaultEdge])

  val (startVertices, startEdges) = input.foldLeft(Set.empty[String], Set.empty[(String, String)]) {
    case ((v, e), line) =>
      line match {
        case s"$name: $neighStr" =>
          val neighbors = neighStr.split(" ")
          (v + name ++ neighbors.toSet, e ++ neighbors.map(n => name -> n))
      }
  }

  startVertices.foreach(graph.addVertex)
  startEdges.foreach{
    case(v1, v2) => graph.addEdge(v1, v2)
  }

  val oneSize = new StoerWagnerMinimumCut(graph).minCut().size()
  val total = (startVertices.size - oneSize) * oneSize
  println(total)
}

object Day25 extends App {

  val input = scala.io.Source.fromResource("aoc23/test.txt").getLines()
  val graph = new SimpleGraph[String, DefaultEdge](classOf[DefaultEdge])

  val (startVertices, startEdges) = input.foldLeft(Set.empty[String], Set.empty[(String, String)]) {
    case ((v, e), line) =>
      line match {
        case s"$name: $neighStr" =>
          val neighbors = neighStr.split(" ")
          (v + name ++ neighbors.toSet, e ++ neighbors.map(n => name -> n))
      }
  }

  startVertices.foreach(graph.addVertex)
  startEdges.foreach{
    case(v1, v2) => graph.addEdge(v1, v2)
  }

  val oneSize = new StoerWagnerMinimumCut(graph).minCut().size()
  val total = (startVertices.size - oneSize) * oneSize
  println(total)
}

