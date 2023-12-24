package aoc23


import scala.annotation.tailrec

object Day23A extends App {
  val input = scala.io.Source.fromResource("aoc23/run.txt").getLines().toArray.map(_.toCharArray)
  val rowNum = input.length
  val colNum = input.head.length

  val start = input.head.zipWithIndex.collectFirst {
    case (ch, x) if ch != '#' => (x, 0)
  }

  val length = walk(start.get, Set.empty, 0)

  println(length)


  def walk(point: (Int, Int), visited: Set[(Int, Int)], count: Long): Long = {
    if (point._2 == rowNum-1) count
    else {
      var curr = point
      var seen = visited + point
      var neighbors = getNeighbors(curr).filterNot(seen.contains)
      var add = 0
      while(neighbors.size == 1) {
        curr = neighbors.head
        neighbors = getNeighbors(curr).filterNot(seen.contains)
        add = add+1
        seen = seen + curr
      }
      if (neighbors.isEmpty) count + add
      else {
        neighbors.map(n => walk(n, seen, count+add+1)).max
      }
    }
  }

  def getNeighbors(loc: (Int, Int)): Seq[(Int, Int)] = {
    val left = ((loc._1-1, loc._2), '<')
    val right = ((loc._1+1, loc._2), '>')
    val up = ((loc._1, loc._2-1), '^')
    val down = ((loc._1, loc._2+1), 'v')
    val inBounds = Seq(left, right, up , down).filter{
      case (n, _) => n._1 >= 0 && n._1 < colNum && n._2 >= 0 && n._2 < rowNum
    }
    inBounds.collect {
      case (n, d) if input(n._2)(n._1) == '.' || input(n._2)(n._1) == d => n
    }
  }
}

object Day23B extends App {
  val input = scala.io.Source.fromResource("aoc23/run.txt").getLines().toArray.map(_.toCharArray)
  val rowNum = input.length
  val colNum = input.head.length

  val start = (input.head.indexOf('.'), 0)
  val end = (input.last.indexOf('.'), rowNum - 1)

  val nodes = Seq(start, end) ++:(for {
    (row, y) <- input.zipWithIndex
    (c, x) <- row.zipWithIndex
    if c != '#' && getNeighbors((x, y)).length > 2
  } yield (x, y)).toSeq

  val graph = buildGraph(nodes)

  var queue = Seq((start, 0))
  var visited = Set.empty[(Int, Int)]
  var max = 0
  while (queue.nonEmpty) {
    val (node, dist) = queue.head
    queue = queue.tail
    if (dist < 0) {
      visited = visited - node
    }
    else if (node == end) {
      max = Math.max(max, dist)
    } else if (!visited.contains(node)){
      visited = visited + node
      val next = graph(node).map {
        case (n, w) => (n, dist + w)
      }.toSeq :+ (node, -1)
      queue = next :++ queue
    }
  }
  println(max)

  def buildGraph(nodes: Seq[(Int, Int)]): Map[(Int, Int), Map[(Int, Int), Int]] = {
    nodes.foldLeft(Map(end -> Map.empty[(Int, Int), Int])) {
      case (g, node) =>
        val edges = buildEdges(node, Seq((node, 0)), Set(node), Map.empty)
        g + (node -> edges)

    }
  }

  @tailrec
  def buildEdges(src: (Int, Int),
                 queue: Seq[((Int, Int), Int)],
                 visited: Set[(Int, Int)],
                 paths: Map[(Int, Int), Int]
                ): Map[(Int, Int), Int] = {
    if (queue.isEmpty) paths
    else {
      val (node, dist) = queue.head
      if (dist > 0 && nodes.contains(node)) {
        val updatedDistance = paths + (node -> dist)
        buildEdges(src, queue.tail, visited, updatedDistance)
      }
      else {
        val neighbors = getNeighbors(node).filterNot(visited.contains)
        val updatedQueue = neighbors.map(n => n -> (dist + 1)) :++ queue.tail
        val updatedVisited = visited ++ neighbors.toSet
        buildEdges(src, updatedQueue, updatedVisited, paths)
      }
    }

  }

  def getNeighbors(loc: (Int, Int)): Seq[(Int, Int)] = {
    val left = (loc._1 - 1, loc._2)
    val right = (loc._1 + 1, loc._2)
    val up = (loc._1, loc._2 - 1)
    val down = (loc._1, loc._2 + 1)
    val inBounds = Seq(left, up, down, right).filter {
      n => n._1 >= 0 && n._1 < colNum && n._2 >= 0 && n._2 < rowNum
    }
    inBounds.collect {
      case n if input(n._2)(n._1) != '#' => n
    }
  }
}
