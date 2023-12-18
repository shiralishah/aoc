package aoc23

object Day17A extends App {
  val input: Array[Array[Char]] = scala.io.Source.fromResource("aoc23/run.txt")
    .getLines().toArray.map(_.toCharArray)
  val rowCount = input.length
  val colCount = input.head.length

  final case class Vertex(x: Int, y: Int) {

    def get: Int = input(this.y)(this.x) - '0'

    def up(i: Int): Option[Vertex] = Option.when(this.y - i >= 0)(this.copy(y = this.y - i))

    def down(i: Int): Option[Vertex] = Option.when(this.y + i < rowCount)(this.copy(y = this.y + i))

    def left(i: Int): Option[Vertex] = Option.when(this.x - i >= 0)(this.copy(x = this.x - i))

    def right(i: Int): Option[Vertex] = Option.when(this.x + i < colCount)(this.copy(x = this.x + i))
  }

  val start = Vertex(0, 0)
  val target = Vertex(colCount - 1, rowCount - 1)

  // cost, vertex, direction
  var queue: Seq[(Int, Vertex, Int)] = Seq((0, start, -1))
  var visited = Set.empty[(Vertex, Int)]
  var costs: Map[(Vertex, Int), Int] = Map.empty

  // dijkstra
  while (queue.nonEmpty) {
    val sorted = queue.sortBy(_._1)
    val (cost, vert, dir) = sorted.head
    queue = sorted.tail
    if (vert == target) {
      costs.find(x => x._1._1 == target) match {
        case Some(prev) => costs = costs + ((target, -1) -> Math.min(prev._2, cost))
        case None => costs = costs + ((target, -1) -> cost)
      }
    }
    if (!visited.contains((vert, dir))) {
      visited = visited + ((vert, dir))
      for (d <- 0 until 4) {
        if (d != dir && (d + 2) % 4 != dir) {
          (1 until 4).foldLeft(0) {
            case (incr, dist) =>
              (d match {
                case 0 => vert.down(dist) // down
                case 1 => vert.right(dist) // right
                case 2 => vert.up(dist) // up
                case 3 => vert.left(dist) // left
              }).map {
                v =>
                  val costIncr = incr + v.get
                  val updatedCost = costIncr + cost
                  if (dist < 1) {
                    costIncr
                  } else if (costs.getOrElse((v, d), Integer.MAX_VALUE) <= updatedCost) {
                    costIncr
                  } else {
                    costs += ((v, d) -> updatedCost)
                    queue = queue :+ ((updatedCost, v, d))
                    costIncr
                  }
              }.getOrElse(incr)

          }
        }
      }
    }
  }


  val targetCost = costs((target, -1)) - start.get

  println(targetCost)

}

object Day17B extends App {
  val input: Array[Array[Char]] = scala.io.Source.fromResource("aoc23/run.txt")
    .getLines().toArray.map(_.toCharArray)
  val rowCount = input.length
  val colCount = input.head.length

  final case class Vertex(x: Int, y: Int) {

    def get: Int = input(this.y)(this.x) - '0'

    def up(i: Int): Option[Vertex] = Option.when(this.y - i >= 0)(this.copy(y = this.y - i))

    def down(i: Int): Option[Vertex] = Option.when(this.y + i < rowCount)(this.copy(y = this.y + i))

    def left(i: Int): Option[Vertex] = Option.when(this.x - i >= 0)(this.copy(x = this.x - i))

    def right(i: Int): Option[Vertex] = Option.when(this.x + i < colCount)(this.copy(x = this.x + i))
  }

  val start = Vertex(0, 0)
  val target = Vertex(colCount - 1, rowCount - 1)

  // cost, vertex, direction
  var queue: Seq[(Int, Vertex, Int)] = Seq((0, start, -1))
  var visited = Set.empty[(Vertex, Int)]
  var costs: Map[(Vertex, Int), Int] = Map.empty

  // dijkstra
  while (queue.nonEmpty) {
    val sorted = queue.sortBy(_._1)
    val (cost, vert, dir) = sorted.head
    queue = sorted.tail
    if (vert == target) {
      costs.find(x => x._1._1 == target) match {
        case Some(prev) => costs = costs + ((target, -1) -> Math.min(prev._2, cost))
        case None => costs = costs + ((target, -1) -> cost)
      }
    }
    if (!visited.contains((vert, dir))) {
      visited = visited + ((vert, dir))
      for (d <- 0 until 4) {
        if (d != dir && (d + 2) % 4 != dir) {
          (1 until 11).foldLeft(0) {
            case (incr, dist) =>
              (d match {
                case 0 => vert.down(dist) // down
                case 1 => vert.right(dist) // right
                case 2 => vert.up(dist) // up
                case 3 => vert.left(dist) // left
              }).map {
                v =>
                  val costIncr = incr + v.get
                  val updatedCost = costIncr + cost
                  if (dist < 4) {
                    costIncr
                  } else if (costs.getOrElse((v, d), Integer.MAX_VALUE) <= updatedCost) {
                    costIncr
                  } else {
                    costs += ((v, d) -> updatedCost)
                    queue = queue :+ ((updatedCost, v, d))
                    costIncr
                  }
              }.getOrElse(incr)

          }
        }
      }
    }
  }

  val targetCost = costs((target, -1))

  println(targetCost)
}

// 1315 is too high