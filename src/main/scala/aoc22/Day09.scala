package aoc22

case class Point(x: Int, y: Int) {
  def diffX(other: Point): Int = this.x - other.x

  def diffY(other: Point): Int = this.y - other.y

}

object Day09A extends App {
  val input = scala.io.Source.fromResource("aoc22/day09.txt").getLines().toSeq.flatMap { line =>
    val splitLine = line.split(" ")
    (0 until splitLine(1).toInt).map(_ => splitLine(0))
  }

  val start = Point(0, 0)
  val (seen, _, _) = input.foldLeft(Set(start), start, start) { case ((visited, tail, head), direction) =>
    val newHead = direction match {
      case "R" => head.copy(x = head.x + 1)
      case "L" => head.copy(x = head.x - 1)
      case "U" => head.copy(y = head.y + 1)
      case "D" => head.copy(y = head.y - 1)
    }
    val diffX = head.diffX(tail)
    val diffY = head.diffY(tail)
    val newTail =
      if (diffX.abs > 1 || diffY.abs > 1) Point(tail.x + diffX.sign, tail.y + diffY.sign)
      else tail
    (visited + newTail, newTail, newHead)
  }

  println(seen.size)

}

object Day09B extends App {
  val input = scala.io.Source.fromResource("aoc22/day09.txt").getLines().toSeq.flatMap { line =>
    val splitLine = line.split(" ")
    (0 until splitLine(1).toInt).map(_ => splitLine(0))
  }

  val start                    = Point(0, 0)
  val startingRope: Seq[Point] = (0 to 9).map(_ => start)
  val (seen, _) = input.foldLeft(Set(start), startingRope) { case ((visited, rope), direction) =>
    val head = rope.head
    val newHead = direction match {
      case "R" => head.copy(x = head.x + 1)
      case "L" => head.copy(x = head.x - 1)
      case "U" => head.copy(y = head.y + 1)
      case "D" => head.copy(y = head.y - 1)
    }

    val (t, newRope) = rope.tail.foldLeft(newHead, Seq(newHead)) { case ((prev, r), knot) =>
      val diffX = prev.diffX(knot)
      val diffY = prev.diffY(knot)
      val next =
        if (diffX.abs > 1 || diffY.abs > 1) Point(knot.x + diffX.sign, knot.y + diffY.sign)
        else knot
      (next, r :+ next)
    }
    (visited + t, newRope)
  }

  println(seen.size)

}
