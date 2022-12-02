package aoc21

sealed trait Direction
object Direction {
  case object UP_RIGHT extends Direction
  case object UP_LEFT extends Direction
  case object DOWN_RIGHT extends Direction
  case object DOWN_LEFT extends Direction
}

final case class Point(x: Int, y: Int)

final case class Line(src: Point, dest: Point) {
  lazy val checkX = src.x == dest.x
  lazy val checkY = src.y == dest.y

  lazy val isRelevant =  checkX || checkY

  def expand: Seq[Point] = {
    val r = if (checkX) {
      ((src.y min dest.y) to (src.y max dest.y)).map(Point(src.x, _))
    } else if (checkY) {
      ((src.x min dest.x) to (src.x max dest.x)).map(Point(_, src.y))
    } else {
      val xMult = if (src.x < dest.x) 1 else -1
      val yMult = if (src.y < dest.y) 1 else -1
      (0 to ((src.x max dest.x) - (src.x min dest.x))).map(d => Point(src.x+d*xMult, src.y+d*yMult))
    }
    r
  }
}

object Day05A extends App {
  val input = scala.io.Source.fromResource("aoc21/day05.txt").getLines().toSeq
  val points = input.map(_ match {
    case s"$x1,$y1 -> $x2,$y2" => Line(Point(x1.toInt, y1.toInt), Point(x2.toInt, y2.toInt))
  }).filter(_.isRelevant).flatMap(_.expand)

  val count = points.groupBy(identity).count(_._2.size > 1)

  println(count)

}

object Day05B extends App {
  val input = scala.io.Source.fromResource("aoc21/day05.txt").getLines().toSeq
  val points = input.map(_ match {
    case s"$x1,$y1 -> $x2,$y2" => Line(Point(x1.toInt, y1.toInt), Point(x2.toInt, y2.toInt))
  }).flatMap(_.expand)

  val count = points.groupBy(identity).count(_._2.size > 1)

  println(count)


}
