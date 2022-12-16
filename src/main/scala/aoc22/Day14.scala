package aoc22

case class SandLine(src: Point, targ: Point) {
  def getPoints(): Seq[Point] = {
    val (minY, maxY) = if (targ.y < src.y) (targ.y, src.y) else (src.y, targ.y)
    val (minX, maxX) = if (targ.x < src.x) (targ.x, src.x) else (src.x, targ.x)
    val horizontal = (minX to maxX).map(x => src.copy(x = x))
    val vertical = (minY to maxY).map(y => src.copy(y = y))
    (horizontal ++ vertical).distinct
  }
}

object Day14A extends App {
  def extractLines(line: Array[String]): SandLine = {
    val coord1 = line.head.split(",")
    val point1 = Point(coord1.head.toInt, coord1.last.toInt)
    val coord2 = line.last.split(",")
    val point2 = Point(coord2.head.toInt, coord2.last.toInt)
    SandLine(point1, point2)
  }

  val input = scala.io.Source.fromResource("aoc22/run.txt")
    .getLines().map(_.split(" -> ").sliding(2).toSeq).toSeq
  val lines = input.map(_.map(extractLines))
  val source = Point(500, 0)
  val linePoints = lines.flatMap(_.flatMap(l => l.getPoints())).distinct

  val minX = linePoints.minBy(_.x)
  val maxX = linePoints.maxBy(_.x)
  val maxY = linePoints.maxBy(_.y)

  val pointsEmpty = (minX.x to maxX.x).flatMap(x => (0 to maxY.y).map(y => Point(x, y) -> 0)).toMap
  val points = linePoints.foldLeft(pointsEmpty) {
    case (map, point) => map.updated(point, 1)
  }

  val q = points.get(Point(500, 9))

  val sandCount = run(points)
  println(sandCount)

  def run(sand: Map[Point, Int], count: Int = 0): Int = {
    findStatic(source, sand) match {
      case None => count
      case Some(p) =>
        run(sand.updated(p, 2), count + 1)
    }
  }

  def findStatic(point: Point, sand: Map[Point, Int]): Option[Point] =
    if (point.x < minX.x || point.x > maxX.x || point.y < 0 || point.y > maxY.y) None
    else {
      val down = point.copy(y = point.y + 1)
      val left = point.copy(x = point.x - 1, y = point.y + 1)
      val right = point.copy(x = point.x + 1, y = point.y + 1)
      if (sand.get(down).getOrElse(0) != 0 &&
        sand.get(left).getOrElse(0) != 0 &&
        sand.get(right).getOrElse(0) != 0) Some(point)
      else if (sand.get(down).getOrElse(0) != 0 &&
        sand.get(left).getOrElse(0) != 0) findStatic(right, sand)
      else if (sand.get(down).getOrElse(0) != 0) findStatic(left, sand)
      else findStatic(down, sand)
    }
}

object Day14B extends App {
  def extractLines(line: Array[String]): SandLine = {
    val coord1 = line.head.split(",")
    val point1 = Point(coord1.head.toInt, coord1.last.toInt)
    val coord2 = line.last.split(",")
    val point2 = Point(coord2.head.toInt, coord2.last.toInt)
    SandLine(point1, point2)
  }

  val input = scala.io.Source.fromResource("aoc22/run.txt")
    .getLines().map(_.split(" -> ").sliding(2).toSeq).toSeq
  val lines = input.map(_.map(extractLines))
  val source = Point(500, 0)
  val linePoints = lines.flatMap(_.flatMap(l => l.getPoints())).distinct

  val minX = linePoints.minBy(_.x)
  val maxX = linePoints.maxBy(_.x)
  val maxY = linePoints.maxBy(_.y)

  val pointsEmpty = (minX.x to maxX.x).flatMap(x => (0 to maxY.y).map(y => Point(x, y) -> 0)).toMap
  val points = linePoints.foldLeft(pointsEmpty) {
    case (map, point) => map.updated(point, 1)
  }

  val q = points.get(Point(500, 9))

  val sandCount = run(points)
  println(sandCount)

  def run(sand: Map[Point, Int], count: Int = 1): Int = {
    findStatic(source, sand) match {
      case None => count
      case Some(p) =>
        run(sand.updated(p, 2), count + 1)
    }
  }

  def findStatic(point: Point, sand: Map[Point, Int]): Option[Point] = {
    def getStatus(p: Point): Int = {
      if (p.y == maxY.y + 2) 1
      else sand.get(p).getOrElse(0)
    }

    val down = point.copy(y = point.y + 1)
    val left = point.copy(x = point.x - 1, y = point.y + 1)
    val right = point.copy(x = point.x + 1, y = point.y + 1)
    if (point == source &&
      getStatus(down) != 0 &&
      getStatus(left) != 0 &&
      getStatus(right) != 0)
      None
    else {
      if (getStatus(down) != 0 &&
        getStatus(left) != 0 &&
        getStatus(right) != 0) Some(point)
      else if (getStatus(down) != 0 &&
        getStatus(left) != 0) findStatic(right, sand)
      else if (getStatus(down) != 0) findStatic(left, sand)
      else findStatic(down, sand)
    }
  }
}

