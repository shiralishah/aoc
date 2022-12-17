package aoc22

case class BeaconPoint(x: Int, y: Int)
case class SensorPoint(x: Int, y: Int, beacon: BeaconPoint) {
  def distance: Int = (this.x - beacon.x).abs + (this.y - beacon.y).abs
  def range(height: Int): Set[Point] = {
    val distance = this.distance
    if (this.y == height) {
      (this.getPointsX(distance) :+ Point(this.x, this.y)).toSet
    } else if (
      (this.y < height && height <= this.y + distance) ||
      (this.y > height && height > this.y - distance)
    ) {
      val diff = distance - (height - this.y).abs
      (this.getPointsX(diff, height) :+ Point(this.x, height)).toSet
    } else Set.empty // not in range
  }

  def inRange(xVal: Int, yVal: Int): Boolean =
    (this.x - xVal).abs + (this.y - yVal).abs <= this.distance

  def borderPoints: Set[Point] =
    (0 to (this.distance + 1))
      .foldLeft(this.distance + 1, Set.empty[Point]) { case ((xDelta, points), yDelta) =>
        val added = Set(
          Point(this.x - xDelta, this.y + yDelta),
          Point(this.x + xDelta, this.y + yDelta),
          Point(this.x - xDelta, this.y - yDelta),
          Point(this.x + xDelta, this.y - yDelta)
        )
        (xDelta - 1, points ++ added)
      }
      ._2

  private def getPointsX(xVal: Int, yVal: Int = this.y): Seq[Point] =
    (1 to xVal).flatMap(n => Set(Point(this.x - n, yVal), Point(this.x + n, yVal)))

}

object Day15A extends App {
  val input = scala.io.Source.fromResource("aoc22/day15.txt").getLines().toSeq

  val sensors = input.map {
    _ match {
      case s"Sensor at x=$sensorX, y=$sensorY: closest beacon is at x=$beaconX, y=$beaconY" =>
        SensorPoint(sensorX.toInt, sensorY.toInt, BeaconPoint(beaconX.toInt, beaconY.toInt))
    }
  }

  val beacons     = sensors.map(_.beacon).toSet
  val ranges      = sensors.flatMap(_.range(2000000)).toSet
  val rangeCount  = ranges.size
  val beaconCount = beacons.filter(_.y == 2000000).size
  val notCount    = rangeCount - beaconCount
  println(notCount)
}

object Day15B extends App {
  val input = scala.io.Source.fromResource("aoc22/day15.txt").getLines().toSeq

  val sensors = input.map {
    _ match {
      case s"Sensor at x=$sensorX, y=$sensorY: closest beacon is at x=$beaconX, y=$beaconY" =>
        SensorPoint(sensorX.toInt, sensorY.toInt, BeaconPoint(beaconX.toInt, beaconY.toInt))
    }
  }

  val viableSet = sensors.foldLeft(Set.empty[Point]) { case (points, sensor) =>
    val border   = sensor.borderPoints.filter(inLimits)
    val filtered = border.filter(p => sensors.forall(s => !s.inRange(p.x, p.y)))
    points ++ filtered
  }

  val viable = viableSet.head

  def inLimits(point: Point): Boolean =
    point.x >= 0 && point.x <= 4000000 && point.y >= 0 && point.y <= 4000000

  println(s"point: (${viable.x}, ${viable.y})")

}
