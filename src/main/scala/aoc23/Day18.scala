package aoc23


object Day18A extends App {

  final case class Point(x: Int, y: Int)

  val input = scala.io.Source.fromResource("aoc23/run.txt").getLines().toSeq
  val (points, _) = input.foldLeft(Seq.empty[Point], Point(0, 0)) {
    case ((accPoints, prev), line) =>
      line match {
        case s"$d $n (#$_)" =>
          val point = d match {
            case "R" => prev.copy(x = prev.x + n.toInt)
            case "L" => prev.copy(x = prev.x - n.toInt)
            case "U" => prev.copy(y = prev.y + n.toInt)
            case "D" => prev.copy(y = prev.y - n.toInt)
          }
          (accPoints :+ point, point)
      }
  }

  val (area, border) = points.zipWithIndex.foldLeft(0, 0) {
    case ((sum, border), (c, i)) =>
      val l = if (i - 1 < 0) points.length - 1 else i - 1
      val r = if (i + 1 >= points.length) 0 else i + 1
      val left = points(l)
      val right = points(r)
      val yDiff = right.y - left.y
      val yCount = Math.abs(left.y - c.y)
      val xCount = Math.abs(left.x - c.x)
      (sum + (c.x * yDiff), border + yCount + xCount)
  }

  val totalArea = Math.abs(area / 2) + 1 + (border / 2)

  println(totalArea)


}

object Day18B extends App {

  final case class Point(x: Long, y: Long)

  val input = scala.io.Source.fromResource("aoc23/run.txt").getLines().toSeq
  val (points, _) = input.foldLeft(Seq.empty[Point], Point(0L, 0L)) {
    case ((accPoints, prev), line) =>
      line match {
        case s"$_ $_ (#$label)" =>
          val (hex, d) = label.splitAt(label.length-1)
          val n = java.lang.Long.parseLong(hex, 16)
          val point = d match {
            case "0" => prev.copy(x = prev.x + n.toInt)
            case "1" => prev.copy(y = prev.y - n.toInt)
            case "2" => prev.copy(x = prev.x - n.toInt)
            case "3" => prev.copy(y = prev.y + n.toInt)
          }
          (accPoints :+ point, point)
      }
  }

  val (area, border) = points.zipWithIndex.foldLeft(0L, 0L) {
    case ((sum, border), (c, i)) =>
      val l = if (i - 1 < 0) points.length - 1 else i - 1
      val r = if (i + 1 >= points.length) 0 else i + 1
      val left = points(l)
      val right = points(r)
      val yDiff = right.y - left.y
      val yCount = Math.abs(left.y - c.y)
      val xCount = Math.abs(left.x - c.x)
      (sum + (c.x * yDiff), border + yCount + xCount)
  }

  val totalArea: Long = Math.abs(area / 2) + 1 + (border / 2)

  println(totalArea)


}