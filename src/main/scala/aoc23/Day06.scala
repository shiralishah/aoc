package aoc23

object Day06A extends App {
  val input = scala.io.Source.fromResource("aoc23/run.txt").getLines().toSeq
  val times = input.head.split("\\s+").tail.map(_.toInt)
  val distances = input.last.split("\\s+").tail.map(_.toInt)

  val solutions = times.zip(distances).foldLeft(Seq.empty[Int]) {
    case (solns, (time, distance)) =>
      val lower = binarySearch(0, time, time, distance)
      lower match {
        case None => solns
        case Some(v) => solns :+ (time - 2*v +1)
      }
  }

  val product = solutions.product
  println(product)

  def getDistance(hold: Int, time: Int): Int = {
    val move = time - hold
    move*hold
  }

  def binarySearch(left: Int, right: Int, time: Int, distance: Int): Option[Int] = {
    if (left > right) {
      None
    } else {
      val mid = left + (right - left) / 2

      if (getDistance(mid, time) > distance) {
          binarySearch(left, mid - 1, time, distance).orElse(Some(mid))
      } else {
          binarySearch(mid + 1, right, time, distance)
      }
    }
  }
}

object Day06B extends App {
  val input = scala.io.Source.fromResource("aoc23/run.txt").getLines().toSeq
  val time = input.head.split("\\s+").tail.mkString.toLong
  val distance = input.last.split("\\s+").tail.mkString.toLong
  val solution =
    binarySearch(0, time, time, distance).map{
      lower =>
        (time - 2*lower +1)
    }.getOrElse(0)
  println(solution)

  def getDistance(hold: Long, time: Long): Long = {
    val move = time - hold
    move*hold
  }

  def binarySearch(left: Long, right: Long, time: Long, distance: Long): Option[Long] = {
    if (left > right) {
      None
    } else {
      val mid = left + (right - left) / 2

      if (getDistance(mid, time) > distance) {
        binarySearch(left, mid - 1, time, distance).orElse(Some(mid))
      } else {
        binarySearch(mid + 1, right, time, distance)
      }
    }
  }
}



