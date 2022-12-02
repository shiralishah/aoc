package aoc21

object Day02A extends App {
  val input = scala.io.Source.fromResource("aoc21/day02.txt").getLines().toSeq
  val (horizontal, depth) = input.foldLeft((0, 0)) {
    case((h, d), line) => line match {
      case s"forward $num" => (h + num.toInt, d)
      case s"up $num" => (h, d - num.toInt)
      case s"down $num" => (h, d + num.toInt)
    }
  }
  println(horizontal*depth)
}

object Day02B extends App {
  val input = scala.io.Source.fromResource("aoc21/day02.txt").getLines().toSeq
  val (horizontal, depth, aim) = input.foldLeft((0, 0, 0)) {
    case((h, d, aim), line) => line match {
      case s"forward $num" => (h + num.toInt, d + (aim*num.toInt), aim)
      case s"up $num" => (h, d, aim - num.toInt)
      case s"down $num" => (h, d, aim + num.toInt)
    }
  }
  println(horizontal*depth)
}