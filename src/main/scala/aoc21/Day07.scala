package aoc21

object Day07A extends App {
  val input = scala.io.Source.fromResource("aoc21/day07.txt").getLines().toSeq.head.split(",").map(_.toInt).sorted
  val median = input(input.size/2)
  val cost = input.map(num => (num-median).abs).sum
  println(cost)
}

object Day07B extends App {
  val input = scala.io.Source.fromResource("aoc21/day07.txt").getLines().toSeq.head.split(",").map(_.toInt)
  val fuel = (input.min to input.max).map(point => input.map(place => cost(point, place)).sum).min
  println(fuel)

  def cost(point: Int, loc: Int) =
    (1 to (loc-point).abs).foldLeft(0) {
      case (c, fuel) => c+fuel
    }

}