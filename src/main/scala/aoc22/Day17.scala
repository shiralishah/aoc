package aoc22

trait Rock {

}
object Rock {
  case class First() extends Rock
  case class Second() extends Rock
  case class Third() extends Rock
  case class Fourth() extends Rock
  case class Fifth() extends Rock
}

final case class Chamber(state: Seq[Seq[Boolean]]) {

}
object Day17A extends App {
  val Width = 7
  val Bottom = 3
  val Left = 2
  val input = scala.io.Source.fromResource("aoc22/run.txt").mkString.toCharArray

  println(input.length)
}
