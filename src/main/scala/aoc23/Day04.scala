package aoc23

import zio.stream.ZSink
import zio.{ZIO, ZIOAppDefault}

object Day04A extends ZIOAppDefault {
  override def run = program
    .tapErrorCause(ZIO.logErrorCause(_))
    .tap(x => ZIO.succeed(println(x)))
    .exitCode

  def program = ZIO.scoped {
    IOHelper.fileStreamLn("run.txt").map {
      case s"Card $_: $a|$b" =>
        val winningNums = a.split(' ').filterNot(_.isEmpty)
        val myNums = b.split(' ').filterNot(_.isEmpty)
        val myWinningNums = myNums.filter(winningNums.contains)
        val exp = myWinningNums.length-1
        if (exp < 0 ) 0 else Math.pow(2, exp).toInt
    } >>> ZSink.sum[Int]
  }
}

object Day04B extends App {
  val input = scala.io.Source.fromResource("aoc23/run.txt").getLines().toSeq
  val allCards = input.foldLeft(Seq.empty[Int]){
    case (cards, card) =>
      card match {
        case s"Card $c: $a|$b" =>
          val n = c.strip.toInt
          val updatedCards = cards :+ n
          val winningNums = a.split(' ').filterNot(_.isEmpty)
          val myNums = b.split(' ').filterNot(_.isEmpty)
          val numOfWins = myNums.count(winningNums.contains)
          val newCards = (n+1 to n+numOfWins)
          val numOfCards = updatedCards.count(_ == n)
          updatedCards :++ (Seq.fill(numOfCards)(newCards).flatten)
      }
  }
  val totalNum = allCards.size
  println(totalNum)
}
