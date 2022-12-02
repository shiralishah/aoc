package aoc21

import scala.annotation.tailrec

final case class Board(board: Array[Array[Int]]) {
  def unmarkedSum(called: Seq[Int]): Int = board.flatten.filterNot(called.contains).sum
  def hasWin(called: Seq[Int]): Boolean =
    board.exists(row => row.forall(called.contains)) | board.transpose.exists(col => col.forall(called.contains))
}

object Day04A extends App {
  val input = scala.io.Source.fromResource("aoc21/day04.txt").getLines().toSeq
  val calls = input.head.split(",").map(_.toInt).toSeq
  val boards = input.drop(2).foldLeft((Seq.empty[Board], Array.empty[Array[Int]])) {
    case ((boards, board), line) if line.isEmpty =>
      (boards :+ Board(board), Array.empty[Array[Int]])
    case ((boards, board), line) =>
      (boards, board :+ line.split(' ').filter(_.nonEmpty).map(_.toInt))
  } match {
    case (boards, board) if board.nonEmpty => boards :+ Board(board)
    case (boards, _)  => boards
  }

  val score = findWin(0)

  println(score)

  @tailrec
  def findWin(callNum: Int): Int = {
    val called = calls.take(callNum)
    boards.find(_.hasWin(called)) match {
      case Some(board) => board.unmarkedSum(called)*called.reverse.head
      case None => findWin(callNum+1)
    }
  }

}

object Day04B extends App {
  val input = scala.io.Source.fromResource("aoc21/day04.txt").getLines().toSeq
  val calls = input.head.split(",").map(_.toInt).toSeq
  val boards = input.drop(2).foldLeft((Seq.empty[Board], Array.empty[Array[Int]])) {
    case ((boards, board), line) if line.isEmpty =>
      (boards :+ Board(board), Array.empty[Array[Int]])
    case ((boards, board), line) =>
      (boards, board :+ line.split(' ').filter(_.nonEmpty).map(_.toInt))
  } match {
    case (boards, board) if board.nonEmpty => boards :+ Board(board)
    case (boards, _)  => boards
  }

  val score = findLastWin(0)

  println(score)

  @tailrec
  def findLastWin(callNum: Int): Int = {
    val called = calls.dropRight(callNum)
    boards.find(!_.hasWin(called.dropRight(1))) match {
      case Some(board) => (board.unmarkedSum(called))*called.reverse.head
      case None => findLastWin(callNum+1)
    }
  }

}
