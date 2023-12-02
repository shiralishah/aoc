package aoc23

import zio.stream.ZSink
import zio.{ZIO, ZIOAppDefault}

import scala.annotation.tailrec

object Day01A extends ZIOAppDefault {
  override def run = program
    .tapErrorCause(ZIO.logErrorCause(_))
    .tap(x => ZIO.succeed(println(x)))
    .exitCode

  def program = ZIO.scoped {
    FileHelper.fileStreamLn("run.txt").map{ line =>
      val digits = line.filter(_.isDigit)
      s"${digits.head}${digits.last}".toLong
    } >>> ZSink.sum[Long]
  }
}

object Day01B extends ZIOAppDefault {
  val DigitMap = Map(
    "one" -> 1,
    "two" -> 2,
    "three" -> 3,
    "four" -> 4,
    "five" -> 5,
    "six" -> 6,
    "seven" -> 7,
    "eight" -> 8,
    "nine" -> 9,
  )
  override def run = program
    .tapErrorCause(ZIO.logErrorCause(_))
    .tap(x => ZIO.succeed(println(x)))
    .exitCode

  def program = ZIO.scoped {
    FileHelper.fileStreamLn("run.txt").map { line =>
      @tailrec
      def search(ind: Int, f: (String, Int) => (String, Char)): String = {
        val (acc, c) = f(line, ind)
        val word = DigitMap.collectFirst {
          case (k, v) if acc.contains(k) => v
        }
        word match {
          case Some(v) => v.toString
          case None if c.isDigit => c.toString
          case None => search(ind + 1, f)
        }
      }
      val first = search(1, front)
      val last = search(1, back)
      s"$first$last".toLong
    } >>> ZSink.sum[Long]
  }

    def front(str: String, ind: Int) = (str.take(ind), str.charAt(ind-1))
    def back(str: String, ind: Int) = (str.takeRight(ind), str.charAt(str.length-ind))
}