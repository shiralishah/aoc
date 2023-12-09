package aoc23

import zio.{ZIO, ZIOAppDefault}
import zio.stream.ZSink

object Day09A extends ZIOAppDefault {
  override def run = program
    .tapErrorCause(ZIO.logErrorCause(_))
    .tap(x => ZIO.succeed(println(x)))
    .exitCode

  def program = ZIO.scoped {
    IOHelper.fileStreamLn("run.txt").map{ line =>
      val nums = line.split(' ').map(_.toLong)
      diffTree(nums.toSeq)
    } >>> ZSink.sum[Long]
  }

  def diffTree(seq: Seq[Long]): Long = {
    if (seq.forall(_ == seq.head)) {
      seq.head
    } else {
      val diffs = seq.sliding(2).map(x => x.last - x.head).toSeq
      seq.last + diffTree(diffs)
    }
  }
}

object Day09B extends ZIOAppDefault {
  override def run = program
    .tapErrorCause(ZIO.logErrorCause(_))
    .tap(x => ZIO.succeed(println(x)))
    .exitCode

  def program = ZIO.scoped {
    IOHelper.fileStreamLn("run.txt").map{ line =>
      val nums = line.split(' ').map(_.toLong)
      diffTree(nums.toSeq.reverse)
    } >>> ZSink.sum[Long]
  }

  def diffTree(seq: Seq[Long]): Long = {
    if (seq.forall(_ == seq.head)) {
      seq.head
    } else {
      val diffs = seq.sliding(2).map(x => x.last - x.head).toSeq
      seq.last + diffTree(diffs)
    }
  }
}