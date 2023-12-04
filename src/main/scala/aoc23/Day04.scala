package aoc23

import zio.stream.ZSink
import zio.{ZIO, ZIOAppDefault}

object Day04A extends ZIOAppDefault {
  override def run = program
    .tapErrorCause(ZIO.logErrorCause(_))
//    .tap(x => ZIO.succeed(println(x)))
    .exitCode

  def program = ZIO.scoped {
    IOHelper.fileStreamLn("test.txt").map { line =>
      ""
    } >>> IOHelper.fileSinkLn("output.txt")
  }
}
