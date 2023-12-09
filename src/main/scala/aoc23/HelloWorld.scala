package aoc23

import zio.{ZIO, ZIOAppDefault}

object HelloWorld extends ZIOAppDefault {
  override def run = program
    .tapErrorCause(ZIO.logErrorCause(_)).exitCode

  def program = ZIO.scoped {
    IOHelper.fileStream("test.txt") >>> (IOHelper.fileSink("output.txt"))
  }

}
