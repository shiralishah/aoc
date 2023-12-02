package aoc23

import zio.{ZIO, ZIOAppDefault}

object HelloWorld extends ZIOAppDefault {
  override def run = program
    .tapErrorCause(ZIO.logErrorCause(_)).exitCode

  def program = ZIO.scoped {
    FileHelper.fileStream("test.txt").run(FileHelper.fileSink("output.txt"))
  }

}
