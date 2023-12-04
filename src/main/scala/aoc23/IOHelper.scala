package aoc23

import zio.Chunk
import zio.stream.{ZPipeline, ZSink, ZStream}

import java.nio.file.Paths
import java.nio.file.StandardOpenOption.{CREATE, WRITE}

object IOHelper {

  val path = (fileName: String) => s"src/main/resources/aoc23/$fileName"

  def fileStreamLn(fileName: String) =
    ZStream.fromPath(Paths.get(path(fileName)))
      .via(ZPipeline.utfDecode >>> ZPipeline.splitLines)

  def fileSinkLn(fileName: String) =
    ZSink.fromPath(Paths.get(path(fileName)))
      .contramapChunks[String](_.flatMap(_.getBytes)).contramap[String](x => s"$x\n")

  def fileStream(fileName: String) =
    ZStream.fromPath(Paths.get(path(fileName)))
      .via(ZPipeline.utfDecode)

  def fileSink(fileName: String) =
    ZSink.fromPath(Paths.get(path(fileName)))
      .contramapChunks[String](_.flatMap(_.getBytes))
}
