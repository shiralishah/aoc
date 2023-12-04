package aoc23

import zio.{Chunk, ZIO, ZIOAppDefault}
import zio.stream.ZSink

object Day02A extends ZIOAppDefault {
  override def run = program
    .tapErrorCause(ZIO.logErrorCause(_))
    .tap(x => ZIO.succeed(println(x)))
    .exitCode

  def program = ZIO.scoped {
    IOHelper.fileStreamLn("run.txt").map {
      case s"Game $n: $games" =>
        val sets = games.split(";").map(_.strip)
        Option.when(sets.forall(validSet))(n.toInt)
      case _ => None
    }.collectSome >>> ZSink.sum[Int]
  }

  def validSet(set: String) = {
    val cubes = set.split(",").map(_.strip)
    val (red, green, blue) = cubes.foldLeft(0, 0 , 0) {
      case((r, g, b), str) =>
        str match {
          case s"$x red" => (r + x.toInt, g, b)
          case s"$x green" => (r, g + x.toInt, b)
          case s"$x blue" => (r, g, b + x.toInt)
        }
    }
    red <= 12 && green <= 13 && blue <= 14
  }
}

object Day02B extends ZIOAppDefault {
  override def run = program
    .tapErrorCause(ZIO.logErrorCause(_))
    .tap(x => ZIO.succeed(println(x)))
    .exitCode

  def program = ZIO.scoped {
    IOHelper.fileStreamLn("run.txt").map {
      case s"Game $n: $games" =>
        getPower(games)
    } >>> ZSink.sum[Long]
  }

  def getPower(games: String) = {
    val sets = games.split(";").map(_.strip)
    val countByColor = sets.map(countSet).unzip3
    countByColor._1.max.toLong *
      countByColor._2.max.toLong *
      countByColor._3.max.toLong
  }

  def countSet(set: String) = {
    val cubes = set.split(",").map(_.strip)
    cubes.foldLeft(0, 0 , 0) {
      case((r, g, b), str) =>
        str match {
          case s"$x red" => (r + x.toInt, g, b)
          case s"$x green" => (r, g + x.toInt, b)
          case s"$x blue" => (r, g, b + x.toInt)
        }
    }
  }
}