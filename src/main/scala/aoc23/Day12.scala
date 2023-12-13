package aoc23

import zio.{ZIO, ZIOAppDefault}
import zio.stream.ZSink

object Day12A extends ZIOAppDefault {
  override def run = program
    .tapErrorCause(ZIO.logErrorCause(_))
    .tap(x => ZIO.succeed(println(x)))
    .exitCode

  def program = ZIO.scoped {
    IOHelper.fileStreamLn("run.txt").map { line =>
      val input = line.split(' ')
      val (springs, records) = (
        input(0).toCharArray,
        input(1).split(',').map(_.toInt)
      )
      getCount(springs, records)
    } >>> ZSink.sum[Long]
  }

  def getCount(springs: Array[Char], records: Seq[Int]): Long = {
    var cache = Map.empty[(Int, Int, Int, Boolean), Long]

    def parse(i: Int, j: Int, curr: Int, needsBuffer: Boolean): Long = {
      cache.get((i, j, curr, needsBuffer)) match {
        // found in cache
        case Some(value) => value
        case None =>

          val num =
          // found all the damaged springs already
          // so remaining springs need to operational
            if (j >= records.length && curr == 0) {
              springs.slice(i, springs.length)
                .find(_ == '#').map(_ => 0).getOrElse(1)

            } // there are no more springs to fulfill target damaged count
            else if (i >= springs.length) 0L
            // found all the damaged springs already
            // but we have counted additional damaged springs in curr
            else if (j >= records.length) 0L
            // we have found too many damaged springs for
            // the next target damaged count
            else if (curr > records(j)) 0L
            // next spring needs to be operational (to separate the damaged ones)
            // but the next spring is not operational
            else if (needsBuffer && springs(i) == '#') 0L
            // next spring is operational (separating the damaged ones)
            // continue looking to the next spring
            // without needing the operational buffer
            else if (needsBuffer && springs(i) != '#') {
              parse(i + 1, j, curr, needsBuffer = false)
            }
            else {
              // count the options if the current spring is damaged
              val damaged = (springs(i), curr) match {
                // found the last of this current damaged sequence move onto next spring
                // and next target damaged count, starting over (needing operational buffer)
                case ('#', n) if n + 1 == records(j) =>
                  parse(i + 1, j + 1, 0, needsBuffer = true)
                // found another damaged spring to add to the current count for this damaged sequence
                // continue onto the next spring
                case ('#', _) =>
                  parse(i + 1, j, curr + 1, needsBuffer = false)
                // found the last of this current damaged sequence move onto next spring
                // and next target damaged count, starting over (needing operational buffer)
                case ('?', n) if n + 1 == records(j) =>
                  parse(i + 1, j + 1, 0, needsBuffer = true)
                // found another damaged spring to add to the current count for this damaged sequence
                // continue onto the next spring
                case ('?', _) =>
                  parse(i + 1, j, curr + 1, needsBuffer = false)
                case _ => 0L
              }
              // count the options if the current spring is operational
              val operational = (springs(i), curr) match {
                // just an operational spring with no damaged count accumulated in sequence
                // move onto the next spring
                case ('.', 0) =>
                  parse(i + 1, j, 0, needsBuffer = false)
                // also an operational spring with no damaged count accumulated in sequence
                // move onto the next spring
                case ('?', 0) =>
                  parse(i + 1, j, 0, needsBuffer = false)
                // reached an operational spring when we didnt need an operational spring
                // to buffer between sequences of damaged springs
                case _ => 0L
              }
              // total options, summing damaged and operational cases
              damaged + operational
            }
          // add calculated value to cache before returning
          cache = cache + ((i, j, curr, needsBuffer) -> num)
          num
      }
    }

    parse(0, 0, 0, needsBuffer = false)
  }

}

object Day12B extends ZIOAppDefault {
  override def run = program
    .tapErrorCause(ZIO.logErrorCause(_))
    .tap(x => ZIO.succeed(println(x)))
    .exitCode

  def program = ZIO.scoped {
    IOHelper.fileStreamLn("run.txt").map { line =>
      val input = line.split(' ')
      val (springs, records) = unfold(input(0), input(1))
      getCount(springs, records)
    } >>> ZSink.sum[Long]
  }

  def unfold(springs: String, records: String): (Array[Char], Seq[Int]) = {
    (
      Array.fill(5)(springs).mkString("?").toCharArray,
      Seq.fill(5)(records).mkString(",").split(",").map(_.toInt)
    )
  }

  def getCount(springs: Array[Char], records: Seq[Int]): Long = {
    var cache = Map.empty[(Int, Int, Int, Boolean), Long]

    def parse(i: Int, j: Int, curr: Int, needsBuffer: Boolean): Long = {
      cache.get((i, j, curr, needsBuffer)) match {
        // found in cache
        case Some(value) => value
        case None =>

          val num =
          // found all the damaged springs already
          // so remaining springs need to operational
            if (j >= records.length && curr == 0) {
              springs.slice(i, springs.length)
                .find(_ == '#').map(_ => 0).getOrElse(1)

            } // there are no more springs to fulfill target damaged count
            else if (i >= springs.length) 0L
            // found all the damaged springs already
            // but we have counted additional damaged springs in curr
            else if (j >= records.length) 0L
            // we have found too many damaged springs for
            // the next target damaged count
            else if (curr > records(j)) 0L
            // next spring needs to be operational (to separate the damaged ones)
            // but the next spring is not operational
            else if (needsBuffer && springs(i) == '#') 0L
            // next spring is operational (separating the damaged ones)
            // continue looking to the next spring
            // without needing the operational buffer
            else if (needsBuffer && springs(i) != '#') {
              parse(i + 1, j, curr, needsBuffer = false)
            }
            else {
              // count the options if the current spring is damaged
              val damaged = (springs(i), curr) match {
                // found the last of this current damaged sequence move onto next spring
                // and next target damaged count, starting over (needing operational buffer)
                case ('#', n) if n + 1 == records(j) =>
                  parse(i + 1, j + 1, 0, needsBuffer = true)
                // found another damaged spring to add to the current count for this damaged sequence
                // continue onto the next spring
                case ('#', _) =>
                  parse(i + 1, j, curr + 1, needsBuffer = false)
                // found the last of this current damaged sequence move onto next spring
                // and next target damaged count, starting over (needing operational buffer)
                case ('?', n) if n + 1 == records(j) =>
                  parse(i + 1, j + 1, 0, needsBuffer = true)
                // found another damaged spring to add to the current count for this damaged sequence
                // continue onto the next spring
                case ('?', _) =>
                  parse(i + 1, j, curr + 1, needsBuffer = false)
                case _ => 0L
              }
              // count the options if the current spring is operational
              val operational = (springs(i), curr) match {
                // just an operational spring with no damaged count accumulated in sequence
                // move onto the next spring
                case ('.', 0) =>
                  parse(i + 1, j, 0, needsBuffer = false)
                // also an operational spring with no damaged count accumulated in sequence
                // move onto the next spring
                case ('?', 0) =>
                  parse(i + 1, j, 0, needsBuffer = false)
                // reached an operational spring when we didnt need an operational spring
                // to buffer between sequences of damaged springs
                case _ => 0L
              }
              // total options, summing damaged and operational cases
              damaged + operational
            }
          // add calculated value to cache before returning
          cache = cache + ((i, j, curr, needsBuffer) -> num)
          num
      }
    }

    parse(0, 0, 0, needsBuffer = false)
  }

}
