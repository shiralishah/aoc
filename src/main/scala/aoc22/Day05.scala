package aoc22

object Day05A extends App {
  val input                    = scala.io.Source.fromResource("aoc22/day05.txt").mkString
  val parts                    = input.split("\n\n")
  val (starting, instructions) = (parts.head.split("\n"), parts.tail.head.split("\n"))
  // first half input processing to get initial state of crates
  val labels = starting.last.trim.split("\\s+")
  val len    = starting.map(_.length).max
  val startFilled = starting
    .dropRight(1)
    .map(str => (str + " ".repeat(len - str.length)).replaceAll("    ", "[-] ").replaceAll("\\s", ""))
  // this is our final structure providing starting state with rows as stack number
  val arr = startFilled.map { line =>
    val grid = line.replaceAll("]", "").replaceFirst("\\[", "").split("\\[")
    val add  = (0 until (labels.length - grid.length)).map(_ => "-")
    grid ++ add
  }.transpose.map(_.filterNot(_ == "-"))

  // apply instructions from second half of input
  val finalState = instructions.foldLeft(arr) { case (state, inst) =>
    inst match {
      case s"move $amount from $src to $dest" =>
        val (remove, remain) = state(src.toInt - 1).splitAt(amount.toInt)
        val destUpdate       = remove.reverse ++: state(dest.toInt - 1)
        val x                = state.updated(src.toInt - 1, remain).updated(dest.toInt - 1, destUpdate)
        x
    }
  }

  val topCrates = finalState.map(_.head).reduce(_ + _)
  println(topCrates)
}

object Day05B extends App {
  val input                    = scala.io.Source.fromResource("aoc22/day05.txt").mkString
  val parts                    = input.split("\n\n")
  val (starting, instructions) = (parts.head.split("\n"), parts.tail.head.split("\n"))
  // first half input processing to get initial state of crates
  val labels = starting.last.trim.split("\\s+")
  val len    = starting.map(_.length).max
  val startFilled = starting
    .dropRight(1)
    .map(str => (str + " ".repeat(len - str.length)).replaceAll("    ", "[-] ").replaceAll("\\s", ""))
  // this is our final structure providing starting state with rows as stack number
  val arr = startFilled.map { line =>
    val grid = line.replaceAll("]", "").replaceFirst("\\[", "").split("\\[")
    val add  = (0 until (labels.length - grid.length)).map(_ => "-")
    grid ++ add
  }.transpose.map(_.filterNot(_ == "-"))

  // apply instructions from second half of input
  val finalState = instructions.foldLeft(arr) { case (state, inst) =>
    inst match {
      case s"move $amount from $src to $dest" =>
        val (remove, remain) = state(src.toInt - 1).splitAt(amount.toInt)
        val destUpdate       = remove ++: state(dest.toInt - 1)
        val x                = state.updated(src.toInt - 1, remain).updated(dest.toInt - 1, destUpdate)
        x
    }
  }

  val topCrates = finalState.map(_.head).reduce(_ + _)
  println(topCrates)
}
