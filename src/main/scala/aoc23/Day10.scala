package aoc23

import scala.annotation.tailrec

final case class Pipe(x: Int, y: Int)

object Pipe {
  val Lefts = Set('-', 'L', 'F')
  val Rights = Set('-', '7', 'J')
  val Ups = Set('|', '7', 'F')
  val Downs = Set('|', 'L', 'J')
}

object Day10A extends App {

  val input: Array[Array[Char]] = scala.io.Source.fromResource("aoc23/run.txt").getLines().toArray.map(_.toCharArray)

  val start = input.zipWithIndex.collectFirst {
    case (row, y) if row.contains('S') =>
      val x = row.zipWithIndex.find(_._1 == 'S').get._2
      Pipe(x, y)
  }.get

  // directions as numbers:
  // left -> 0
  // up -> 1
  // right -> 2
  // down -> 3

  val (next, dir) = buildPipe(start, Pipe.Lefts, (a, b) => (a - 1, b), 0)
    .orElse(buildPipe(start, Pipe.Ups, (a, b) => (a, b - 1), 1))
    .orElse(buildPipe(start, Pipe.Rights, (a, b) => (a + 1, b), 2))
    .orElse(buildPipe(start, Pipe.Downs, (a, b) => (a, b + 1), 3))
    .get

  val totalLength = count(next.x, next.y, dir, 1)
  val middle = totalLength / 2
  println(middle)

  @tailrec
  def count(x: Int, y: Int, d: Int, dist: Int): Int = {
    (input(y)(x), d) match {
      case ('S', _)=> dist
      // up
      case ('|', 1 ) => count(x, y-1, 1, dist+1)
      // down
      case ('|', _ ) => count(x, y+1, 3, dist+1)
      // left
      case ('-', 0 ) => count(x-1, y, 0, dist+1)
      // right
      case ('-', _ ) => count(x+1, y, 2, dist+1)
      // up
      case ('7', 1 ) => count(x-1, y, 0, dist+1)
      // right
      case ('7', _ ) => count(x, y+1, 3, dist+1)
      // down
      case ('J', 3 ) => count(x-1, y, 0, dist+1)
      // right
      case ('J', _ ) => count(x, y-1, 1, dist+1)
      // down
      case ('L', 3 ) => count(x+1, y, 2, dist+1)
      // left
      case ('L', _ ) => count(x, y-1, 1, dist+1)
      // up
      case ('F', 1 ) => count(x+1, y, 2, dist+1)
      // left
      case ('F', _ ) => count(x, y+1, 3, dist+1)
    }
  }

  def buildPipe(pipe: Pipe, seq: Set[Char], f: (Int, Int) => (Int, Int), dir: Int) = {
    val (i, j) = f(pipe.x, pipe.y)
    val ch = Option.when(i >= 0 && j >= 0 && i < input.head.length && j < input.length)(input(j)(i))
    ch.flatMap(c => Option.when(seq.contains(c))(Pipe(i, j), dir))
  }
}

object Day10B extends App {

  val input: Array[Array[Char]] = scala.io.Source.fromResource("aoc23/run.txt").getLines().toArray.map(_.toCharArray)

  val start = input.zipWithIndex.collectFirst {
    case (row, y) if row.contains('S') =>
      val x = row.zipWithIndex.find(_._1 == 'S').get._2
      Pipe(x, y)
  }.get

  // directions as numbers:
  // left -> 0
  // up -> 1
  // right -> 2
  // down -> 3

  val (next, dir) = buildPipe(start, Pipe.Lefts, (a, b) => (a - 1, b), 0)
    .orElse(buildPipe(start, Pipe.Ups, (a, b) => (a, b - 1), 1))
    .orElse(buildPipe(start, Pipe.Rights, (a, b) => (a + 1, b), 2))
    .orElse(buildPipe(start, Pipe.Downs, (a, b) => (a, b + 1), 3))
    .get

  val loopVertices = traceLoop(next, dir, Seq.empty[Pipe])

  val (area, border) = loopVertices.zipWithIndex.foldLeft(0, 0){
    case ((sum, border), (c, i)) =>
      val l = if (i - 1 < 0) loopVertices.length-1 else i-1
      val r = if (i + 1 >= loopVertices.length) 0 else i +1
      val left = loopVertices(l)
      val right = loopVertices(r)
      val yDiff =  right.y - left.y
      val yCount = Math.abs(left.y - c.y)
      val xCount = Math.abs(left.x - c.x)
      (sum + (c.x * yDiff), border + yCount + xCount)
  }

  // A + 1 - b/2
  val internal = Math.abs(area/2) +1 - (border/2)

  println(internal)

  @tailrec
  def traceLoop(prev: Pipe, d: Int, vertices: Seq[Pipe]): Seq[Pipe] = {
    (input(prev.y)(prev.x), d) match {
      case ('S', _)=>
        if (d != dir) vertices :+ start
        else vertices
      // up
      case ('|', 1 ) =>
        val p = Pipe(prev.x, prev.y-1)
        traceLoop(p, 1, vertices)
      // down
      case ('|', _ ) =>
        val p = Pipe(prev.x, prev.y + 1)
        traceLoop(p, 3, vertices)
      // left
      case ('-', 0 ) =>
        val p = Pipe(prev.x-1, prev.y)
        traceLoop(p, 0, vertices)
      // right
      case ('-', _ ) =>
        val p = Pipe(prev.x+1, prev.y)
        traceLoop(p, 2, vertices)
      // up
      case ('7', 1 ) =>
        val p = Pipe(prev.x-1, prev.y)
        traceLoop(p, 0, vertices :+ prev)
      // right
      case ('7', _ ) =>
        val p = Pipe(prev.x, prev.y+1)
        traceLoop(p, 3, vertices :+ prev)
      // down
      case ('J', 3 ) =>
        val p = Pipe(prev.x-1, prev.y)
        traceLoop(p, 0, vertices :+ prev)
      // right
      case ('J', _ ) =>
        val p = Pipe(prev.x, prev.y - 1)
        traceLoop(p, 1, vertices :+ prev)
      // down
      case ('L', 3 ) =>
        val p = Pipe(prev.x+1, prev.y)
        traceLoop(p, 2, vertices :+ prev)
      // left
      case ('L', _ ) =>
        val p = Pipe(prev.x, prev.y - 1)
        traceLoop(p, 1, vertices :+ prev)
      // up
      case ('F', 1 ) =>
        val p = Pipe(prev.x+1, prev.y)
        traceLoop(p, 2, vertices :+ prev)
      // left
      case ('F', _ ) =>
        val p = Pipe(prev.x, prev.y + 1)
        traceLoop(p, 3, vertices :+ prev)
    }
  }

  def buildPipe(pipe: Pipe, seq: Set[Char], f: (Int, Int) => (Int, Int), dir: Int) = {
    val (i, j) = f(pipe.x, pipe.y)
    val ch = Option.when(i >= 0 && j >= 0 && i < input.head.length && j < input.length)(input(j)(i))
    ch.flatMap(c => Option.when(seq.contains(c))(Pipe(i, j), dir))
  }
}
