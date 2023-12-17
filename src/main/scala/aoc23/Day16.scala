package aoc23

sealed trait Direction

object Direction {
  case object Left extends Direction

  case object Right extends Direction

  case object Up extends Direction

  case object Down extends Direction
}

final case class Beam(x: Int, y: Int, direction: Direction) {
  def next: Beam =
    this.direction match {
      case Direction.Left => this.left
      case Direction.Right => this.right
      case Direction.Up => this.up
      case Direction.Down => this.down
    }

  def left: Beam = this.copy(x = this.x - 1, direction = Direction.Left)

  def right: Beam = this.copy(x = this.x + 1, direction = Direction.Right)

  def up: Beam = this.copy(y = this.y - 1, direction = Direction.Up)

  def down: Beam = this.copy(y = this.y + 1, direction = Direction.Down)
}

object Day16A extends App {
  val input = scala.io.Source.fromResource("aoc23/run.txt").getLines().toArray.map(_.toCharArray)
  val rowCount = input.length
  val colCount = input.head.length
  var beams = Seq(Beam(-1, 0, Direction.Right))

  var energized = Map.empty[(Int, Int), Set[Direction]]

  while (beams.nonEmpty) {
    val newBeams = beams.flatMap(move)
    beams = newBeams
  }

  val energizedCount = energized.count(i => i._2.nonEmpty)

  println(energizedCount)

  def move(beam: Beam): Seq[Beam] = {
    beam.direction match {
      case Direction.Right =>
        if ((beam.x + 1) >= colCount) Seq.empty
        else {
          val next = beam.copy(x = beam.x + 1)
          input(next.y)(next.x) match {
            case '.' => energize(next, Direction.Right)
            case '-' => energize(next, Direction.Right)
            case '|' => energize(next, Direction.Up) :++ energize(next, Direction.Down)
            case '/' => energize(next, Direction.Up)
            case _ => energize(next, Direction.Down)
          }
        }
      case Direction.Left =>
        if ((beam.x - 1) < 0) Seq.empty
        else {
          val next = beam.copy(x = beam.x - 1)
          input(next.y)(next.x) match {
            case '.' => energize(next, Direction.Left)
            case '-' => energize(next, Direction.Left)
            case '|' => energize(next, Direction.Up) :++ energize(next, Direction.Down)
            case '/' => energize(next, Direction.Down)
            case _ => energize(next, Direction.Up)
          }
        }
      case Direction.Up =>
        if ((beam.y - 1) < 0) Seq.empty
        else {
          val next = beam.copy(y = beam.y - 1)
          input(next.y)(next.x) match {
            case '.' => energize(next, Direction.Up)
            case '|' => energize(next, Direction.Up)
            case '-' => energize(next, Direction.Left) :++ energize(next, Direction.Right)
            case '/' => energize(next, Direction.Right)
            case _ => energize(next, Direction.Left)
          }
        }
      case Direction.Down =>
        if ((beam.y + 1) >= rowCount) Seq.empty
        else {
          val next = beam.copy(y = beam.y + 1)
          input(next.y)(next.x) match {
            case '.' => energize(next, Direction.Down)
            case '|' => energize(next, Direction.Down)
            case '-' => energize(next, Direction.Left) :++ energize(next, Direction.Right)
            case '/' => energize(next, Direction.Left)
            case _ => energize(next, Direction.Right)
          }
        }
    }
  }

  def energize(beam: Beam, dir: Direction): Seq[Beam] = {
    (energized.get(beam.x, beam.y) match {
      case Some(directions) if directions.contains(dir) => None
      case Some(directions) =>
        energized += ((beam.x, beam.y) -> (directions + dir))
        Some(beam.copy(direction = dir))
      case None =>
        energized += ((beam.x, beam.y) -> Set(dir))
        Some(beam.copy(direction = dir))
    }).map(Seq(_)).getOrElse(Seq.empty)
  }
}

object Day16B extends App {
  val input = scala.io.Source.fromResource("aoc23/run.txt").getLines().toArray.map(_.toCharArray)
  val rowCount = input.length
  val colCount = input.head.length

  var traveled = Set.empty[Beam]

  val starts = (0 until rowCount).foldLeft(Seq.empty[Beam]){
    case (set, r) => set :+ Beam(0, r, Direction.Right) :+ Beam(colCount-1, r, Direction.Left)
  } :++ (0 until Day16B.colCount).foldLeft(Seq.empty[Beam]) {
    case (set, c) => set :+ Beam(c, 0, Direction.Down) :+ Beam(c, rowCount-1, Direction.Up)
  }

  val max = starts.zipWithIndex.foldLeft(0L){
    case (m, (start, ind)) =>
      traveled = Set.empty[Beam]
      val res = count(move(start, Seq.empty))
      Math.max(m, res)
  }

  println(max)

  def count(visited: Seq[Beam]) =
    visited.groupMapReduce(x => (x.x, x.y))(x => Set(x.direction))((a, b) => a ++ b).size


  def move(beam: Beam, path: Seq[Beam]): Seq[Beam] = {
    if (beam.x >= colCount || beam.x < 0 || beam.y >= rowCount || beam.y < 0) {
      traveled = traveled + beam
      path
    } else if (traveled.contains(beam)) {
      path
    } else {
      traveled = traveled + beam
      val char = input(beam.y)(beam.x)
      char match {
        case '.' => move(beam.next, path :+ beam)
        case '-' if beam.direction == Direction.Left || beam.direction == Direction.Right =>
          move(beam.next, path :+ beam)
        case '-' =>
          move(beam.left, path :+ beam.copy(direction = Direction.Left)) ++
            move(beam.right, path :+ beam.copy(direction = Direction.Right))
        case '|' if beam.direction == Direction.Up || beam.direction == Direction.Down =>
          move(beam.next, path :+ beam)
        case '|' =>
          move(beam.up, path :+ beam.copy(direction = Direction.Up)) ++
            move(beam.down, path :+ beam.copy(direction = Direction.Down))
        case '/' =>
          beam.direction match {
            case Direction.Left => move(beam.down, path :+ beam.copy(direction = Direction.Down))
            case Direction.Right => move(beam.up, path :+ beam.copy(direction = Direction.Up))
            case Direction.Up => move(beam.right, path :+ beam.copy(direction = Direction.Right))
            case Direction.Down => move(beam.left, path :+ beam.copy(direction = Direction.Left))
          }
        case _ =>
          beam.direction match {
            case Direction.Right => move(beam.down, path :+ beam.copy(direction = Direction.Down))
            case Direction.Left => move(beam.up, path :+ beam.copy(direction = Direction.Up))
            case Direction.Down => move(beam.right, path :+ beam.copy(direction = Direction.Right))
            case Direction.Up => move(beam.left, path :+ beam.copy(direction = Direction.Left))
          }
      }
    }
  }
}


//  val vis = visualize(energized)
//  println(vis)
//
//  println(energized.size)
//
//  def visualize(m: Map[(Int, Int), Set[Direction]]): String = {
//    (0 until rowCount).foldLeft(Seq.empty[String]) {
//      case (seq, y) =>
//        val str = (0 until colCount).foldLeft("") {
//          case (s, x) =>
//            m.get((x, y)) match {
//              case Some(directions) if directions.nonEmpty => s + "#"
//              case _ => s + "."
//            }
//        }
//        seq :+ str
//    }.mkString("\n")
//  }



