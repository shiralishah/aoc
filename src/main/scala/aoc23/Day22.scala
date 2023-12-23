package aoc23

import scala.collection.immutable

object Day22A extends App {
  final case class Point(x: Int, y:Int, z:Int)
  final case class Brick(start: Point, end: Point) {

    def overlap(b: Brick): Boolean = {
      Math.max(b.start.x, this.start.x) <= Math.min(b.end.x, this.end.x) &&
        Math.max(b.start.y, this.start.y) <= Math.min(b.end.y, this.end.y)
    }
  }

  val input = scala.io.Source.fromResource("aoc23/run.txt").getLines()

  // when searching a column track everyone weve found?
  var cache: Map[(Int, Int), Seq[Brick]] = Map.empty
  val snapshot = input.foldLeft(Seq.empty[Brick]){
    case (bs, line) =>
      line match {
        case s"$x1,$y1,$z1~$x2,$y2,$z2" => bs :+ Brick(
          Point(x1.toInt, y1.toInt, z1.toInt),
          Point(x2.toInt, y2.toInt, z2.toInt)
        )
      }
  }.sortBy(_.start.z)

  val fallenBricks = snapshot.indices.foldLeft(snapshot){
    case(bricks, ind) =>
      val brick = bricks(ind)
      val nextHeight = bricks.take(ind).foldLeft(1){
        case (maxHeight, other) =>
          if (brick.overlap(other)){
            Math.max(maxHeight, other.end.z+1)
          } else maxHeight
      }
      val drop = brick.start.z - nextHeight
      val b = Brick(brick.start.copy(z = nextHeight), brick.end.copy(z = brick.end.z - drop))
      bricks.take(ind) :+ b :++ bricks.drop(ind+1)
  }

  val emptyStart = fallenBricks.indices.map(i => i -> Set.empty[Int]).toMap

  val (support, standing) = fallenBricks.zipWithIndex.foldLeft(emptyStart, emptyStart){
    case ((accSupp, accStand), (brick, j)) =>
      fallenBricks.take(j).zipWithIndex.foldLeft(accSupp, accStand){
        case ((acsp, ast), (other, i)) =>
          if (brick.overlap(other) && brick.start.z == (other.end.z+1)) {
            (acsp +(i -> (acsp(i) + j)), ast + (j -> (ast(j) + i)))
          } else (acsp, ast)
      }
  }

  val count = support.collect{
    case (b, tops) if tops.forall(i => standing.getOrElse(i, Set.empty).size >= 2) => b
  }.size

  println(count)

}

object Day22B extends App {
  final case class Point(x: Int, y:Int, z:Int)
  final case class Brick(start: Point, end: Point) {

    def overlap(b: Brick): Boolean = {
      Math.max(b.start.x, this.start.x) <= Math.min(b.end.x, this.end.x) &&
        Math.max(b.start.y, this.start.y) <= Math.min(b.end.y, this.end.y)
    }
  }

  val input = scala.io.Source.fromResource("aoc23/run.txt").getLines()

  // when searching a column track everyone weve found?
  var cache: Map[(Int, Int), Seq[Brick]] = Map.empty
  val snapshot = input.foldLeft(Seq.empty[Brick]){
    case (bs, line) =>
      line match {
        case s"$x1,$y1,$z1~$x2,$y2,$z2" => bs :+ Brick(
          Point(x1.toInt, y1.toInt, z1.toInt),
          Point(x2.toInt, y2.toInt, z2.toInt)
        )
      }
  }.sortBy(_.start.z)

  val fallenBricks = snapshot.indices.foldLeft(snapshot){
    case(bricks, ind) =>
      val brick = bricks(ind)
      val nextHeight = bricks.take(ind).foldLeft(1){
        case (maxHeight, other) =>
          if (brick.overlap(other)){
            Math.max(maxHeight, other.end.z+1)
          } else maxHeight
      }
      val drop = brick.start.z - nextHeight
      val b = Brick(brick.start.copy(z = nextHeight), brick.end.copy(z = brick.end.z - drop))
      bricks.take(ind) :+ b :++ bricks.drop(ind+1)
  }

  val emptyStart = fallenBricks.indices.map(i => i -> Set.empty[Int]).toMap

  val (support, standing) = fallenBricks.zipWithIndex.foldLeft(emptyStart, emptyStart){
    case ((accSupp, accStand), (brick, j)) =>
      fallenBricks.take(j).zipWithIndex.foldLeft(accSupp, accStand){
        case ((acsp, ast), (other, i)) =>
          if (brick.overlap(other) && brick.start.z == (other.end.z+1)) {
            (acsp +(i -> (acsp(i) + j)), ast + (j -> (ast(j) + i)))
          } else (acsp, ast)
      }
  }

  val chains = support.map{
    b => findChains(b._1)
  }.sum

  println(chains)

  def findChains(b: Int): Int = {
    var queue = support(b).filter(o => standing(o).size == 1).toSeq
    var acc = queue.toSet + b
    while(queue.nonEmpty) {
      val next = support(queue.head).filter{
        case (ind) => !acc.contains(ind) && standing(ind).forall(acc.contains)
      }
      acc = acc ++ next
      queue = queue.tail :++ next.toSeq
    }
    acc.size-1
  }

}


//88503