package aoc23

import scala.annotation.tailrec

final case class Point(x: Int, y: Int)

object Day11A extends App {
  val input = scala.io.Source.fromResource("aoc23/test.txt").getLines().toArray.map(_.toCharArray)
  val (emptyRows, coordinates) = input.zipWithIndex.foldLeft(Seq.empty[Int], Seq.empty[Point]){
    case((empRow, coords), (row, y)) =>
      val gals = row.zipWithIndex.collect {
        case (c, x) if c == '#' => Point(x, y)
      }
      (if (gals.isEmpty) empRow :+ y else empRow, coords :++ gals)
  }
  val emptyCols = input.transpose.zipWithIndex.collect{
    case (col, ind) if !col.contains('#') => ind
  }.toSeq

  def distance(a: Point, b: Point): Long = {
    val dist = Math.abs(a.y-b.y) + Math.abs(a.x-b.x)
    val emptyRowCount = emptyRows.count(n =>
      Math.min(a.y, b.y) < n && n < Math.max(a.y, b.y))
    val emptyColCount = emptyCols.count(n =>
      Math.min(a.x, b.x) < n && n < Math.max(a.x, b.x)
    )
    dist + emptyRowCount + emptyColCount
  }

  @tailrec
  def sumPairs(points: Seq[Point], total: Long): Long = {
    if (points.isEmpty) total
    else {
      val (head, rem) = (points.head, points.tail)
      val sum = rem.map(distance(head, _)).sum
      sumPairs(rem, total + sum)
    }
  }

  val totalDistance = sumPairs(coordinates, 0L)
  println(totalDistance)
}

object Day11B extends App {
  val MILL = 1000000
  val input = scala.io.Source.fromResource("aoc23/test.txt").getLines().toArray.map(_.toCharArray)
  val (emptyRows, coordinates) = input.zipWithIndex.foldLeft(Seq.empty[Int], Seq.empty[Point]){
    case((empRow, coords), (row, y)) =>
      val gals = row.zipWithIndex.collect {
        case (c, x) if c == '#' => Point(x, y)
      }
      (if (gals.isEmpty) empRow :+ y else empRow, coords :++ gals)
  }
  val emptyCols = input.transpose.zipWithIndex.collect{
    case (col, ind) if !col.contains('#') => ind
  }.toSeq

  def distance(a: Point, b: Point): Long = {
    val dist = Math.abs(a.y-b.y) + Math.abs(a.x-b.x)
    val emptyRowCount = emptyRows.count(n =>
      Math.min(a.y, b.y) < n && n < Math.max(a.y, b.y))
    val emptyColCount = emptyCols.count(n =>
      Math.min(a.x, b.x) < n && n < Math.max(a.x, b.x)
    )
    dist + emptyRowCount*(MILL-1) + emptyColCount*(MILL-1)
  }

  @tailrec
  def sumPairs(points: Seq[Point], total: Long): Long = {
    if (points.isEmpty) total
    else {
      val (head, rem) = (points.head, points.tail)
      val sum = rem.map(distance(head, _)).sum
      sumPairs(rem, total + sum)
    }
  }

  val totalDistance = sumPairs(coordinates, 0L)
  println(totalDistance)
}
