package aoc23

object Day15A extends App {
  val input = scala.io.Source.fromResource("aoc23/run.txt").mkString.split(',').map(_.toCharArray)
  val total = input.foldLeft(0L){
    case (sum, ins) =>
      sum + getHash(ins).toLong
  }

  println(total)
  def getHash(inp: Array[Char]): Int =
    inp.foldLeft(0) {
      case (curr, c) =>
        val updated = curr + c.toInt
        val mul = updated * 17
        mul % 256
    }

}

final case class Lens(label: String, focalVal: Int)

object Day15B extends App {
  val input = scala.io.Source.fromResource("aoc23/run.txt").mkString.split(',')
  val hashMap = input.foldLeft(Map.empty[Int, Seq[Lens]]) {
    case (map, line) => line match {
      case s"$lab-" =>
        val hash = getHash(lab)
        val lenses = map.getOrElse(hash, Seq.empty[Lens])
        val updated = lenses.filterNot(_.label == lab)
        if (updated.isEmpty) map.removed(hash) else map + (hash -> updated)
      case s"$lab=$v" =>
        val hash = getHash(lab)
        val lenses = map.getOrElse(hash, Seq.empty[Lens])
        val updated = lenses.zipWithIndex.find(x => x._1.label == lab) match {
          case Some((lens, ind)) =>
            val (front, back) = lenses.splitAt(ind)
            (front :+ lens.copy(focalVal = v.toInt)) :++ back.tail
          case None => lenses :+ Lens(lab, v.toInt)
        }
        map + (hash -> updated)
    }
  }

  val power = getPower(hashMap)

  println(power)

  def getPower(m: Map[Int, Seq[Lens]]): Long = {
    m.foldLeft(0L){
      case (power, (box, lenses)) =>
        power + lenses.zipWithIndex.foldLeft(0L){
          case (boxPower, (lens, ind)) =>
            boxPower + ((box+1)*(ind+1)*(lens.focalVal))
        }
    }
  }


  def getHash(label: String): Int =
    label.foldLeft(0) {
      case (curr, c) =>
        val updated = curr + c.toInt
        val mul = updated * 17
        mul % 256
    }

}
