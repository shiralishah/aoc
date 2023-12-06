package aoc23

final case class GardenNode(src: Long, dest: Long, range: Long){

  val srcEnd = this.src + this.range - 1
  val destEnd = this.dest + this.range - 1

  def getValue(num: Long): Option[Long] = {
    val diff = num - this.src
    Option.when(diff < this.range && diff >= 0)(this.dest + diff)
  }

  def findNodes(nodes: Seq[RangeNode]): (Seq[RangeNode], Seq[RangeNode]) = {
    val (found, remaining) = nodes.map(_.produceOverlap(this)).unzip
    (found.flatten, remaining.flatten)
  }
}
object GardenNode {
  def build(str: String): GardenNode = {
    val values = str.split(' ')
    GardenNode(values(1).toLong, values(0).toLong, values(2).toLong)
  }
}

final case class RangeNode(start: Long, end: Long) {
  def produceOverlap(node: GardenNode): (Option[RangeNode], Seq[RangeNode]) = {
    (this.start, this.end) match {
      // inside
      case (st, end) if node.src <= st && end <= node.srcEnd =>
        val diffSt = st - node.src
        val diffEnd = end - node.src
        (Some(RangeNode(node.dest+diffSt, node.dest+diffEnd)), Seq.empty)
      // outside
      case (st, end) if st < node.src && node.srcEnd < end =>
        val front = RangeNode(st, node.src-1)
        val back = RangeNode(node.srcEnd+1, end)
        (Some(RangeNode(node.dest, node.destEnd)), Seq(front, back))
      // right overlap
      case (st, end) if st < node.src && node.src <= end =>
        val left = RangeNode(st, node.src-1)
        val diff = end - node.src
        val n = RangeNode(node.dest, node.dest+diff)
        (Some(n), Seq(left))
      // left overlap
      case (st, end) if st < node.srcEnd && node.srcEnd < end =>
        val right = RangeNode(node.srcEnd+1, end)
        val diff = st - node.src
        val n = RangeNode(node.dest+diff, node.destEnd)
        (Some(n), Seq(right))
      // no overlap
      case (st, end) => (None, Seq(RangeNode(st, end)))
    }
  }
}

object Day05A extends App {
  val input = scala.io.Source.fromResource("aoc23/run.txt").getLines()
    .mkString("\n").split("\\n\\n")
  val (seedStr, remStr) = (input.head, input.tail.map(_.split("\\n")))
  val soilNodes = remStr(0).tail.map(GardenNode.build)
  val fertNodes = remStr(1).tail.map(GardenNode.build)
  val waterNodes = remStr(2).tail.map(GardenNode.build)
  val lightNodes = remStr(3).tail.map(GardenNode.build)
  val tempNodes = remStr(4).tail.map(GardenNode.build)
  val humidNodes = remStr(5).tail.map(GardenNode.build)
  val locNodes = remStr(6).tail.map(GardenNode.build)
  val minLocation = seedStr.split(' ').tail
    .map(_.toLong).foldLeft(-1L){
    case (min, seed) =>
      val soil = soilNodes.flatMap(_.getValue(seed)).headOption.getOrElse(seed)
      val fert = fertNodes.flatMap(_.getValue(soil)).headOption.getOrElse(soil)
      val water = waterNodes.flatMap(_.getValue(fert)).headOption.getOrElse(fert)
      val light = lightNodes.flatMap(_.getValue(water)).headOption.getOrElse(water)
      val temp = tempNodes.flatMap(_.getValue(light)).headOption.getOrElse(light)
      val humid = humidNodes.flatMap(_.getValue(temp)).headOption.getOrElse(temp)
      val loc = locNodes.flatMap(_.getValue(humid)).headOption.getOrElse(humid)
      if (min < 0) loc
      else if (loc < min) loc
      else min
  }

  println(minLocation)

}

object Day05B extends App {
  val input = scala.io.Source.fromResource("aoc23/run.txt").getLines()
    .mkString("\n").split("\\n\\n")
  val (seedStr, remStr) = (input.head, input.tail.map(_.split("\\n")))
  val soilNodes = remStr(0).tail.map(GardenNode.build)
  val fertNodes = remStr(1).tail.map(GardenNode.build)
  val waterNodes = remStr(2).tail.map(GardenNode.build)
  val lightNodes = remStr(3).tail.map(GardenNode.build)
  val tempNodes = remStr(4).tail.map(GardenNode.build)
  val humidNodes = remStr(5).tail.map(GardenNode.build)
  val locNodes = remStr(6).tail.map(GardenNode.build)
  val seeds = seedStr.split(' ').tail.toSeq.map(_.toLong).grouped(2).collect{
    case Seq(s, r) => RangeNode(s, s+r-1)
  }.toSeq

  val soil = mapRanges(soilNodes, seeds)
  val fert = mapRanges(fertNodes, soil)
  val water = mapRanges(waterNodes, fert)
  val light = mapRanges(lightNodes, water)
  val temp = mapRanges(tempNodes, light)
  val humid = mapRanges(humidNodes, temp)
  val loc = mapRanges(locNodes, humid)

  val minLocation = loc.map(_.start).min

  println(minLocation)

  def mapRanges(nodes: Array[GardenNode], starting: Seq[RangeNode]): Seq[RangeNode] = {
    val (remSeeds, finalSoil) = nodes.foldLeft(starting, Seq.empty[RangeNode]) {
      case ((seedList, soilList), node) =>
        val (soil, rem) = node.findNodes(seedList)
        (rem, soilList :++ soil)
    }
    remSeeds :++ finalSoil
  }

}

