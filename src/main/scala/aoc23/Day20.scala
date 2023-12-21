package aoc23

object Day20A extends App {

  sealed trait Pulse

  object Pulse {
    case object Low extends Pulse

    case object High extends Pulse
  }

  sealed trait State

  object State {
    case object On extends State

    case object Off extends State
  }

  sealed trait Module {
    def outputs: Seq[String]

    def startState: Boolean

    def update(name: String, pulse: Pulse): (Module, Option[Pulse])

  }

  final case class Flip(state: State, outputs: Seq[String]) extends Module {
    override def startState: Boolean = this.state == State.Off

    override def update(name: String, pulse: Pulse): (Flip, Option[Pulse]) =
      pulse match {
        case Pulse.Low if this.state == State.Off => (this.copy(state = State.On), Some(Pulse.High))
        case Pulse.Low if this.state == State.On => (this.copy(state = State.Off), Some(Pulse.Low))
        case Pulse.High => (this, None)
      }
  }

  final case class Conjunction(memory: Map[String, Pulse], outputs: Seq[String]) extends Module {
    override def startState: Boolean = this.memory.values.forall(_ == Pulse.Low)

    override def update(name: String, pulse: Pulse): (Conjunction, Option[Pulse]) = {
      val updated = this.copy(memory = this.memory + (name -> pulse))
      if (updated.memory.values.forall(_ == Pulse.High)) (updated, Some(Pulse.Low))
      else (updated, Some(Pulse.High))
    }

  }

  final case class Broadcaster(outputs: Seq[String]) extends Module {
    override def startState: Boolean = true

    override def update(name: String, pulse: Pulse): (Broadcaster, Option[Pulse]) =
      (this, Some(pulse))
  }

  val Broad = "broadcaster"

  val input = scala.io.Source.fromResource("aoc23/run.txt").getLines()

  val initial = input.foldLeft(Map.empty[String, Module]) {
    case (ms, line) =>
      line match {
        case s"broadcaster -> $outs" => ms + (Broad -> Broadcaster(outs.split(", ").toSeq))
        case s"%$name -> $outs" => ms + (name -> Flip(State.Off, outs.split(", ").toSeq))
        case s"&$name -> $outs" => ms + (name -> Conjunction(Map.empty, outs.split(", ").toSeq))
      }
  }
  var modules: Map[String, Module] = initial.map {
    case (name, c: Conjunction) =>
      val inputMap = initial.collect {
        case (k, v) if v.outputs.contains(name) => (k -> Pulse.Low)
      }
      (name -> c.copy(memory = inputMap))
    case other => other
  }

  var pushes = 0
  var lowAcc = 0
  var highAcc = 0
  while (pushes < 1000) {
    val (high, low) = push
    lowAcc = lowAcc + low
    highAcc = highAcc + high
    pushes = pushes + 1
  }

  val pulseValue = (highAcc) * (lowAcc)
  println(pulseValue)

  def push: (Int, Int) = {
    var queue: Seq[(String, String, Pulse)] = Seq(("", Broad, Pulse.Low))
    var loCount = 1
    var hiCount = 0
    while (queue.nonEmpty) {
      val (prev, modName, pulse) = queue.head
      modules.get(modName) match {
        case Some(mod) =>
          val (newMod, nextPulseOpt) = mod.update(prev, pulse)
          modules = modules + (modName -> newMod)
          nextPulseOpt match {
            case Some(nextPulse) if nextPulse == Pulse.Low =>
              loCount = loCount + newMod.outputs.length
              queue = queue.tail :++ newMod.outputs.map(o => (modName, o, nextPulse))
            case Some(nextPulse) if nextPulse == Pulse.High =>
              hiCount = hiCount + newMod.outputs.length
              queue = queue.tail :++ newMod.outputs.map(o => (modName, o, nextPulse))
            case None =>
              queue = queue.tail
          }
        case None => queue = queue.tail
      }
    }
    (hiCount, loCount)
  }
}

object Day20B extends App {

  sealed trait Pulse

  object Pulse {
    case object Low extends Pulse

    case object High extends Pulse
  }

  sealed trait State

  object State {
    case object On extends State

    case object Off extends State
  }

  sealed trait Module {
    def outputs: Seq[String]

    def startState: Boolean

    def setStart: Module

    def update(name: String, pulse: Pulse): (Module, Option[Pulse])

  }

  final case class Flip(state: State, outputs: Seq[String]) extends Module {
    override def startState: Boolean = this.state == State.Off

    override def setStart: Module = this.copy(state = State.Off)

    override def update(name: String, pulse: Pulse): (Flip, Option[Pulse]) =
      pulse match {
        case Pulse.Low if this.state == State.Off => (this.copy(state = State.On), Some(Pulse.High))
        case Pulse.Low if this.state == State.On => (this.copy(state = State.Off), Some(Pulse.Low))
        case Pulse.High => (this, None)
      }
  }

  final case class Conjunction(memory: Map[String, Pulse], outputs: Seq[String]) extends Module {
    override def startState: Boolean = this.memory.values.forall(_ == Pulse.Low)

    override def setStart: Module = {
      val mem = this.memory.map{
        case(k, _) => (k -> Pulse.Low)
      }
      this.copy(memory = mem)
    }

    override def update(name: String, pulse: Pulse): (Conjunction, Option[Pulse]) = {
      val updated = this.copy(memory = this.memory + (name -> pulse))
      if (updated.memory.values.forall(_ == Pulse.High)) (updated, Some(Pulse.Low))
      else (updated, Some(Pulse.High))
    }

  }

  final case class Broadcaster(outputs: Seq[String]) extends Module {
    override def startState: Boolean = true

    override def setStart: Module = this

    override def update(name: String, pulse: Pulse): (Broadcaster, Option[Pulse]) =
      (this, Some(pulse))
  }

  val Broad = "broadcaster"

  val input = scala.io.Source.fromResource("aoc23/run.txt").getLines()

  val initial = input.foldLeft(Map.empty[String, Module]) {
    case (ms, line) =>
      line match {
        case s"broadcaster -> $outs" => ms + (Broad -> Broadcaster(outs.split(", ").toSeq))
        case s"%$name -> $outs" => ms + (name -> Flip(State.Off, outs.split(", ").toSeq))
        case s"&$name -> $outs" => ms + (name -> Conjunction(Map.empty, outs.split(", ").toSeq))
      }
  }
  var modules: Map[String, Module] = initial.map {
    case (name, c: Conjunction) =>
      val inputMap = initial.collect {
        case (k, v) if v.outputs.contains(name) => (k -> Pulse.Low)
      }
      (name -> c.copy(memory = inputMap))
    case other => other
  }
  val target = modules.find(_._2.outputs.contains("rx")).get._1
  val inputNum = modules.count(_._2.outputs.contains(target))

  var cycles = Map.empty[String, Int]

  var count = 1
  while (cycles.size < inputNum) {
    push(count)
    count = count+1
  }

  val pushes = cycles.values.map(_.toLong).product
  println(pushes)



  def push(pressNum: Int): Unit = {
    var queue: Seq[(String, String, Pulse)] = Seq(("", Broad, Pulse.Low))
    while (queue.nonEmpty) {
      val (prev, modName, pulse) = queue.head
      if (modName == target && pulse == Pulse.High && !cycles.contains(prev)) {
        cycles = cycles + (prev -> pressNum)
      }
      modules.get(modName) match {
        case Some(mod) =>
          val (newMod, nextPulseOpt) = mod.update(prev, pulse)
          modules = modules + (modName -> newMod)
          nextPulseOpt match {
            case Some(nextPulse) =>
              queue = queue.tail :++ newMod.outputs.map(o => (modName, o, nextPulse))
            case None =>
              queue = queue.tail
          }
        case None => queue = queue.tail
      }
    }
  }
}
