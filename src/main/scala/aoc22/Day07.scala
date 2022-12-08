package aoc22

case class FileTrie(node: FileNode) {
  val root = node

  def collectDirectories(): Set[FileNode] =
    collectDirectories(root, Set.empty)

  private def collectDirectories(n: FileNode, acc: Set[FileNode]): Set[FileNode] = {
      if (n.isDirectory) {
        val dirKids = n.kids.values.filter(_.isDirectory)
        acc + n ++ dirKids.flatMap(collectDirectories(_, acc + n))
      } else acc
    }
}

case class FileNode(name: String, contents: Option[Long], kids: Map[String, FileNode]) {
  val size: Long = contents.getOrElse(kids.values.map(_.size).sum)
  val isDirectory = contents.isEmpty

  def children = (str:String) => kids.getOrElse(str, FileNode(s"$name/$str", None, Map.empty))

  private def put(key: Seq[String], value: Long, pos:Int): FileNode = {
    if (pos == key.length) FileNode(key.mkString("/"), Some(value), kids)
    else {
      val str = key(pos)
      FileNode(key.take(pos).mkString("/"), contents, kids.updated(str, children(str).put(key, value, pos +1)))
    }
  }

  private def get(key: Seq[String], pos:Int): FileNode = {
    if (pos == key.length) this else children(key(pos)).get(key, pos+1)
  }

  def put(key: Seq[String], value: Long): FileNode =
    put(key, value, 0)

  def get(key: Seq[String]): FileNode =
    get(key, 0)
}

object Day07A extends App {
  val input = scala.io.Source.fromResource("aoc22/day07.txt").getLines().toSeq
  val (files, _) = input.foldLeft(Map.empty[String, Long], "") {
    case((filesAcc,  path), command) =>
      command match {
        case s"$$ cd .." =>
          val p = if (path.length == 1) path else path.split("/").dropRight(1).mkString("/")
          (filesAcc, p)
        case s"$$ cd $dir" if dir == "/" => (filesAcc, "")
        case s"$$ cd $dir" => (filesAcc, s"$path/$dir")
        case s"$$ ls" => (filesAcc, path)
        case s"dir $dir" => (filesAcc,  path)
        case s"$num $name" =>
          (filesAcc + (s"$path/$name" -> num.toLong), path)
      }
  }

  val trie = files.foldLeft(FileNode("", None, Map.empty)) {
    case(t, entry) => t.put(entry._1.split("/"), entry._2)
  }
  val smallTotal = FileTrie(trie.get(Seq(""))).collectDirectories().toSeq
    .map(_.size).filter(_ < 100000L).sum

  println(smallTotal)
}

object Day07B extends App {
  val input = scala.io.Source.fromResource("aoc22/day07.txt").getLines().toSeq
  val (files, _) = input.foldLeft(Map.empty[String, Long], "") {
    case ((filesAcc, path), command) =>
      command match {
        case s"$$ cd .." =>
          val p = if (path.length == 1) path else path.split("/").dropRight(1).mkString("/")
          (filesAcc, p)
        case s"$$ cd $dir" if dir == "/" => (filesAcc, "")
        case s"$$ cd $dir" => (filesAcc, s"$path/$dir")
        case s"$$ ls" => (filesAcc, path)
        case s"dir $dir" => (filesAcc, path)
        case s"$num $name" =>
          (filesAcc + (s"$path/$name" -> num.toLong), path)
      }
  }

  val trie = files.foldLeft(FileNode("", None, Map.empty)) {
    case (t, entry) => t.put(entry._1.split("/"), entry._2)
  }

  val rootSize = trie.get(Seq("")).size
  val reqSpace = 30000000L - (70000000L - rootSize)

  val candidate = FileTrie(trie.get(Seq(""))).collectDirectories().toSeq
    .map(_.size).filter(_ >= reqSpace).sorted.head

  println(candidate)
}
