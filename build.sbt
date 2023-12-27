name := "aoc"
version := "0.1"
scalaVersion := "2.13.10"
val ZioVersion = "2.0.19"
libraryDependencies := Seq(
  "dev.zio" %% "zio" % ZioVersion,
  "dev.zio" %% "zio-streams" % ZioVersion,
  "dev.zio" %% "zio-macros" % ZioVersion,
  "dev.zio" %% "zio-json" % "0.5.0",
  "org.scalanlp" %% "breeze" % "2.1.0",
  ("org.scala-graph" %% "graph-core" % "2.0.1").cross(CrossVersion.for3Use2_13),
  "org.jgrapht" % "jgrapht-core" % "1.5.1"
)