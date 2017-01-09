name := "collz"

organization := "a14e"

version := "0.1-SNAPSHOT"

scalaVersion := "2.12.0"

crossScalaVersions := Seq("2.11.8", "2.12.0")


javacOptions in (Compile, compile) ++= {
  val javaVersion = if (scalaVersion.value.startsWith("2.11")) "1.6" else "1.8"
  Seq("-source", javaVersion, "-target", javaVersion)
}

libraryDependencies ++= Seq("org.scalatest" %% "scalatest" % "3.0.1" % "test")
