name := "collz"

organization := "com.github.a14e"

version := "0.2.2"

scalaVersion := "2.12.1"

crossScalaVersions := Seq("2.11.8", "2.12.1")


javacOptions in(Compile, compile) ++= {
  val javaVersion = if (scalaVersion.value.startsWith("2.11")) "1.6" else "1.8"
  Seq("-source", javaVersion, "-target", javaVersion)
}

libraryDependencies ++= Seq("org.scalatest" %% "scalatest" % "3.0.1" % "test")



publishArtifact in Test := false

////////////////////
// publishing

pomExtra := {
  <url>https://github.com/a14e/collz/</url>
    <licenses>
      <license>
        <name>MIT</name>
        <url>https://github.com/a14e/collz/blob/master/LICENSE.txt</url>
        <distribution>repo</distribution>
      </license>
    </licenses>
    <scm>
      <connection>scm:git:git@github.com:a14e/collz.git</connection>
      <url>https://github.com/a14e/collz.git</url>
    </scm>
    <developers>
      <developer>
        <id>AndrewInstance</id>
        <name>Andrew Borisenko</name>
        <email>m0hct3r@gmail.com</email>
      </developer>
    </developers>
}

publishMavenStyle := true

publishTo := {
  val base = "https://oss.sonatype.org/"
  if (version.value.trim.endsWith("SNAPSHOT"))
    Some("snapshots" at base + "content/repositories/snapshots/")
  else
    Some("releases" at base + "service/local/staging/deploy/maven2/")
}
//credentials += Credentials(Path.userHome / ".ivy2" / ".credentials")

// только чтобы переписывать фаилы при сборке по http://stackoverflow.com/questions/27530507/sbt-publish-only-when-version-does-not-exist
isSnapshot := true

pomIncludeRepository := { x => false }

pgpReadOnly := false
//useGpg := true