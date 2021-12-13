resolvers +=
  "Sonatype OSS Snapshots" at "https://s01.oss.sonatype.org/content/repositories/snapshots"

addSbtPlugin("io.vasilev" % "sbt-spiewak-sonatype" % "0.23-11-6c79539-SNAPSHOT")
addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.4.5")
addSbtPlugin("org.xerial.sbt" % "sbt-sonatype" % "3.9.10")
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.8.0")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.1.0")
