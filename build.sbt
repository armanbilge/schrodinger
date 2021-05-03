import sbt.internal.util.SourcePosition

import scala.util.Try

ThisBuild / baseVersion := "0.2"

ThisBuild / organization := "com.armanbilge"
ThisBuild / publishGithubUser := "armanbilge"
ThisBuild / publishFullName := "Arman Bilge"
ThisBuild / startYear := Some(2021)

mimaPreviousArtifacts := Set()

enablePlugins(SonatypeCiReleasePlugin)
ThisBuild / spiewakCiReleaseSnapshots := true
ThisBuild / git.formattedShaVersion ~= {
  _.map { v => if (!v.endsWith("-SNAPSHOT")) v + "-SNAPSHOT" else v }
}
ThisBuild / spiewakMainBranches := Seq("main")
ThisBuild / homepage := Some(url("https://github.com/armanbilge/schrodinger"))
ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/armanbilge/schrodinger"),
    "git@github.com:armanbilge/schrodinger.git"))
sonatypeCredentialHost := "s01.oss.sonatype.org"

ThisBuild / scalaVersion := "2.13.5"

replaceCommandAlias(
  "ci",
  "; project /; headerCheckAll; scalafmtCheckAll; scalafmtSbtCheck; clean; testIfRelevant; mimaReportBinaryIssuesIfRelevant"
)
addCommandAlias("prePR", "; root/clean; +root/scalafmtAll; scalafmtSbt; +root/headerCreate")

val CatsVersion = "2.6.0"
val CatsEffectVersion = "3.1.0"
val Specs2Version = "4.11.0"
val ScalaCheckVersion = "1.15.3"
val DisciplineVersion = "1.1.5"

lazy val root =
  project.aggregate(core, effect, laws, testkit, tests, example).enablePlugins(NoPublishPlugin)

lazy val core = project
  .in(file("core"))
  .settings(
    name := "schrodinger",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % CatsVersion,
      "org.specs2" %% "specs2-core" % Specs2Version % Test,
      "org.specs2" %% "specs2-scalacheck" % Specs2Version % Test,
      "org.apache.commons" % "commons-rng-core" % "1.3" % Test
    )
  )

lazy val effect = project
  .in(file("effect"))
  .dependsOn(core)
  .settings(
    name := "schrodinger-effect",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-effect-kernel" % CatsEffectVersion
    )
  )

lazy val laws = project
  .in(file("laws"))
  .dependsOn(core)
  .settings(
    name := "schrodinger-laws",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-laws" % CatsVersion,
      "org.typelevel" %% "discipline-specs2" % DisciplineVersion % Test
    )
  )

lazy val testkit = project
  .in(file("testkit"))
  .dependsOn(core)
  .settings(
    name := "schrodinger-testkit",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-laws" % CatsVersion,
      "org.apache.commons" % "commons-math3" % "3.6.1",
      "org.specs2" %% "specs2-core" % Specs2Version % Test,
      "org.specs2" %% "specs2-scalacheck" % Specs2Version % Test
    )
  )

lazy val tests = project
  .in(file("tests"))
  .dependsOn(core % Test, effect % Test, laws % Test, testkit % Test)
  .settings(
    name := "schrodinger-tests",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "discipline-specs2" % DisciplineVersion % Test,
      "org.typelevel" %% "cats-kernel-laws" % CatsVersion % Test,
      "org.typelevel" %% "cats-effect-laws" % CatsEffectVersion % Test,
      "org.typelevel" %% "cats-effect-testkit" % CatsEffectVersion % Test
    )
  )
  .enablePlugins(NoPublishPlugin)

lazy val example = project
  .in(file("example"))
  .dependsOn(core, effect)
  .settings(
    name := "schrodinger-example",
    libraryDependencies += "org.typelevel" %% "cats-effect" % CatsEffectVersion
  )
  .enablePlugins(NoPublishPlugin)
