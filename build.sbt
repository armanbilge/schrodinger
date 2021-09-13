ThisBuild / baseVersion := "0.3"

ThisBuild / organization := "com.armanbilge"
ThisBuild / publishGithubUser := "armanbilge"
ThisBuild / publishFullName := "Arman Bilge"
ThisBuild / startYear := Some(2021)

enablePlugins(SonatypeCiReleasePlugin)
ThisBuild / spiewakCiReleaseSnapshots := false
ThisBuild / spiewakMainBranches := Seq("main")
ThisBuild / homepage := Some(url("https://github.com/armanbilge/schrodinger"))
ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/armanbilge/schrodinger"),
    "git@github.com:armanbilge/schrodinger.git"))
sonatypeCredentialHost := "s01.oss.sonatype.org"

addCommandAlias("prePR", "; root/clean; +root/scalafmtAll; scalafmtSbt; +root/headerCreate")

val Scala3 = "3.0.2"
ThisBuild / crossScalaVersions := Seq(Scala3)

val AlgebraVersion = "2.2.3"
val CatsVersion = "2.6.1"
val CatsEffectVersion = "3.2.8"
val CatsMtlVersion = "1.2.1"
val CommonsMathVersion = "3.6.1"
val CommonsRngVersion = "1.4"
val Fs2Version = "3.1.2"
val Specs2Version = "5.0.0-RC-11"
val ScalaCheckVersion = "1.15.3"
val DisciplineSpecs2Version = "1.2-7-e3ce260"

val commonSettings = Seq(
  scalacOptions ++= Seq("-new-syntax", "-indent", "-source:future"),
  sonatypeCredentialHost := "s01.oss.sonatype.org"
)

lazy val root =
  project
    .in(file("."))
    .aggregate(kernel, random, stats, core, monteCarlo, testkit, tests, example)
    .enablePlugins(NoPublishPlugin)

lazy val kernel = project
  .in(file("kernel"))
  .settings(
    name := "schrodinger-kernel"
  )
  .settings(commonSettings: _*)

lazy val laws = project
  .in(file("laws"))
  .dependsOn(kernel)
  .settings(
    name := "schrodinger-laws",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-laws" % CatsVersion
    )
  )
  .settings(commonSettings: _*)

lazy val logDouble = project
  .in(file("log-double"))
  .settings(
    name := "schrodinger-log-double",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "algebra" % AlgebraVersion,
      "org.specs2" %% "specs2-core" % Specs2Version,
      "io.vasilev" %% "discipline-specs2" % DisciplineSpecs2Version % Test,
      "org.typelevel" %% "algebra-laws" % AlgebraVersion % Test
    )
  )
  .settings(commonSettings: _*)

lazy val random = project
  .in(file("random"))
  .dependsOn(kernel, logDouble)
  .settings(
    name := "schrodinger-random",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % CatsVersion,
      "org.typelevel" %% "cats-mtl" % CatsMtlVersion
    )
  )
  .settings(commonSettings: _*)

lazy val core = project
  .in(file("core"))
  .dependsOn(random)
  .settings(
    name := "schrodinger",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-effect-kernel" % CatsEffectVersion,
      "org.specs2" %% "specs2-core" % Specs2Version % Test,
      "org.specs2" %% "specs2-scalacheck" % Specs2Version % Test,
      "org.apache.commons" % "commons-rng-core" % CommonsRngVersion % Test
    )
  )
  .settings(commonSettings: _*)

lazy val testkit = project
  .in(file("testkit"))
  .dependsOn(core, random)
  .settings(
    name := "schrodinger-testkit",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-laws" % CatsVersion,
      "org.apache.commons" % "commons-math3" % "3.6.1",
      "org.specs2" %% "specs2-core" % Specs2Version % Test,
      "org.specs2" %% "specs2-scalacheck" % Specs2Version % Test
    )
  )
  .settings(commonSettings: _*)

lazy val tests = project
  .in(file("tests"))
  .dependsOn(testkit % Test, laws % Test)
  .settings(
    name := "schrodinger-tests",
    libraryDependencies ++= Seq(
      "io.vasilev" %% "discipline-specs2" % DisciplineSpecs2Version % Test,
      "org.typelevel" %% "cats-effect-laws" % CatsEffectVersion % Test,
      "org.typelevel" %% "cats-effect-testkit" % CatsEffectVersion % Test,
      "org.apache.commons" % "commons-rng-core" % CommonsRngVersion % Test,
      "org.apache.commons" % "commons-rng-sampling" % CommonsRngVersion % Test
    )
  )
  .settings(commonSettings: _*)
  .enablePlugins(NoPublishPlugin)

lazy val stats = project
  .in(file("stats"))
  .dependsOn(kernel, logDouble)
  .settings(
    name := "schrodinger-stats",
    libraryDependencies ++= Seq(
      "org.apache.commons" % "commons-math3" % CommonsMathVersion,
      "org.typelevel" %% "cats-core" % CatsVersion,
      "org.typelevel" %% "cats-laws" % CatsVersion % Test
    )
  )
  .settings(commonSettings: _*)

lazy val monteCarlo = project
  .in(file("monte-carlo"))
  .dependsOn(kernel, logDouble % Test, testkit % Test)
  .settings(
    name := "schrodinger-monte-carlo",
    libraryDependencies ++= Seq(
      "co.fs2" %% "fs2-core" % Fs2Version,
      "org.typelevel" %% "algebra" % AlgebraVersion,
      "org.typelevel" %% "cats-effect-std" % CatsEffectVersion,
      "org.typelevel" %% "cats-laws" % CatsVersion % Test,
      "org.typelevel" %% "cats-effect-laws" % CatsEffectVersion % Test,
      "org.typelevel" %% "cats-effect-testkit" % CatsEffectVersion % Test,
      "io.vasilev" %% "discipline-specs2" % DisciplineSpecs2Version % Test
    )
  )
  .settings(commonSettings: _*)

lazy val example = project
  .in(file("example"))
  .dependsOn(core)
  .settings(
    name := "schrodinger-example",
    libraryDependencies += "org.typelevel" %% "cats-effect" % CatsEffectVersion
  )
  .settings(commonSettings: _*)
  .enablePlugins(NoPublishPlugin)
