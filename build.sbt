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

val Scala3 = "3.1.1-RC1-bin-20211013-3b5ff7a-NIGHTLY"
ThisBuild / crossScalaVersions := Seq(Scala3)

val AlgebraVersion = "2.2.3"
val CatsVersion = "2.6.1"
val CatsEffectVersion = "3.2.9"
val CommonsMathVersion = "3.6.1"
val CommonsRngVersion = "1.4"
val Fs2Version = "3.1.5"
val Specs2Version = "5.0.0-RC-15"
val ScalaCheckVersion = "1.15.3"
val VaultVersion = "3.1.0"
val DisciplineSpecs2Version = "1.2-7-e3ce260"

val commonSettings = Seq(
  scalacOptions ++= Seq("-new-syntax", "-indent", "-source:future"),
  sonatypeCredentialHost := "s01.oss.sonatype.org"
)

lazy val root =
  project
    .in(file("."))
    .aggregate(
      kernel.jvm,
      kernel.js,
      laws.jvm,
      laws.js,
      math.jvm,
      math.js,
      randomTestkit,
      random,
      stats,
      core,
      monteCarlo,
      testkit,
      tests)
    .enablePlugins(NoPublishPlugin)

lazy val kernel = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("kernel"))
  .settings(
    name := "schrodinger-kernel"
  )
  .settings(commonSettings: _*)

lazy val laws = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("laws"))
  .dependsOn(kernel)
  .settings(
    name := "schrodinger-laws",
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-laws" % CatsVersion
    )
  )
  .settings(commonSettings: _*)

lazy val math = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("math"))
  .settings(
    name := "schrodinger-math",
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "algebra" % AlgebraVersion,
      "org.specs2" %%% "specs2-core" % Specs2Version,
      "io.vasilev" %%% "discipline-specs2" % DisciplineSpecs2Version % Test,
      "org.typelevel" %%% "algebra-laws" % AlgebraVersion % Test
    )
  )
  .settings(commonSettings: _*)

lazy val randomTestkit = project
  .in(file("random-testkit"))
  .dependsOn(kernel.jvm)
  .settings(
    name := "schrodinger-random-testkit",
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-core" % CatsVersion,
      "org.typelevel" %%% "vault" % VaultVersion,
      "org.specs2" %%% "specs2-scalacheck" % Specs2Version % Test,
      "org.apache.commons" % "commons-rng-core" % CommonsRngVersion % Test
    )
  )
  .settings(commonSettings: _*)

lazy val random = project
  .in(file("random"))
  .dependsOn(kernel.jvm, math.jvm, randomTestkit % Test)
  .settings(
    name := "schrodinger-random",
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-core" % CatsVersion,
      "org.specs2" %%% "specs2-scalacheck" % Specs2Version % Test,
      "org.apache.commons" % "commons-rng-core" % CommonsRngVersion % Test,
      "org.apache.commons" % "commons-rng-sampling" % CommonsRngVersion % Test
    )
  )
  .settings(commonSettings: _*)

lazy val core = project
  .in(file("core"))
  .dependsOn(random)
  .settings(
    name := "schrodinger",
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-effect" % CatsEffectVersion,
      "org.specs2" %%% "specs2-core" % Specs2Version % Test,
      "org.specs2" %%% "specs2-scalacheck" % Specs2Version % Test,
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
      "org.typelevel" %%% "cats-laws" % CatsVersion,
      "org.typelevel" %%% "cats-effect" % CatsEffectVersion,
      "org.apache.commons" % "commons-math3" % "3.6.1",
      "org.specs2" %%% "specs2-core" % Specs2Version % Test,
      "org.specs2" %%% "specs2-scalacheck" % Specs2Version % Test
    )
  )
  .settings(commonSettings: _*)

lazy val tests = project
  .in(file("tests"))
  .dependsOn(testkit % Test, laws.jvm % Test)
  .settings(
    name := "schrodinger-tests",
    libraryDependencies ++= Seq(
      "io.vasilev" %%% "discipline-specs2" % DisciplineSpecs2Version % Test,
      "org.typelevel" %%% "cats-effect-laws" % CatsEffectVersion % Test,
      "org.typelevel" %%% "cats-effect-testkit" % CatsEffectVersion % Test,
      "org.apache.commons" % "commons-rng-core" % CommonsRngVersion % Test,
      "org.apache.commons" % "commons-rng-sampling" % CommonsRngVersion % Test
    )
  )
  .settings(commonSettings: _*)
  .enablePlugins(NoPublishPlugin)

lazy val stats = project
  .in(file("stats"))
  .dependsOn(kernel.jvm, math.jvm)
  .settings(
    name := "schrodinger-stats",
    libraryDependencies ++= Seq(
      "org.apache.commons" % "commons-math3" % CommonsMathVersion,
      "org.typelevel" %%% "cats-core" % CatsVersion,
      "org.typelevel" %%% "cats-laws" % CatsVersion % Test
    )
  )
  .settings(commonSettings: _*)

lazy val monteCarlo = project
  .in(file("monte-carlo"))
  .dependsOn(kernel.jvm, math.jvm, testkit % Test)
  .settings(
    name := "schrodinger-monte-carlo",
    libraryDependencies ++= Seq(
      "co.fs2" %%% "fs2-core" % Fs2Version,
      "org.typelevel" %%% "algebra" % AlgebraVersion,
      "org.typelevel" %%% "cats-effect-std" % CatsEffectVersion,
      "org.typelevel" %%% "cats-laws" % CatsVersion % Test,
      "org.typelevel" %%% "cats-effect-laws" % CatsEffectVersion % Test,
      "org.typelevel" %%% "cats-effect-testkit" % CatsEffectVersion % Test,
      "io.vasilev" %%% "discipline-specs2" % DisciplineSpecs2Version % Test
    )
  )
  .settings(commonSettings: _*)
