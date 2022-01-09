ThisBuild / baseVersion := "0.3"

ThisBuild / organization := "com.armanbilge"
ThisBuild / publishGithubUser := "armanbilge"
ThisBuild / publishFullName := "Arman Bilge"
ThisBuild / startYear := Some(2021)

enablePlugins(SonatypeCiReleasePlugin)
ThisBuild / spiewakCiReleaseSnapshots := true
ThisBuild / spiewakMainBranches := Seq("main")
ThisBuild / publishSnapshotsAsHashReleases := true
ThisBuild / homepage := Some(url("https://github.com/armanbilge/schrodinger"))
ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/armanbilge/schrodinger"),
    "git@github.com:armanbilge/schrodinger.git"))
sonatypeCredentialHost := "s01.oss.sonatype.org"

addCommandAlias("prePR", "; root/clean; +root/scalafmtAll; scalafmtSbt; +root/headerCreate")
replaceCommandAlias(
  "ci",
  "; project /; headerCheckAll; scalafmtCheckAll; scalafmtSbtCheck; clean; testIfRelevant; mimaReportBinaryIssuesIfRelevant"
)

val Scala3 = "3.1.1-RC2"
ThisBuild / crossScalaVersions := Seq(Scala3)

val CatsVersion = "2.7.0"
val CatsEffectVersion = "3.3.4"
val CommonsMathVersion = "3.6.1"
val CommonsRngVersion = "1.4"
val Fs2Version = "3.2.4"
val Specs2Version = "5.0.0-RC-22"
val Specs2CatsVersion = "1.0.0-ALPHA-05"
val ScalaCheckVersion = "1.15.4"
val VaultVersion = "3.1.0"
val DisciplineVersion = "1.1.5"
val DisciplineSpecs2Version = "2.0-44-19f6d7f"

val commonSettings = Seq(
  scalacOptions ++= Seq("-new-syntax", "-indent", "-source:future"),
  sonatypeCredentialHost := "s01.oss.sonatype.org"
)

val commonJvmSettings = Seq(
  Test / run / fork := true
)

lazy val root =
  project
    .in(file("."))
    .aggregate(
      kernel.jvm,
      kernel.js,
      math.jvm,
      math.js,
      kernelTestkit,
      laws.jvm,
      laws.js,
      random,
      core,
      testkit,
      tests,
      stats,
      monteCarlo)
    .enablePlugins(NoPublishPlugin)

lazy val kernel = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("kernel"))
  .settings(
    name := "schrodinger-kernel"
  )
  .settings(commonSettings)
  .jvmSettings(commonJvmSettings)

lazy val math = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("math"))
  .settings(
    name := "schrodinger-math",
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "algebra" % CatsVersion,
      "org.specs2" %%% "specs2-core" % Specs2Version % Test,
      "org.typelevel" %%% "discipline-specs2" % DisciplineSpecs2Version % Test,
      "org.typelevel" %%% "algebra-laws" % CatsVersion % Test
    )
  )
  .settings(commonSettings)
  .jvmSettings(commonJvmSettings)

lazy val kernelTestkit = project
  .in(file("kernel-testkit"))
  .dependsOn(kernel.jvm, math.jvm)
  .settings(
    name := "schrodinger-kernel-testkit",
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-core" % CatsVersion,
      "org.typelevel" %%% "cats-laws" % CatsVersion,
      "org.typelevel" %%% "cats-effect-kernel-testkit" % CatsEffectVersion,
      "org.typelevel" %%% "vault" % VaultVersion,
      "org.apache.commons" % "commons-math3" % CommonsMathVersion,
      "org.scalacheck" %%% "scalacheck" % ScalaCheckVersion,
      "org.specs2" %%% "specs2-scalacheck" % Specs2Version % Test,
      "org.typelevel" %%% "discipline-specs2" % DisciplineSpecs2Version % Test,
      "org.apache.commons" % "commons-rng-core" % CommonsRngVersion % Test
    )
  )
  .settings(commonSettings)
  .settings(commonJvmSettings)

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
  .settings(commonSettings)
  .jvmSettings(commonJvmSettings)

lazy val random = project
  .in(file("random"))
  .dependsOn(kernel.jvm, math.jvm, kernelTestkit % Test)
  .settings(
    name := "schrodinger-random",
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-core" % CatsVersion,
      "org.specs2" %%% "specs2-scalacheck" % Specs2Version % Test,
      "org.apache.commons" % "commons-rng-core" % CommonsRngVersion % Test,
      "org.apache.commons" % "commons-rng-sampling" % CommonsRngVersion % Test
    )
  )
  .settings(commonSettings)
  .settings(commonJvmSettings)

lazy val core = project
  .in(file("core"))
  .dependsOn(random)
  .settings(
    name := "schrodinger",
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-effect" % CatsEffectVersion,
      "org.specs2" %%% "specs2-scalacheck" % Specs2Version % Test,
      "org.apache.commons" % "commons-rng-core" % CommonsRngVersion % Test
    )
  )
  .settings(commonSettings)
  .settings(commonJvmSettings)

lazy val testkit = project
  .in(file("testkit"))
  .dependsOn(core, kernelTestkit)
  .settings(
    name := "schrodinger-testkit",
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-effect-testkit" % CatsEffectVersion
    )
  )
  .settings(commonSettings)
  .settings(commonJvmSettings)

lazy val tests = project
  .in(file("tests"))
  .enablePlugins(NoPublishPlugin)
  .dependsOn(testkit % Test, laws.jvm % Test)
  .settings(
    name := "schrodinger-tests",
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "discipline-specs2" % DisciplineSpecs2Version % Test,
      "org.typelevel" %%% "cats-effect-laws" % CatsEffectVersion % Test,
      "org.specs2" %%% "specs2-scalacheck" % Specs2Version % Test,
      "org.specs2" %%% "specs2-cats-effect" % Specs2CatsVersion % Test,
      "org.apache.commons" % "commons-rng-core" % CommonsRngVersion % Test,
      "org.apache.commons" % "commons-rng-sampling" % CommonsRngVersion % Test
    )
  )
  .settings(commonSettings)
  .settings(commonJvmSettings)

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
  .settings(commonSettings)
  .settings(commonJvmSettings)

lazy val monteCarlo = project
  .in(file("monte-carlo"))
  .dependsOn(kernel.jvm, math.jvm, testkit % Test)
  .settings(
    name := "schrodinger-monte-carlo",
    libraryDependencies ++= Seq(
      "co.fs2" %%% "fs2-core" % Fs2Version,
      "org.typelevel" %%% "algebra" % CatsVersion,
      "org.typelevel" %%% "cats-effect-std" % CatsEffectVersion,
      "org.typelevel" %%% "cats-laws" % CatsVersion % Test,
      "org.typelevel" %%% "cats-effect-laws" % CatsEffectVersion % Test,
      "org.typelevel" %%% "cats-effect-testkit" % CatsEffectVersion % Test,
      "org.typelevel" %%% "discipline-specs2" % DisciplineSpecs2Version % Test
    )
  )
  .settings(commonSettings)
  .settings(commonJvmSettings)
