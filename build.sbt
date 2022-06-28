ThisBuild / tlBaseVersion := "0.4"

ThisBuild / organization := "com.armanbilge"
ThisBuild / organizationName := "Arman Bilge"
ThisBuild / developers += tlGitHubDev("armanbilge", "Arman Bilge")
ThisBuild / startYear := Some(2021)

ThisBuild / tlUntaggedAreSnapshots := false
ThisBuild / tlSonatypeUseLegacyHost := false

val Scala3 = "3.1.3"
ThisBuild / crossScalaVersions := Seq(Scala3)

val CatsVersion = "2.8.0"
val CatsEffectVersion = "3.3.13"
val CommonsMathVersion = "3.6.1"
val CommonsRngVersion = "1.4"
val Fs2Version = "3.2.8"
val Specs2Version = "5.0.1"
val Specs2CatsVersion = "1.0.0-ALPHA-07"
val ScalaCheckVersion = "1.16.0"
val VaultVersion = "3.2.1"
val DisciplineVersion = "1.1.5"
val DisciplineSpecs2Version = "2.0.0"

ThisBuild / scalacOptions ++= Seq("-new-syntax", "-indent", "-source:future")

val commonJvmSettings = Seq(
  Test / run / fork := true
)

lazy val root = tlCrossRootProject.aggregate(
  math,
  kernel,
  kernelTestkit,
  laws,
  core,
  testkit,
  tests,
  stats,
  monteCarlo)

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
  .jvmSettings(commonJvmSettings)

lazy val kernel = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("kernel"))
  .dependsOn(math)
  .settings(
    name := "schrodinger-kernel",
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-core" % CatsVersion
    )
  )
  .jvmSettings(commonJvmSettings)

lazy val stats = project
  .in(file("stats"))
  .dependsOn(kernel.jvm, math.jvm)
  .settings(
    name := "schrodinger-stats",
    libraryDependencies ++= Seq(
      "org.apache.commons" % "commons-math3" % CommonsMathVersion,
      "org.typelevel" %%% "cats-laws" % CatsVersion % Test,
      "org.specs2" %%% "specs2-scalacheck" % Specs2Version % Test,
      "org.typelevel" %%% "discipline-specs2" % DisciplineSpecs2Version % Test
    )
  )
  .settings(commonJvmSettings)

lazy val kernelTestkit = project
  .in(file("kernel-testkit"))
  .dependsOn(kernel.jvm, stats)
  .settings(
    name := "schrodinger-kernel-testkit",
    libraryDependencies ++= Seq(
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
  .settings(commonJvmSettings)

lazy val laws = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("laws"))
  .dependsOn(kernel, math)
  .settings(
    name := "schrodinger-laws",
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-laws" % CatsVersion,
      "org.typelevel" %%% "algebra-laws" % CatsVersion
    )
  )
  .jvmSettings(commonJvmSettings)

lazy val core = project
  .in(file("core"))
  .dependsOn(kernel.jvm)
  .settings(
    name := "schrodinger",
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-effect" % CatsEffectVersion,
      "org.specs2" %%% "specs2-scalacheck" % Specs2Version % Test,
      "org.apache.commons" % "commons-rng-core" % CommonsRngVersion % Test
    )
  )
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
  .settings(commonJvmSettings)

lazy val tests = crossProject(JVMPlatform, JSPlatform)
  .in(file("tests"))
  .enablePlugins(NoPublishPlugin)
  .dependsOn(laws % Test)
  .settings(
    name := "schrodinger-tests",
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "discipline-specs2" % DisciplineSpecs2Version % Test,
      "org.typelevel" %%% "cats-effect-laws" % CatsEffectVersion % Test,
      "org.specs2" %%% "specs2-scalacheck" % Specs2Version % Test,
      "org.specs2" %%% "specs2-cats-effect" % Specs2CatsVersion % Test
    )
  )
  .jvmConfigure(_.dependsOn(testkit % Test))
  .jvmSettings(
    libraryDependencies ++= Seq(
      "org.apache.commons" % "commons-rng-core" % CommonsRngVersion % Test,
      "org.apache.commons" % "commons-rng-sampling" % CommonsRngVersion % Test
    )
  )
  .jvmSettings(commonJvmSettings)

lazy val monteCarlo = project
  .in(file("monte-carlo"))
  .dependsOn(kernel.jvm, stats, testkit % Test)
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
  .settings(commonJvmSettings)
