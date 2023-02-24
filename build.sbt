ThisBuild / tlBaseVersion := "0.4"

ThisBuild / organization := "com.armanbilge"
ThisBuild / organizationName := "Arman Bilge"
ThisBuild / developers += tlGitHubDev("armanbilge", "Arman Bilge")
ThisBuild / startYear := Some(2021)

ThisBuild / tlUntaggedAreSnapshots := false
ThisBuild / tlSonatypeUseLegacyHost := false

val Scala3 = "3.2.2"
ThisBuild / crossScalaVersions := Seq(Scala3)

ThisBuild / githubWorkflowJavaVersions := Seq(JavaSpec.temurin("17"))
ThisBuild / tlJdkRelease := Some(17)

val CatsVersion = "2.9.0"
val CatsEffectVersion = "3.4.7"
val CatsCollectionsVersion = "0.9.6"
val CommonsNumbersGamma = "1.1"
val CommonsRngVersion = "1.5"
val Fs2Version = "3.6.1"
val ScodecBitsVersion = "1.1.35"
val ScalaCheckVersion = "1.17.0"
val ScalaCheckEffectVersion = "2.0.0-M2"
val VaultVersion = "3.5.0"
val DisciplineVersion = "1.1.5"
val MunitVersion = "1.0.0-M7"
val DisciplineMunitVersion = "2.0.0-M3"
val MunitCatsEffectVersion = "2.0.0-M3"

ThisBuild / scalacOptions ++= Seq("-new-syntax", "-indent", "-source:future")
ThisBuild / Test / testOptions += Tests.Argument("+l")

val commonJvmSettings = Seq(
  fork := true,
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
  monteCarlo,
)

lazy val math = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .in(file("math"))
  .settings(
    name := "schrodinger-math",
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "algebra" % CatsVersion,
      "org.typelevel" %%% "algebra-laws" % CatsVersion % Test,
      "org.scalameta" %%% "munit-scalacheck" % MunitVersion % Test,
      "org.typelevel" %%% "discipline-munit" % DisciplineMunitVersion % Test,
    ),
  )
  .jvmSettings(commonJvmSettings)
  .jvmSettings(
    libraryDependencies ++= Seq(
      "org.apache.commons" % "commons-numbers-gamma" % CommonsNumbersGamma,
    ),
  )

lazy val kernel = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("kernel"))
  .dependsOn(math)
  .settings(
    name := "schrodinger-kernel",
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-core" % CatsVersion,
      "org.typelevel" %%% "cats-collections-core" % CatsCollectionsVersion,
    ),
  )
  .jvmSettings(commonJvmSettings)

lazy val stats = crossProject(JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("stats"))
  .dependsOn(kernel, math)
  .settings(
    name := "schrodinger-stats",
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-laws" % CatsVersion % Test,
      "org.scalameta" %%% "munit-scalacheck" % MunitVersion % Test,
      "org.typelevel" %%% "discipline-munit" % DisciplineMunitVersion % Test,
    ),
  )
  .jvmSettings(commonJvmSettings)

lazy val kernelTestkit = crossProject(JVMPlatform, NativePlatform)
  .in(file("kernel-testkit"))
  .dependsOn(kernel, stats)
  .settings(
    name := "schrodinger-kernel-testkit",
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-laws" % CatsVersion,
      "org.typelevel" %%% "cats-effect-kernel-testkit" % CatsEffectVersion,
      "org.typelevel" %%% "vault" % VaultVersion,
      "org.scalacheck" %%% "scalacheck" % ScalaCheckVersion,
      "org.scalameta" %%% "munit-scalacheck" % MunitVersion % Test,
      "org.typelevel" %%% "discipline-munit" % DisciplineMunitVersion % Test,
    ),
  )
  .jvmSettings(commonJvmSettings)
  .jvmSettings(
    libraryDependencies ++= Seq(
      "org.apache.commons" % "commons-rng-core" % CommonsRngVersion % Test,
    ),
  )

lazy val laws = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("laws"))
  .dependsOn(kernel, math)
  .settings(
    name := "schrodinger-laws",
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-laws" % CatsVersion,
      "org.typelevel" %%% "algebra-laws" % CatsVersion,
    ),
  )
  .jvmSettings(commonJvmSettings)

lazy val core = crossProject(JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("core"))
  .dependsOn(kernel)
  .settings(
    name := "schrodinger",
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-effect" % CatsEffectVersion,
      "org.scodec" %%% "scodec-bits" % ScodecBitsVersion % Test,
      "org.scalameta" %%% "munit-scalacheck" % MunitVersion % Test,
      "org.typelevel" %%% "munit-cats-effect" % MunitCatsEffectVersion % Test,
      "org.typelevel" %%% "scalacheck-effect-munit" % ScalaCheckEffectVersion % Test,
    ),
  )
  .jvmSettings(commonJvmSettings)
  .jvmSettings(
    libraryDependencies ++= Seq(
      "org.apache.commons" % "commons-rng-core" % CommonsRngVersion % Test,
    ),
  )

lazy val testkit = crossProject(JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("testkit"))
  .dependsOn(core, kernelTestkit)
  .settings(
    name := "schrodinger-testkit",
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-effect-testkit" % CatsEffectVersion,
    ),
  )
  .jvmSettings(commonJvmSettings)

lazy val tests = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .in(file("tests"))
  .enablePlugins(NoPublishPlugin)
  .dependsOn(laws % Test)
  .settings(
    name := "schrodinger-tests",
    libraryDependencies ++= Seq(
      "org.scalameta" %%% "munit-scalacheck" % MunitVersion % Test,
      "org.typelevel" %%% "discipline-munit" % DisciplineMunitVersion % Test,
      "org.typelevel" %%% "munit-cats-effect" % MunitCatsEffectVersion % Test,
      "org.typelevel" %%% "cats-effect-laws" % CatsEffectVersion % Test,
      "org.typelevel" %%% "scalacheck-effect-munit" % ScalaCheckEffectVersion % Test,
    ),
  )
  .jvmConfigure(_.dependsOn(testkit.jvm % Test))
  .jvmSettings(
    libraryDependencies ++= Seq(
      "org.apache.commons" % "commons-rng-core" % CommonsRngVersion % Test,
      "org.apache.commons" % "commons-rng-sampling" % CommonsRngVersion % Test,
    ),
  )
  .jvmSettings(commonJvmSettings)
  .nativeConfigure(_.dependsOn(testkit.native % Test))

lazy val monteCarlo = crossProject(JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("monte-carlo"))
  .dependsOn(kernel, stats, testkit % Test)
  .settings(
    name := "schrodinger-monte-carlo",
    libraryDependencies ++= Seq(
      "co.fs2" %%% "fs2-core" % Fs2Version,
      "org.typelevel" %%% "algebra" % CatsVersion,
      "org.typelevel" %%% "cats-effect-std" % CatsEffectVersion,
      "org.typelevel" %%% "cats-laws" % CatsVersion % Test,
      "org.typelevel" %%% "cats-effect-laws" % CatsEffectVersion % Test,
      "org.typelevel" %%% "cats-effect-testkit" % CatsEffectVersion % Test,
      "org.typelevel" %%% "discipline-munit" % DisciplineMunitVersion % Test,
      "org.typelevel" %%% "munit-cats-effect" % MunitCatsEffectVersion % Test,
    ),
  )
  .jvmSettings(commonJvmSettings)
