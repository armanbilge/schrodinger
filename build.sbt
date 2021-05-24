ThisBuild / baseVersion := "0.3"

ThisBuild / organization := "com.armanbilge"
ThisBuild / publishGithubUser := "armanbilge"
ThisBuild / publishFullName := "Arman Bilge"
ThisBuild / startYear := Some(2021)

mimaPreviousArtifacts := Set()

enablePlugins(SonatypeCiReleasePlugin)
ThisBuild / spiewakCiReleaseSnapshots := true
git.formattedShaVersion ~= {
  _.map { v => if (!v.endsWith("-SNAPSHOT")) v + "-SNAPSHOT" else v }
}
ThisBuild / spiewakMainBranches := Seq("main")
ThisBuild / homepage := Some(url("https://github.com/armanbilge/schrodinger"))
ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/armanbilge/schrodinger"),
    "git@github.com:armanbilge/schrodinger.git"))
sonatypeCredentialHost := "s01.oss.sonatype.org"
lazy val sonatypeBundleReleaseIfRelevant =
  taskKey[Unit]("Executes sonatypeBundleRelease if not a snapshot")
sonatypeBundleReleaseIfRelevant := Def.taskDyn[Unit] {
  if (!isSnapshot.value)
    Def.task(sonatypeBundleRelease.value)
  else
    Def.task(())
}

replaceCommandAlias(
  "ci",
  "; project /; headerCheckAll; scalafmtCheckAll; scalafmtSbtCheck; clean; testIfRelevant; mimaReportBinaryIssuesIfRelevant"
)
replaceCommandAlias(
  "release",
  "; reload; project /; +mimaReportBinaryIssuesIfRelevant; +publishIfRelevant; sonatypeBundleReleaseIfRelevant"
)
addCommandAlias("prePR", "; root/clean; +root/scalafmtAll; scalafmtSbt; +root/headerCreate")

val Scala213 = "2.13.6"
val Scala3 = "3.0.0"
ThisBuild / crossScalaVersions := Seq(Scala3, Scala213)

val CatsVersion = "2.6.1"
val CatsEffectVersion = "3.1.1"
val CommonsRngVersion = "1.3"
val Fs2Version = "3.0.4"
val LitterVersion = "0.1.1"
val Specs2Version = "4.11.0"
val ScalaCheckVersion = "1.15.3"
val DisciplineVersion = "1.1.6"

lazy val root =
  project.aggregate(kernel, random, core, monteCarlo, testkit, tests, example).enablePlugins(NoPublishPlugin)

lazy val kernel = project
  .in(file("kernel"))
  .settings(
    name := "schrodinger-kernel",
    sonatypeCredentialHost := "s01.oss.sonatype.org"
  )

lazy val laws = project
  .in(file("laws"))
  .dependsOn(kernel)
  .settings(
    name := "schrodinger-laws",
    sonatypeCredentialHost := "s01.oss.sonatype.org",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-laws" % CatsVersion,
      "org.typelevel" %% "discipline-specs2" % DisciplineVersion
    )
  )

lazy val random = project
  .in(file("random"))
  .dependsOn(kernel)
  .settings(
    name := "schrodinger-random",
    sonatypeCredentialHost := "s01.oss.sonatype.org",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % CatsVersion
    )
  )

lazy val core = project
  .in(file("core"))
  .dependsOn(random)
  .settings(
    name := "schrodinger",
    sonatypeCredentialHost := "s01.oss.sonatype.org",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-effect-kernel" % CatsEffectVersion,
      "org.specs2" %% "specs2-core" % Specs2Version % Test cross CrossVersion.for3Use2_13,
      "org.specs2" %% "specs2-scalacheck" % Specs2Version % Test cross CrossVersion.for3Use2_13,
      "org.apache.commons" % "commons-rng-core" % CommonsRngVersion % Test
    )
  )

lazy val testkit = project
  .in(file("testkit"))
  .dependsOn(core, random)
  .settings(
    name := "schrodinger-testkit",
    sonatypeCredentialHost := "s01.oss.sonatype.org",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-laws" % CatsVersion,
      "org.apache.commons" % "commons-math3" % "3.6.1",
      "org.specs2" %% "specs2-core" % Specs2Version % Test cross CrossVersion.for3Use2_13,
      "org.specs2" %% "specs2-scalacheck" % Specs2Version % Test cross CrossVersion.for3Use2_13
    )
  )

lazy val tests = project
  .in(file("tests"))
  .dependsOn(testkit % Test, laws % Test)
  .settings(
    name := "schrodinger-tests",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "discipline-specs2" % DisciplineVersion % Test,
      "org.typelevel" %% "cats-effect-laws" % CatsEffectVersion % Test,
      "org.typelevel" %% "cats-effect-testkit" % CatsEffectVersion % Test,
      "org.apache.commons" % "commons-rng-core" % CommonsRngVersion % Test,
      "org.apache.commons" % "commons-rng-sampling" % CommonsRngVersion % Test
    )
  )
  .enablePlugins(NoPublishPlugin)

lazy val monteCarlo = project
  .in(file("monte-carlo"))
  .dependsOn(random, testkit % Test)
  .settings(
    name := "schrodinger-monte-carlo",
    sonatypeCredentialHost := "s01.oss.sonatype.org",
    libraryDependencies ++= Seq(
      "co.fs2" %% "fs2-core" % Fs2Version,
      "com.armanbilge" %% "litter" % LitterVersion,
      "org.typelevel" %% "cats-effect-std" % CatsEffectVersion,
      "org.typelevel" %% "cats-laws" % CatsVersion % Test,
      "org.typelevel" %% "cats-effect-laws" % CatsEffectVersion % Test,
      "org.typelevel" %% "cats-effect-testkit" % CatsEffectVersion % Test,
      "org.typelevel" %% "discipline-specs2" % DisciplineVersion % Test
    )
  )

lazy val example = project
  .in(file("example"))
  .dependsOn(core)
  .settings(
    name := "schrodinger-example",
    libraryDependencies += "org.typelevel" %% "cats-effect" % CatsEffectVersion
  )
  .enablePlugins(NoPublishPlugin)
