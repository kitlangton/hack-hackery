ThisBuild / scalaVersion     := "2.13.10"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"

val zioVersion = "2.0.6"
lazy val root = (project in file("."))
  .settings(
    name := "llvm-tutorial",
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio"        % zioVersion,
      "dev.zio" %% "zio-test"   % zioVersion % Test,
      "dev.zio" %% "zio-parser" % "0.1.8"
    ),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"),
    scalacOptions ++= Seq("-Xfatal-warnings")
  )
