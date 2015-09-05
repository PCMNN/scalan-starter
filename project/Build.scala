import sbt._
import sbt.Keys._

object Build extends Build {
  val commonDeps = libraryDependencies ++= Seq(
    //"org.scalaz.stream" %% "scalaz-stream" % "0.6a",
    //"junit" % "junit" % "4.11" % "test",
    //("com.novocode" % "junit-interface" % "0.11" % "test").exclude("junit", "junit-dep").exclude("org.scala-tools.testing", "test-interface"),
    "org.scalatest" %% "scalatest" % "2.2.1" % "test",
    "org.scalacheck" %% "scalacheck" % "1.11.5" % "test")

  val testSettings = inConfig(ItTest)(Defaults.testTasks /*++ baseAssemblySettings*/) ++ Seq(
    testOptions in Test := Seq(Tests.Argument(TestFrameworks.JUnit, "-a", "-s"), Tests.Filter(unitFilter)),
    testOptions in ItTest := Seq(Tests.Argument(TestFrameworks.JUnit, "-v", "-a", "-s", "-q"), Tests.Filter(itFilter)),
    // needed thanks to http://stackoverflow.com/questions/7898273/how-to-get-logging-working-in-scala-unit-tests-with-testng-slf4s-and-logback
    parallelExecution in Test := false,
    parallelExecution in ItTest := false,
    publishArtifact in Test := true,
    publishArtifact in(Test, packageDoc) := false
    //jarName in(ItTest, assembly) := s"${name.value}-test-${version.value}.jar"
    )

  val buildSettings = Seq(
    organization := "com.huawei.scalan",
    scalaVersion := "2.11.7",
    scalacOptions ++= Seq(
      "-unchecked", "-deprecation",
      "-feature",
      "-language:higherKinds",
      "-language:implicitConversions",
      "-language:existentials",
      "-language:postfixOps"))

  lazy val noPublishingSettings = Seq(
    publishArtifact := false,
    publish := {},
    publishLocal := {})

  override lazy val settings = super.settings ++ buildSettings

  lazy val commonSettings =
    buildSettings /*++ assemblySettings ++ releaseSettings*/ ++ testSettings ++
      Seq(
      //resolvers += "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases",
      publishTo := {
        val nexus = "http://10.122.85.37:9081/nexus/"
        if (version.value.trim.endsWith("SNAPSHOT"))
          Some("snapshots" at (nexus + "content/repositories/snapshots"))
        else
          Some("releases" at (nexus + "content/repositories/releases"))
      },
      commonDeps)

  implicit class ProjectExt(p: Project) {
    def allConfigDependency = p % "compile->compile;test->test"

    def addTestConfigsAndCommonSettings =
      p.configs(ItTest).settings(commonSettings: _*)
  }

  def liteProject(name: String) = ProjectRef(file("../scalan-ce"), name)

  def liteDependency(name: String) = "com.huawei.scalan" %% name % "0.2.11-SNAPSHOT"

  lazy val metaDeps = liteDependency("scalan-meta")
  lazy val meta = Project(
    id = "starter-meta",
    base = file("meta")).addTestConfigsAndCommonSettings.
    settings(libraryDependencies ++= Seq(metaDeps))

  lazy val core = liteDependency("scalan-core")
  lazy val common = liteDependency("scalan-common")
  lazy val community = liteDependency("scalan-library")
  lazy val ml_study = Project(
    id = "scalan-starter",
    base = file(".")).addTestConfigsAndCommonSettings.
    settings(libraryDependencies ++= Seq(core, common, common % "test" classifier "tests",
                                         community, core % "test" classifier "tests"))

  def itFilter(name: String): Boolean =
    name endsWith "ItTests"

  def unitFilter(name: String): Boolean = !itFilter(name)

  lazy val ItTest = config("it").extend(Test)

  publishArtifact in Test := true

  publishArtifact in (Test, packageDoc) := false

  publishTo in ThisBuild := {
    val nexus = "http://10.122.85.37:9081/nexus/"
    if (version.value.trim.endsWith("SNAPSHOT"))
      Some("snapshots" at (nexus + "content/repositories/snapshots"))
    else
      Some("releases" at (nexus + "content/repositories/releases"))
  }
}
