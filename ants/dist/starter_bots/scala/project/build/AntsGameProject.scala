import sbt._

class AntsGameProject(info: ProjectInfo) extends DefaultProject(info) {
  val specs2 = "org.specs2" %% "specs2" % "1.1-SNAPSHOT"

  def specs2Framework = new TestFramework("org.specs2.runner.SpecsFramework")

  override def testFrameworks = super.testFrameworks ++ Seq(specs2Framework)

  val snapshots = "snapshots" at "http://scala-tools.org/repo-snapshots"
  val releases = "releases" at "http://scala-tools.org/repo-releases"
}