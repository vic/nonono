// -*- mode: scala -*-

import mill._, os._, scalalib._, publish._
import scala.util.Properties

object meta {

  val crossVersions = Seq("3.1.1")

  implicit val wd: Path = pwd

  def nonEmpty(s: String): Option[String] = s.trim match {
    case v if v.isEmpty => None
    case v              => Some(v)
  }

  val MILL_VERSION = Properties.propOrNull("MILL_VERSION")
  val versionFromEnv = Properties.propOrNone("PUBLISH_VERSION")
  val gitSha = nonEmpty(
    proc("git", "rev-parse", "--short", "HEAD").call().out.trim
  )
  val gitTag = nonEmpty(
    proc("git", "tag", "-l", "-n0", "--points-at", "HEAD").call().out.trim
  )
  val publishVersion =
    (versionFromEnv orElse gitTag orElse gitSha).getOrElse("latest")

  def pomSettings = PomSettings(
    description = "Scala plugin that forbids some method invocations.",
    organization = "com.github.vic",
    url = "https://github.com/vic/nonono",
    licenses = Seq(License.`Apache-2.0`),
    versionControl = VersionControl.github("vic", "nonono"),
    developers = Seq(
      Developer("vic", "Victor Borja", "https://github.com/vic")
    )
  )

}

object `nonono-lib` extends Cross[NoNoNoLib](meta.crossVersions: _*)
class NoNoNoLib(val crossScalaVersion: String)
    extends CrossScalaModule
    with PublishModule { self =>
  def publishVersion = meta.publishVersion
  override def artifactName = "nonono-lib"
  override def pomSettings: T[PomSettings] = meta.pomSettings
}

object `nonono-plugin` extends Cross[NoNoNoPlugin](meta.crossVersions: _*)
class NoNoNoPlugin(val crossScalaVersion: String)
    extends CrossScalaModule
    with PublishModule { self =>
  def publishVersion = meta.publishVersion

  override def artifactName = "nonono-plugin"
  override def pomSettings: T[PomSettings] = meta.pomSettings

  override def moduleDeps = super.moduleDeps ++ Seq(`nonono-lib`())

  override def compileIvyDeps = T {
    super.compileIvyDeps() ++ Seq(
      ivy"org.scala-lang::scala3-compiler:3.1.1"
    )
  }

  object tests extends Tests with TestModule.Utest {
    override def ivyDeps = T {
      Agg(ivy"com.lihaoyi::utest:0.7.11") ++ self.compileIvyDeps()
    }

    override def scalacPluginClasspath = T {
      super.scalacPluginClasspath() ++ Seq(self.jar())
    }
  }
}
