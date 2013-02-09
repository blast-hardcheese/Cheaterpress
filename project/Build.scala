import sbt._
import Keys._

object MinimalBuild extends Build with ConfigureScalaBuild{
  lazy val root = scalaMiniProject("se.hardchee","Cheaterpress","1.1").settings(
    libraryDependencies ++= Seq(
      "com.github.scopt" %% "scopt" % "2.1.0"
    )
  )
}

trait ConfigureScalaBuild {
  lazy val sonatype = "sonatype-public" at "https://oss.sonatype.org/content/groups/public"

  def scalaMiniProject(org: String, name: String, buildVersion: String, baseFile: java.io.File = file(".")) =
    Project(id = name, base = baseFile, settings = Project.defaultSettings).settings(
      version := buildVersion,
      organization := org,
      resolvers += sonatype
    )
}
