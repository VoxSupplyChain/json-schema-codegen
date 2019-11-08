
val commonSettings = Seq(

  organization := "com.voxsupplychain",

  scalaVersion := "2.12.4",


  publishMavenStyle := true,

  publishTo := {
    val nexus = "https://my.artifact.repo.net/"
    if (isSnapshot.value)
      Some("snapshots"  at "https://nexus.tundra-shared.com/repository/maven-snapshots/")
    else
      Some("releases" at "https://nexus.tundra-shared.com/repository/maven-releases/")
  },

  resolvers += "releases" at "https://nexus.tundra-shared.com/repository/maven-releases/",

  credentials += Credentials(Path.userHome / ".ivy2" / ".credentials"),

  licenses += ("Apache-2.0" -> new URL("http://www.apache.org/licenses/LICENSE-2.0"))

)

lazy val codegen = Project(id = "json-schema-codegen", base = file("codegen"))
  .settings(
    commonSettings
  )

lazy val sbtplugin: Project =
  Project(id = "json-schema-codegen-sbt", base = file("sbt-plugin"))
    .dependsOn(codegen)
    .settings(
      commonSettings
    )

lazy val root = Project(id = "json-schema-codegen-root", base = file("."))
  .aggregate(codegen, sbtplugin)
  .settings(
    commonSettings
  )
