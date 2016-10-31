import sbt._
import sbt.Keys._
import bintray.Opts
import bintray.Plugin._
import bintray.Keys._
import sbtrelease.ReleasePlugin._


object Build extends Build {

  lazy val projectSettings = Seq(

    organization := "com.voxsupplychain",

    scalaVersion := "2.10.5",

    resolvers += Resolver.url(
      "vox-public-ivy",
      url("http://dl.bintray.com/content/voxsupplychain/ivy-public"))(
      Resolver.ivyStylePatterns),

    publishMavenStyle := false,

    licenses += ("Apache-2.0" -> new URL("http://www.apache.org/licenses/LICENSE-2.0")),

    repository in bintray := "ivy-public",

    packageLabels in bintray := Seq("json-schema", "code", "generator"),

    bintrayOrganization in bintray := Some("voxsupplychain")

  ) ++ bintraySettings ++ releaseSettings

  lazy val root = Project(id = "json-schema-codegen-root", base = file(".")).aggregate(codegen, sbtplugin).settings(projectSettings: _*)

  lazy val codegen = Project(id = "json-schema-codegen", base = file("codegen"))
    .settings(
      libraryDependencies ++= Seq(
        "com.voxsupplychain" %% "json-schema-parser" % "0.10.0",
        "org.scalatest" % "scalatest_2.10" % "2.2.1" % "test",
        "org.scalacheck" %% "scalacheck" % "1.12.0" % "test"
      )
    )
    .settings(projectSettings: _*)


  lazy val sbtplugin: Project = Project(id = "json-schema-codegen-sbt",
    base = file("sbt-plugin")).dependsOn(codegen).settings(
    sbtPlugin := true
  )
    .settings(projectSettings: _*)

}
