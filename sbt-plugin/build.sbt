sbtPlugin := true

scalaVersion := "2.12.4"

sbtVersion in Global := "1.0.4"

scalaCompilerBridgeSource := {
  val sv = appConfiguration.value.provider.id.version
  ("org.scala-sbt" % "compiler-interface" % sv % "component").sources
}

crossSbtVersions := Vector("0.13.16", "1.0.0")