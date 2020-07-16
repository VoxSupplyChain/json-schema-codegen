name := "json-schema-codegen-examples"

enablePlugins(json.schema.codegen.Plugin)

libraryDependencies ++= Seq(
  "io.argonaut" %% "argonaut" % "6.2.2",
  "com.github.alexarchambault" %% "argonaut-shapeless_6.2" % "1.2.0-M4",
  "org.scalatest" %% "scalatest" % "3.0.0" % "test"
)

fullResolvers in ThisBuild := Seq(
  "Tundra Nexus" at "https://nexus.tundra-shared.com/repository/tundra/"
)

typeScriptDirectory in Compile := Some(target.value / "typescript")
