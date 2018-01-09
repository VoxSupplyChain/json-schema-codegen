name := "json-schema-codegen-examples"

enablePlugins(json.schema.codegen.Plugin)

libraryDependencies ++= Seq(
  "io.argonaut" %% "argonaut" % "6.2.1",
  "org.scalatest" %% "scalatest" % "2.2.1" % "test"
)

typeScriptDirectory in Compile := Some(target.value / "typescript")