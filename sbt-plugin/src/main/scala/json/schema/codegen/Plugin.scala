package json.schema.codegen

import java.io.File

import json.schema.parser.JsonSchemaParser
import sbt.Keys._
import sbt.{Def, _}

object Plugin extends sbt.AutoPlugin {

  object autoImport {
    lazy val typeScriptDirectory = SettingKey[Option[File]](
      "type-script-directory",
      "Destination Folder for TypeScript generated code")
    lazy val scalaCodegen = TaskKey[Seq[File]](
      "scala-codegen",
      "Generate Scala code from Json-Schema")
    lazy val typescriptCodegen = TaskKey[Seq[File]](
      "typescript-codegen",
      "Generate TypeScript code from Json-Schema")


    lazy val jsonGenSettings = Seq(
      // scala code gen
      sourceDirectory in scalaCodegen := sourceDirectory.value / "json-schema",
      sources in scalaCodegen := {
        ((sourceDirectory in scalaCodegen).value ** GlobFilter("*.json")).get
      },
      watchSources in Defaults.ConfigGlobal ++= (sources in scalaCodegen).value,
      scalaCodegen := {
        val destinationDir = sourceManaged.value
        val s = streams.value
        val jsonFiles = (sources in scalaCodegen).value
        runGen(s, destinationDir, new ScalaGenerator with SbtLog {
          val log = s.log("scala-codegen")
        }, jsonFiles)
      },
      sourceGenerators += scalaCodegen.taskValue,

      // // typescript gen

      // TypeScript generation is disabled by default.
      typeScriptDirectory := None,

      sourceDirectory in typescriptCodegen := (sourceDirectory in typescriptCodegen).value,
      sources in typescriptCodegen := {
        ((sourceDirectory in typescriptCodegen).value ** GlobFilter("*.json")).get
      },
      typescriptCodegen := {
        val destinationDir = typeScriptDirectory.value
        val s = streams.value
        val jsonFiles = (sources in typescriptCodegen).value
        destinationDir.foreach(destinationDir =>
          runGen(s, destinationDir, new TypeScriptGenerator with SbtLog {
            val log = s.log("typescript-codegen")
          }, jsonFiles)
        )
        Nil
      },
      resourceGenerators += typescriptCodegen.taskValue
    )
  }


  import autoImport._

  private trait SbtLog extends Logging {
    def log: sbt.Logger

    override def debug(s: => String): Unit = log.debug(s)

    override def info(s: => String): Unit = log.info(s)

    override def error(s: => String): Unit = log.error(s)
  }

  private def runGen(s: TaskStreams,
                     destinationDir: File,
                     generator: CodeGenerator,
                     jsonFiles: Seq[sbt.File]): Seq[File] = {

    val cachedFun = FileFunction.cached(s.cacheDirectory / "json-schema",
      FilesInfo.lastModified, /* inStyle */
      FilesInfo.exists) /* outStyle */ {
      (jsonSchemas: Set[File]) =>
        val destinationPath = destinationDir.toPath

        generator.info(
          s"${generator.getClass} generating code using $jsonSchemas in $destinationPath")

        val genFiles = for {
          schemas <- JsonSchemaParser.parseAll(jsonSchemas.toSeq)
          result <- generator(schemas)(destinationPath)
        } yield result

        genFiles
          .fold(
            e =>
              throw new IllegalArgumentException(
                s"Failed code generation in $jsonSchemas: $e "),
            p => p.map(_.toFile)
          )
          .toSet

    }

    if (jsonFiles == null || jsonFiles.isEmpty)
      generator.info(s"found no JSON-schema files for generating code")

    cachedFun(jsonFiles.toSet).toSeq
  }

  lazy override val projectSettings =
    inConfig(Compile)(jsonGenSettings)

}
