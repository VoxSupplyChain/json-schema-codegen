package json.schema.codegen

import java.io.File

import json.schema.parser.JsonSchemaParser
import sbt.Keys._
import sbt.internal.util.ManagedLogger
import sbt.{Def, _}

object Plugin extends sbt.AutoPlugin {

  object autoImport {
    lazy val typeScriptDirectory = SettingKey[Option[File]](
      "type-script-directory",
      "Destination Folder for TypeScript generated code")
    lazy val scalaCodegen = TaskKey[Seq[File]](
      "scala-codegen",
      "Generate Scala code from Json-Schema")
    lazy val filter = TaskKey[String => Boolean](
      "filter",
      "Scala types to generate")
    lazy val typescriptCodegen = TaskKey[Seq[File]](
      "typescript-codegen",
      "Generate TypeScript code from Json-Schema")


    lazy val jsonGenSettings = Seq(
      // scala code gen
      sourceDirectory in scalaCodegen := sourceDirectory.value / "json-schema",
      filter in scalaCodegen := { _ => true },
      sources in scalaCodegen := {
        ((sourceDirectory in scalaCodegen).value ** GlobFilter("*.json")).get
      },
      watchSources in Defaults.ConfigGlobal ++= (sources in scalaCodegen).value,
      scalaCodegen := {
        val destinationDir = sourceManaged.value
        val s = streams.value
        val jsonFiles = (sources in scalaCodegen).value
        val filterFn = (filter in scalaCodegen).value
        val codegenLog = s.log("scala-codegen")
        runGen(s, destinationDir, new ScalaGenerator with SbtLog {
          val log = codegenLog
        }, filterFn, jsonFiles)
      },
      sourceGenerators += scalaCodegen.taskValue,

      // // typescript gen

      // TypeScript generation is disabled by default.
      typeScriptDirectory := None,
      filter in typescriptCodegen := { _ => true },

      sourceDirectory in typescriptCodegen := (sourceDirectory in typescriptCodegen).value,
      sources in typescriptCodegen := {
        ((sourceDirectory in typescriptCodegen).value ** GlobFilter("*.json")).get
      },
      typescriptCodegen := {
        val destinationDir = typeScriptDirectory.value
        val s = streams.value
        val jsonFiles = (sources in typescriptCodegen).value
        val filterFn = (filter in typescriptCodegen).value
        destinationDir.foreach(destinationDir =>
          runGen(s, destinationDir, new TypeScriptGenerator with SbtLog {
            val log = s.log("typescript-codegen")
          }, filterFn, jsonFiles)
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
                     filter: String => Boolean,
                     jsonFiles: Seq[sbt.File]): Seq[File] = {

    val cachedFun = FileFunction.cached(s.cacheDirectory / "json-schema",
      FilesInfo.lastModified, /* inStyle */
      FilesInfo.exists) /* outStyle */ {
      jsonSchemas: Set[File] =>
        val destinationPath = destinationDir.toPath

        generator.info(s"generating ${generator.generatedLanguage} code using ${jsonSchemas.size} schemas from: $destinationPath")

        val genFiles = for {
          schemas <- JsonSchemaParser.parseAll(jsonSchemas.toSeq)
          result <- generator(schemas)(filter, destinationPath)
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
