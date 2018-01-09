package json.schema.codegen

import java.io.{File, FilenameFilter}
import java.nio.file.Path

import json.schema.parser.JsonSchemaParser

import scalaz.std.AllInstances._
import scalaz.syntax.all._
import scalaz.syntax.std.all._
import scala.collection.convert.WrapAsScala._
import json.source.JsonSource._

abstract class GeneratorCommand(codegens: List[CodeGenerator]) {

  val jsonFilesFilter = new FilenameFilter {
    override def accept(dir: File, name: String): Boolean = name.endsWith(".json")
  }

  def main(args: Array[String]) {

    val oargs = args.lift
    val parser = new JsonSchemaParser[Double]
    val result = for {
      source <- oargs(0).map(new File(_)).toRightDisjunction("json-schema is required")
      targetDir <- oargs(1).map(new File(_)).toRightDisjunction("target folder is required")
      genRoot: Path = targetDir.toPath
      sources = if (source.isDirectory) source.listFiles(jsonFilesFilter).toSeq
      else Seq(source)

      schemas <- parser.parseAll(sources)
      results <- codegens.map(gen => gen(schemas)(genRoot)).sequenceU
    } yield results

    result.fold({ e =>
      sys.error(s"Code generation failed with: $e")
      System.exit(1)
    }, _ => System.exit(0))

  }

}