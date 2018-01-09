package json.schema

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{StandardOpenOption, Files, Path}

import json.schema.parser.{JsonSchemaParser, SchemaDocument}
import json.source.JsonSource

import scala.util.Try
import scalaz.{\/, Leibniz}
import scalaz.Leibniz._
import scalaz.Scalaz._

package object codegen {

  type SValidation[T] = String \/ T

  sealed trait LangType {
    // types referenced by this type
    val referenced: Set[LangType]
    // name space of this type
    val scope: String
    // language specific valid identifier
    val identifier: String
  }

  sealed case class PredefType(scope: String, identifier: String) extends LangType {
    override val referenced: Set[LangType] = Set.empty
  }

  sealed case class ArrayType(scope: String, unique: Boolean, nested: LangType) extends LangType {
    override val identifier = nested.identifier
    override lazy val referenced: Set[LangType] = nested.referenced + nested
  }

  sealed case class ClassType(scope: String, identifier: String, properties: List[LangTypeProperty], additionalNested: Option[LangType]) extends LangType {
    override lazy val referenced: Set[LangType] = properties.map(t => t.isa.referenced + t.isa).toSet.flatten ++ additionalNested.map(t => t.referenced + t).getOrElse(Set.empty)
  }

  sealed case class EnumType(scope: String, identifier: String, nested: LangType, enums: Set[_]) extends LangType {
    override lazy val referenced: Set[LangType] = nested.referenced + nested
  }

  case class LangTypeProperty(name: String, required: Boolean, isa: LangType)


  implicit class ParserWrapper[N: Numeric](jsonParser: JsonSchemaParser[N]) {

    def parseAll[T: JsonSource](sources: Seq[T]): SValidation[List[SchemaDocument[N]]] = {
      implicit val evdoc: ===[SValidation[SchemaDocument[N]], SValidation[SchemaDocument[N]]] = Leibniz.refl
      sources.map(source => jsonParser.parse(source)).toList.sequenceU
    }
  }

  trait Logging {

    def info(s: => String)

    def debug(s: => String)

    def error(s: => String)

    protected implicit class Printable[T](v: T) {
      def withInfo(prefix: String = ""): T = {
        info(s"$prefix : $v")
        v
      }

      def withError(prefix: String = ""): T = {
        error(s"$prefix : $v")
        v
      }

      def withDebug(prefix: String = ""): T = {
        debug(s"$prefix : $v")
        v
      }
    }

  }

  trait ConsoleLogging extends Logging {

    override def info(s: => String) = System.out.println(s)

    override def debug(s: => String) = System.out.println(s)

    override def error(s: => String) = System.err.println(s)

  }

  trait CodeGenerator extends Naming with Logging {

    protected def generateFile(codePackage: String, fileName: String, outputDir: Path)(content: Option[String] => SValidation[String]): SValidation[List[Path]] = {

      Try {
        content(codePackage.some.noneIfEmpty) map {
          fileContent =>

            if (fileContent.trim.isEmpty)
              Nil
            else {

              val packageDir = codePackage.replaceAll("\\.", File.separator)

              // create package structure
              val fileDir: Path = outputDir.resolve(packageDir)

              if (!fileDir.toFile.exists())
                Files.createDirectories(fileDir)

              val generateAbsoluteFile: Path = fileDir.resolve(fileName)
              Files.deleteIfExists(generateAbsoluteFile)
              List(
                Files.write(generateAbsoluteFile, fileContent.getBytes(StandardCharsets.UTF_8), StandardOpenOption.CREATE_NEW)
              )

            }
        }
      }.recover {
        case e: Throwable => e.toString.left
      }.get

    }


    private def packageModels(models: Set[LangType]): Map[String, Set[LangType]] = models.groupBy(_.scope)

    protected implicit val ev: ===[SValidation[List[Path]], SValidation[List[Path]]] = Leibniz.refl
    implicit val evset: ===[SValidation[Set[LangType]], SValidation[Set[LangType]]] = Leibniz.refl

    def apply[N: Numeric](schemas: List[SchemaDocument[N]])(codeGenTarget: Path): SValidation[List[Path]] = {

      implicit val evdoc: ===[SValidation[SchemaDocument[N]], SValidation[SchemaDocument[N]]] = Leibniz.refl

      for {
        models <- schemas.map(schema => languageModel(schema).withDebug("generated object model")).sequenceU
        modelsByPackage: Map[String, Set[LangType]] = packageModels(models.flatMap(_.toList).toSet)
        modelFiles <- modelsByPackage.map {
          case (packageName, packageModels) =>
            generateModelFiles(packageModels, packageName, codeGenTarget).withDebug("model files")
        }.toList.sequenceU
        codecFiles <- modelsByPackage.map {
          case (packageName, packageModels) =>
            generateCodecFiles(packageModels, packageName, codeGenTarget).withDebug("serializatoin files")
        }.toList.sequenceU
        predefinedCodecs <- generateCodecFiles(codeGenTarget).withDebug("serialization files")
      } yield {
        val paths: List[Path] = predefinedCodecs ++ modelFiles.flatten ++ codecFiles.flatten
        info(s"generated ${paths.size} in $codeGenTarget")
        paths.withDebug("generated files")
      }

    }

    def languageModel[N: Numeric](schema: SchemaDocument[N]): SValidation[Set[LangType]]

    /**
     * generate model files.
     * @param ts model graph.
     * @param scope target scope where to generate model code.
     * @param outputDir
     * @return generated list of files.
     */
    def generateModelFiles(ts: Set[LangType], scope: String, outputDir: Path): SValidation[List[Path]]

    /**
     * generate codecs for predefined types.
     * @param outputDir
     * @return generated list of files.
     */
    def generateCodecFiles(outputDir: Path): SValidation[List[Path]]

    /**
     * generate codecs for the above models.
     * @param ts model graph.
     * @param scope target scope where to generate model code.
     * @param outputDir
     * @return generated list of files.
     */
    def generateCodecFiles(ts: Set[LangType], scope: String, outputDir: Path): SValidation[List[Path]]

  }

}
