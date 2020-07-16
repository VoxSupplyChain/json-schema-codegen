package json.schema.codegen

import java.io.File
import java.nio.file.Path

import json.schema.parser.SchemaDocument

import scalaz.Success
import scalaz.syntax.all._

trait TypeScriptGenerator extends CodeGenerator with TypeScriptNaming {

  val fileName: String = "model.ts"

  override def generatedLanguage: String = "Scala"

  def generateModelFiles(ts: Set[LangType], scope: String, outputDir: Path): SValidation[List[Path]] =
    generateFile(scope, fileName, outputDir) { packageName =>
      val referencesBlock = ts.flatMap(referenceString).mkString("\n")

      val packageDecl = packageName.map(p => s"$referencesBlock\n\ndeclare module $p {\n\n").getOrElse("")
      val modelDecl   = ts.map(genTypeDeclaration).filter(!_.trim.isEmpty)
      if (modelDecl.isEmpty)
        "".right
      else
        modelDecl
          .mkString(
            packageDecl,
            "\n\n",
            "\n\n}"
          )
          .right
    }

  private def referenceString(lt: LangType): Set[String] = {
    val thisRef     = withPackagePath(lt)(fileName)
    val thisRefPath = Option(new File(thisRef).getParentFile).map(_.toPath).getOrElse(new File("").toPath)

    lt.referenced.filter(_.scope.nonEmpty).map(t => withPackagePath(t)(fileName)).filter(_ != thisRef).map { ref =>
      val othersRef = new File(ref).toPath
      val relRef    = thisRefPath.relativize(othersRef)
      s"""/// <reference path="$relRef" />"""
    }
  }

  private def withPackagePath(t: LangType)(name: => String): String =
    if (t.scope.isEmpty) name else t.scope.replace(".", "/") + "/" + name

  private def withPackageReference(t: LangType)(name: => String): String =
    if (t.scope.isEmpty) name else t.scope + "." + name

  def genPropertyType(t: LangType): String =
    t match {
      case a: ArrayType =>
        val nestedType = genPropertyType(a.nested)
        if (a.unique) s"$nestedType[]/* a set */" else s"$nestedType[]"
      case a: LangType => withPackageReference(a)(a.identifier)
    }

  def genPropertyType(p: LangTypeProperty): String = genPropertyType(p.isa)

  def genTypeDeclaration(clazz: LangType): String =
    clazz match {
      case t: ClassType =>
        val properties: List[String] = t.properties.map { p =>
          val propType = genPropertyType(p)
          memberName(p.name).map { member =>
            if (p.required) s"$member: $propType;" else s"$member?: $propType;"
          }.getOrElse("")
        }.filter(_.nonEmpty)

        val extra =
          t.additionalNested.map { tn =>
            val propType = genPropertyType(tn)
            s"[key: string]: $propType;"
          }
        val members = "\n" + (properties ++ extra.toList).mkString("\n") + "\n"
        s"""interface ${t.identifier} {$members}""".stripMargin
      case a: ArrayType =>
        val typeEq = genPropertyType(a)
        s"""type ${a.identifier} = $typeEq""".stripMargin
      case t: AliasType =>
        val typeEq = genPropertyType(t.nested)
        s"""type ${t.identifier} = $typeEq""".stripMargin
      case t: EnumType =>
        val values = t.enums.map {
          case v: String => "\"" + v + "\""
          case v: Int =>
            v.toInt.toString
          case v: Long =>
            v.toLong.toString
          case v: Double =>
            v.toString
          case _ => ""
        }.mkString(" | ")
        val typeEq = if (values.isEmpty) genPropertyType(t.nested) else values
        s"""type ${t.identifier} = $typeEq""".stripMargin
      case _ => ""
    }

  def memberName(s: String): Option[String] = if (isIdentifier(s)) escapePropertyReserved(s) else None

  // typescript code needs only types to be able to access the JS object models. it is up to the user to deserialize the JSON to JS objects.
  def generateCodecFiles(ts: Set[LangType], scope: String, outputDir: Path): SValidation[List[Path]] = Nil.right

  def generateCodecFiles(outputDir: Path): SValidation[List[Path]] = Nil.right

  def languageModel[N: Numeric](schema: SchemaDocument[N]): SValidation[Set[LangType]] =
    TypeScriptModelGenerator(schema)

}

object TypeScriptCmd extends GeneratorCommand(List(new TypeScriptGenerator with ConsoleLogging {}))
