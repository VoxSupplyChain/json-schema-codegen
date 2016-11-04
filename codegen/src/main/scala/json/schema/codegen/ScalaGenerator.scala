package json.schema.codegen

import java.nio.file.Path

import json.schema.parser.SchemaDocument

import scalaz.syntax.all._
import scalaz.syntax.std.all._

trait ScalaGenerator extends CodeGenerator with ScalaNaming {

  val predefinedPackageCodec = "json.schema.codegen.predefined"

  val addPropName = "_additional"

  def generateCodecFiles(outputDir: Path): SValidation[List[Path]] = {
    val codecClassName: String = "Codecs"
    val fileName: String = codecClassName.toLowerCase + ".scala"
    generateFile(predefinedPackageCodec, fileName, outputDir) {
      packageName =>

        val codecs = List(
          genCodecURI(),
          genCodecInetAddress("4"),
          genCodecInetAddress("6"),
          genCodecDate()
        ).filter(!_.trim.isEmpty).mkString("\n")

        if (codecs.isEmpty)
          "".right
        else {
          val packageDecl = packageName.map(p => s"package $p").getOrElse("")
          s"""
          |$packageDecl
          |
          |import argonaut._, Argonaut._
          |
          |trait $codecClassName {
          |  $codecs
          |}
          |
          |object $codecClassName extends $codecClassName
          |""".stripMargin.right
        }

    }

  }


  def generateCodecFiles(ts: Set[LangType], scope: String, outputDir: Path): SValidation[List[Path]] = {
    val codecClassName: String = "Codecs"
    val fileName: String = codecClassName + ".scala"

    val formatTypes: Set[LangType] = ScalaModelGenerator.format2scala.values.toSet

    def codecPackage(t: LangType) = formatTypes.contains(t) ? (predefinedPackageCodec + "." + codecClassName) | withPackageReference(t)(codecClassName)

    generateFile(scope, fileName, outputDir) {
      packageName =>

        val referencedTypes = ts.flatMap(_.referenced).filter(t => !t.scope.isEmpty && t.scope != scope)
        val referencedCodes = referencedTypes.isEmpty ? "" | referencedTypes.map(codecPackage).mkString(" extends ", " with ", "")

        val codecs = ts.map {
          case t: ClassType => genCodecClass(t)
          case t: EnumType => genCodecEnum(t)
          case _ => ""
        }.filter(!_.trim.isEmpty).mkString("\n")

        if (codecs.isEmpty)
          "".right
        else {
          val packageDecl = packageName.map(p => s"package $p").getOrElse("")
          s"""
          |$packageDecl
          |
          |import argonaut._, Argonaut._
          |
          |trait $codecClassName $referencedCodes {
          |  $codecs
          |}
          |
          |object $codecClassName extends $codecClassName
          |""".stripMargin.right
        }

    }

  }

  def generateModelFiles(ts: Set[LangType], scope: String, outputDir: Path): SValidation[List[Path]] = {
    val fileName: String = "model.scala"
    generateFile(scope, fileName, outputDir) {
      packageName =>

        val packageDecl = packageName.map(p => s"package $p\n\n").getOrElse("")
        val modelDecl = ts.map(genTypeDeclaration).filter(!_.trim.isEmpty)
        if (modelDecl.isEmpty)
          "".right
        else
          modelDecl.mkString(
            packageDecl,
            "\n\n", ""
          ).right

    }
  }

  def genCodecClass(c: ClassType): String = {
    val propNames = c.properties.map(p => '"' + p.name + '"').mkString(", ")
    val className = c.identifier

    c.additionalNested match {
      case None =>
        s"""implicit def ${className}Codec: CodecJson[$className]=casecodec${c.properties.length}($className.apply, $className.unapply)($propNames)"""
      case Some(additionalType) =>
        val addClassReference = genPropertyType(additionalType)
        val addPropNames = propNames + (propNames.isEmpty ? "" | ", ") + '"' + addPropName + '"'
        s"""
        |private def ${className}SimpleCodec: CodecJson[$className] = casecodec${c.properties.length + 1}($className.apply, $className.unapply)($addPropNames)
        |
        |implicit def ${className}Codec: CodecJson[$className] = CodecJson.derived(EncodeJson {
        |  v =>
        |    val j = ${className}SimpleCodec.encode(v)
        |    val nj = j.field("$addPropName").fold(j)(a => j.deepmerge(a))
        |    nj.hcursor.downField("$addPropName").deleteGoParent.focus.getOrElse(nj)
        |}, DecodeJson {
        |  c =>
        |    val md: DecodeJson[Option[Map[String, $addClassReference]]] = implicitly
        |    val od: DecodeJson[$className] = ${className}SimpleCodec
        |    for {
        |      o <- od.decode(c)
        |      withoutProps = List($propNames).foldLeft(c)((c, f) => c.downField(f).deleteGoParent.hcursor.getOrElse(c))
        |      m <- md.decode(withoutProps)
        |    } yield o.copy($addPropName = m)
        |})
        |""".stripMargin
    }
  }

  def genCodecEnum(c: EnumType): String = {
    val enumTypeName = c.identifier
    val enumNameReference = genPropertyType(c)
    val nestedTypeReference = genPropertyType(c.nested)
    s"""
    |implicit def ${enumTypeName}Codec: CodecJson[$enumNameReference] = CodecJson[$enumNameReference]((v: $enumNameReference) => v.${if (nestedTypeReference == "String") "toString" else "id"}.asJson, (j: HCursor) => j.as[$nestedTypeReference].flatMap {
    |  s: $nestedTypeReference =>
    |    try{
    |      DecodeResult.ok(${if (c.nested.identifier == "String") enumTypeName + ".withName" else enumTypeName}(${if (c.nested.identifier == "String") "s" else "s.toInt"}))
    |    } catch {
    |      case e:NoSuchElementException => DecodeResult.fail("$enumTypeName", j.history)
    |    }
    |})
    |""".stripMargin
  }

  def genCodecURI(): String =
    """
    |implicit def URICodec: CodecJson[java.net.URI] = CodecJson.derived(
    |  EncodeJson(v => jString(v.toString)),
    |  StringDecodeJson.flatMap {
    |    uri =>
    |      DecodeJson(
    |        j => {
    |          try{
    |            DecodeResult.ok(new java.net.URI(uri))
    |          } catch {
    |            case e:NoSuchElementException => DecodeResult.fail("URI", j.history)
    |          }
    |        }
    |      )
    |  }
    |)
    |""".stripMargin

  def genCodecDate(): String =
    s"""
    |private def isoDate = new java.text.SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSXXX")
    |implicit def DateCodec: CodecJson[java.util.Date] = CodecJson.derived(
    |  EncodeJson(v => jString(isoDate.format(v))), StringDecodeJson.flatMap {
    |    time =>
    |      DecodeJson(
    |        j => {
    |         try {
    |           DecodeResult.ok(isoDate.parse(time))
    |         } catch {
    |           case e: NoSuchElementException => DecodeResult.fail("Inet6Address", j.history)
    |         }
    |       }
    |      )
    |  })
    """.stripMargin

  def genCodecInetAddress(v: String): String =
    s"""
    |implicit def Inet${v}AddressCodec: CodecJson[java.net.Inet${v}Address] = CodecJson.derived(
    |  EncodeJson(v => jString(v.toString.substring(1))), StringDecodeJson.flatMap {
    |    addr =>
    |      DecodeJson(
    |        j => {
    |          try {
    |            DecodeResult.ok(java.net.InetAddress.getByName(addr).asInstanceOf[java.net.Inet${v}Address])
    |          } catch {
    |            case e: NoSuchElementException => DecodeResult.fail("Inet${v}Address", j.history)
    |          }
    |        }
    |      )
    |  }
    |)
    |""".stripMargin


  private def withPackageReference(t: LangType)(name: => String): String = if (t.scope.isEmpty) name else t.scope + "." + name

  def genPropertyType(t: LangType): String = {
    t match {
      case a: ArrayType =>
        val nestedType = genPropertyType(a.nested)
        if (a.unique) s"Set[$nestedType]" else s"List[$nestedType]"
      case a: EnumType => withPackageReference(a)(a.identifier + ".Value")
      case a: UnionType if a.members.nonEmpty =>
        val (x :: xs) = a.members.map(genPropertyType).reverse
        xs.foldLeft(x) { case (a, x) => s"Either[${x}, ${a}]" }
      case a: LangType => withPackageReference(a)(a.identifier)
    }
  }

  def genPropertyType(p: LangTypeProperty): String = {
    val t = genPropertyType(p.isa)
    p.required ? t | s"Option[$t]"
  }

  def genTypeDeclaration(clazz: LangType): String = clazz match {
    case t: ClassType =>
      val properties = t.properties.map {
        p =>
          val propType = genPropertyType(p)
          memberName(p.name).map {
            member =>
              s"$member:$propType"
          }.getOrElse("")
      }
      val extra =
        t.additionalNested.map {
          tn =>
            val propType = genPropertyType(tn)
            // nested type is option , so that the special codec works even when no props are given
            s"$addPropName:Option[Map[String, $propType]]"
        }
      val members = (properties ++ extra.toList).mkString(", ")
      s"""case class ${t.identifier}($members)""".stripMargin

    // enum of number are not support
    case t: EnumType if t.nested.identifier == "Double" => ""
    case t: EnumType =>
      val valueDeclarations = t.enums.map {
        case v: String =>
          memberName(v).map {
            valueId =>
              s"""val $valueId = Value("$v")"""
          }.getOrElse("")
        case v: Int =>
          val valueId = s"v$v"
          s"val $valueId = Value(${v.toInt})"
        case v: Long =>
          val valueId = s"v$v"
          s"val $valueId = Value(${v.toInt})"
        case v: Double =>
          val valueId = s"v${v.toInt}"
          s"val $valueId = Value(${v.toInt})"
        case _ => ""
      }.filter(_ != "").mkString("\n")
      s"""object ${t.identifier} extends Enumeration { $valueDeclarations }""".stripMargin

    case _ => ""

  }

  def memberName(s: String): Option[String] = {
    val camel: String = underscoreToCamel(identifier(s))
    Some(escapeReserved(camel, None))
  }

  def languageModel[N: Numeric](schema: SchemaDocument[N]): SValidation[Set[LangType]] = ScalaModelGenerator(schema)

}


object ScalaCmd extends GeneratorCommand(List(new ScalaGenerator with ConsoleLogging {}))
