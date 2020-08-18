package json.schema.codegen

import json.schema.parser.JsonSchemaParser
import org.scalatest.{FlatSpec, Matchers}
import scalaz.\/-

class ScalaCodecGeneratorTest extends FlatSpec with Matchers with ScalaGenerator with ConsoleLogging {

  def parse(s: String): SValidation[Set[LangType]] = JsonSchemaParser.parse(s).flatMap(ScalaModelGenerator(_))

  def gen(s: String): SValidation[String] =
    parse(s).map { ts =>
      ts.map {
        case t: ClassType => genCodecClass(t).trim
        case _            => ""
      }.filter(_.nonEmpty).mkString("\n")
    }

  it should "generate case codec implicits when are less than 22 fields" in {
    val codec = gen("""
        |{
        | "id": "http://some/product",
        |"type":"object",
        |"properties": {
        |"type":{"type":"string"},
        |"b":{"type":"number"}
        |},
        |"required":["type"]
        |}
      """.stripMargin)
    codec shouldBe \/-(
      """implicit def ProductCodec: CodecJson[Product] = casecodec2(Product.apply, Product.unapply)("type", "b")""".stripMargin.trim
    )
  }

  it should "generate derived codec implicits when are more than 22 fields" in {
    val codec = gen("""
        |{
        | "id": "http://some/product",
        |"type":"object",
        |"properties": {
        |"b0":{"type":"number"},
        |"b1":{"type":"number"},
        |"b2":{"type":"number"},
        |"b3":{"type":"number"},
        |"b4":{"type":"number"},
        |"b5":{"type":"number"},
        |"b6":{"type":"number"},
        |"b7":{"type":"number"},
        |"b8":{"type":"number"},
        |"b9":{"type":"number"},
        |"b10":{"type":"number"},
        |"b11":{"type":"number"},
        |"b12":{"type":"number"},
        |"b13":{"type":"number"},
        |"b14":{"type":"number"},
        |"b15":{"type":"number"},
        |"b16":{"type":"number"},
        |"b17":{"type":"number"},
        |"b18":{"type":"number"},
        |"b19":{"type":"number"},
        |"b20":{"type":"number"},
        |"b21":{"type":"number"},
        |"b22":{"type":"number"},
        |"b23":{"type":"number"}
        |},
        |"required":["b0"]
        |}
      """.stripMargin)
    codec shouldBe \/-( // Note: No type declaration on this one, since it will cause a stack overflow error
      """implicit def ProductCodec = CodecJson.derived(EncodeJson.of[Product], DecodeJson.of[Product])""".stripMargin.trim
    )
  }

  it should "fail to generate derived codec implicits when are more than 22 fields and a field name is a scala keyword" in {
    an[IllegalArgumentException] should be thrownBy gen("""
        |{
        | "id": "http://some/product",
        |"type":"object",
        |"properties": {
        |"type":{"type":"string"},
        |"b0":{"type":"number"},
        |"b1":{"type":"number"},
        |"b2":{"type":"number"},
        |"b3":{"type":"number"},
        |"b4":{"type":"number"},
        |"b5":{"type":"number"},
        |"b6":{"type":"number"},
        |"b7":{"type":"number"},
        |"b8":{"type":"number"},
        |"b9":{"type":"number"},
        |"b10":{"type":"number"},
        |"b11":{"type":"number"},
        |"b12":{"type":"number"},
        |"b13":{"type":"number"},
        |"b14":{"type":"number"},
        |"b15":{"type":"number"},
        |"b16":{"type":"number"},
        |"b17":{"type":"number"},
        |"b18":{"type":"number"},
        |"b19":{"type":"number"},
        |"b20":{"type":"number"},
        |"b21":{"type":"number"},
        |"b22":{"type":"number"},
        |"b23":{"type":"number"}
        |},
        |"required":["b0"]
        |}
      """.stripMargin)
  }

  it should "generate codecs type with additional properties in a map" in {
    gen("""
          |{
          | "id": "http://some/product",
          |"type":"object",
          |"additionalProperties":{"$ref":"#/definitions/nested"},
          |"definitions": {
          |"nested": {
          |"id":"#/definitions/nested",
          |"type":"object"
          | }
          |}
          |}
      """.stripMargin) shouldBe
      \/-("""private def ProductSimpleCodec: CodecJson[Product] = casecodec1(Product.apply, Product.unapply)("_additional")
            |
            |           implicit def ProductCodec: CodecJson[Product] = CodecJson.derived(EncodeJson {
            |              v =>
            |                val j = ProductSimpleCodec.encode(v)
            |                val nj = j.field("_additional").fold(j)(a => j.deepmerge(a))
            |                nj.hcursor.downField("_additional").deleteGoParent.focus.getOrElse(nj)
            |            }, DecodeJson {
            |              c =>
            |                val md: DecodeJson[Option[Map[String, product.definitions.Nested]]] = implicitly
            |                val od: DecodeJson[Product] = ProductSimpleCodec
            |                for {
            |                  o <- od.decode(c)
            |                  withoutProps = List().foldLeft(c)((c, f) => c.downField(f).deleteGoParent.hcursor.getOrElse(c))
            |                  m <- md.decode(withoutProps)
            |                } yield o.copy(_additional = m)
            |            })
            |implicit def NestedCodec: CodecJson[Nested] = casecodec0(Nested.apply, Nested.unapply)()""".stripMargin)
  }
}
