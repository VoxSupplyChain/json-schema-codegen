package json.schema.codegen

import json.schema.parser.JsonSchemaParser
import org.scalatest.{FlatSpec, Matchers}
import scalaz.\/-

class ScalaCodecGeneratorTest extends FlatSpec with Matchers with ScalaGenerator with ConsoleLogging {

  def parse(s: String): SValidation[Set[LangType]] = JsonSchemaParser.parse(s).flatMap(ScalaModelGenerator(_))

  def gen(s: String): SValidation[String] =
    parse(s).map { ts =>
      ts.map {
        case t: ClassType => genCodecClass(t)
        case _ => ""
      }.filter(_.nonEmpty).mkString("\n")
    }

  it should "generate case codec implicits when field name is scala keyword" in {
    val codec = gen(
      """
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
    codec shouldBe \/-("""implicit def ProductCodec = casecodec2(Product.apply, Product.unapply)("type", "b")""".stripMargin.trim)
  }

  it should "generate derived codec implicits when field name is not a scala keyword" in {
    val codec = gen(
      """
        |{
        | "id": "http://some/product",
        |"type":"object",
        |"properties": {
        |"notKeyword":{"type":"string"},
        |"b":{"type":"number"}
        |},
        |"required":["type"]
        |}
      """.stripMargin)
    codec shouldBe \/-("""implicit def ProductCodec = CodecJson.derived(EncodeJson.of[Product], DecodeJson.of[Product])""".stripMargin.trim)
  }
}
