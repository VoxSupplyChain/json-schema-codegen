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

  it should "generate codec with case classes when field name is scala keyword" in {
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
    println(codec)
    // TODO: continue from here

    //shouldBe \/-("""case class Product(_type:String, b:Option[Double])""".stripMargin.trim)
    gen(
      """
        |{
        | "id": "http://some/product",
        |"type":"object",
        |"properties": {
        |"big number":{"type":"number"}
        |}
        |}
      """.stripMargin) shouldBe \/-("""case class Product(big_number:Option[Double])""".stripMargin.trim)
  }
}
