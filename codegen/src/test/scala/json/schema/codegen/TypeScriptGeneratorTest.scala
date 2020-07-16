package json.schema.codegen

import json.schema.parser.JsonSchemaParser
import org.scalatest.{FlatSpec, Matchers}

import scalaz.{\/-, Success}

class TypeScriptGeneratorTest extends FlatSpec with Matchers with TypeScriptGenerator with ConsoleLogging {

  def parse(s: String): SValidation[Set[LangType]] = JsonSchemaParser.parse(s).flatMap(TypeScriptModelGenerator(_))

  def gen(s: String): SValidation[String] =
    parse(s) map { ts =>
      ts.map(genTypeDeclaration).mkString("\n").trim
    }

  "TypeScriptGenerator" should "generate type with optional properties" in {
    gen("""
        |{
        | "id": "http://some/product",
        |"type":"object",
        |"properties": {
        |"a":{"type":"string"},
        |"b":{"type":"number"}
        |},
        |"required":["a"]
        |}
      """.stripMargin) shouldBe \/-("""interface Product {
        |a: string;
        |b?: number;
        |}""".stripMargin.trim)
  }

  it should "generate type with properties names as is" in {
    gen("""
        |{
        | "id": "http://some/product",
        |"type":"object",
        |"properties": {
        |"a_b":{"type":"string"}
        |},
        |"required":["a_b"]
        |}
      """.stripMargin) shouldBe \/-("""interface Product {
        |a_b: string;
        |}""".stripMargin.trim)
  }
  it should "invalid and reserved properties are ignored" in {
    gen("""
        |{
        | "id": "http://some/product",
        |"type":"object",
        |"properties": {
        |"c d":{"type":"number"},
        |"if":{"type":"string"}
        |}
        |}
      """.stripMargin) shouldBe \/-("""interface Product {
        |
        |}""".stripMargin.trim)
  }

  it should "generate type with array properties" in {
    gen("""
        |{
        | "id": "http://some/product",
        |"type":"object",
        |"properties": {
        |"a":{"type":"array", "items":{"type":"string"}},
        |"b":{"type":"array", "items":{"type":"number"}}
        |},
        |"required":["a"]
        |}
      """.stripMargin) shouldBe \/-("""interface Product {
        |a: string[];
        |b?: number[];
        |}""".stripMargin.trim)
  }

  it should "generate type with nested types" in {
    gen("""
        |{
        | "id": "http://some/product",
        |"type":"object",
        |"properties": {
        |"a":{"type":"array", "items":{"$ref":"#/definitions/nested"}},
        |"b":{"type":"array", "items":{"type":"number"}}
        |},
        |"required":["a"],
        |"definitions": {
        |"nested": {
        |"id":"#/definitions/nested",
        |"type":"object"
        | }
        |}
        |
        |}
      """.stripMargin) shouldBe \/-("""
        |interface Product {
        |a: product.definitions.Nested[];
        |b?: number[];
        |}
        |interface Nested {
        |
        |}
        | """.stripMargin.trim)
  }

  it should "enumeration types are not supported" in {
    gen("""
        |{
        | "id": "http://some/product",
        |"type":"string",
        |"enum":["a 1","b"]
        |}
      """.stripMargin) shouldBe \/-("type Product = \"a 1\" | \"b\"")
    gen("""
        |{
        | "id": "http://some/product",
        |"type":"object",
        |"properties": {
        |"a":{
        |"type":"string",
        |"enum":["a 1","b"]
        |},
        |"b":{"type":"number",
        |"enum":[1,2]}
        |}
        |}
      """.stripMargin) shouldBe \/-("""type B = 1.0 | 2.0
        |type A = "a 1" | "b"
        |interface Product {
        |a?: A;
        |b?: B;
        |}""".stripMargin.trim)
  }

  it should "generate type with additional properties in a map" in {
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
        |
        |}
      """.stripMargin) shouldBe
      \/-("""
          |interface Product {
          |[key: string]: product.definitions.Nested;
          |}
          |interface Nested {
          |
          |}
          | """.stripMargin.trim)
  }

  it should "generate array of types" in {
    gen("""
        |{
        |  "id": "http://some/StringArray",
        |  "type": "array",
        |  "items": {
        |     "type": "string"
        |  }
        |}
        |""".stripMargin.trim) shouldBe
      \/-("""
          |type StringArray = string[]
          |""".stripMargin.trim)
  }

  it should "generate simple types" in {
    gen("""
        |{
        |  "id": "http://some/MyString",
        |  "type": "string"
        |}
        |""".stripMargin.trim) shouldBe
      \/-("""
          |type MyString = string
          |""".stripMargin.trim)
  }

}
