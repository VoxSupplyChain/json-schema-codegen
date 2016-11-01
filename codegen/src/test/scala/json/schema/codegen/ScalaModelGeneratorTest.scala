package json.schema.codegen

import json.schema.parser.JsonSchemaParser
import org.scalatest.{FlatSpec, Matchers}

import scalaz.{\/-, Success}

class ScalaModelGeneratorTest extends FlatSpec with Matchers {


  def parse(s: String): SValidation[LangType] = JsonSchemaParser.parse(s).flatMap(ScalaModelGenerator(_)).map(_.head)

  def parseAll(s: String): SValidation[Set[LangType]] = JsonSchemaParser.parse(s).flatMap(ScalaModelGenerator(_))

  ScalaModelGenerator.getClass.getName should "convert simple types to Scala types" in {
    parse(
      """
        |{"type":"integer"}
      """.stripMargin).map(_.identifier) shouldBe \/-("Long")
    parse(
      """
        |{"type":"boolean"}
      """.stripMargin).map(_.identifier) shouldBe \/-("Boolean")
    parse(
      """
        |{"type":"number"}
      """.stripMargin).map(_.identifier) shouldBe \/-("Double")
    parse(
      """
        |{"type":"string"}
      """.stripMargin).map(_.identifier) shouldBe \/-("String")
  }

  it should "convert simple types with format to Scala types" in {
    parse(
      """
        |{"type":"string",
        |"format":"uri"
        |}
      """.stripMargin).map(_.identifier) shouldBe \/-("URI")
  }

  it should "convert array of unique items to Scala Set" in {
    parse(
      """
        |{"type":"array",
        |"items":{"type":"string"}, "uniqueItems":true
        |}
      """.stripMargin) shouldBe \/-(ArrayType("", unique = true, PredefType("", "String")))
  }

  it should "convert array of items to Scala List" in {
    parse(
      """
        |{"type":"array",
        |"items":{"type":"string"}
        |}
      """.stripMargin) shouldBe \/-(ArrayType("", unique = false, PredefType("", "String")))
  }

  it should "use id in camel case for class name" in {
    parse(
      """
        |{
        | "id": "http://some/product",
        |"type":"object"
        |}
      """.stripMargin) shouldBe \/-(ClassType("", "Product", Nil, None))

    parse(
      """
        |{
        | "id": "http://some/path#/product",
        |"type":"object"
        |}
      """.stripMargin) shouldBe \/-(ClassType("path", "Product", Nil, None))
  }

  it should "create type with members from properties" in {
    parse(
      """
        |{
        | "id": "http://some/product",
        |"type":"object",
        |"properties": {
        |"a":{"type":"string"},
        |"b":{"type":"number"}
        |},
        |"required":["a"]
        |}
      """.stripMargin).map(_.asInstanceOf[ClassType].properties) shouldBe \/-(
      List(
        LangTypeProperty("a", required = true, PredefType("", "String")),
        LangTypeProperty("b", required = false, PredefType("", "Double"))
      )
    )
  }

  it should "create type using definitions" in {
    parse(
      """
        |{
        | "id": "product",
        |"type":"object",
        |"properties": {
        |"a":{"$ref": "#/definitions/typea"}
        |},
        |"definitions":{
        | "typea":{"type":"string"}
        |},
        |"required":["a"]
        |}
      """.stripMargin).map(_.asInstanceOf[ClassType].properties) shouldBe \/-(
      List(
        LangTypeProperty("a", required = true, PredefType("", "String"))
      )
    )
  }

  it should "create type with name using explicit id " in {
    parse(
      """
        |{
        | "id": "product",
        |"type":"object",
        |"properties": {
        |"a":{"$ref": "#/definitions/typea"}
        |},
        |"definitions":{
        | "typea":{
        | "id":"#/definitions/typea",
        | "type":"object",
        | "properties":{
        | "b":{"type":"string"}
        | }
        | }
        |},
        |"required":["a"]
        |}
      """.stripMargin).map(_.asInstanceOf[ClassType].properties) shouldBe \/-(
      List(
        LangTypeProperty("a", required = true, ClassType("product.definitions", "Typea", List(LangTypeProperty("b", required = false, PredefType("", "String"))), None))
      )
    )
  }

  it should "create type with field name as type name, when id is not specified" in {
    parse(
      """
        |{
        | "id": "product",
        |"type":"object",
        |"properties": {
        |"a":{"$ref": "#/definitions/typea"}
        |},
        |"definitions":{
        | "typea":{
        | "type":"object",
        | "properties":{
        | "nested":{"type":"string"}
        | }
        | }
        |},
        |"required":["a"]
        |}
      """.stripMargin).map(_.asInstanceOf[ClassType].properties) shouldBe \/-(
      List(
        LangTypeProperty("a", required = true, ClassType("product.definitions", "Typea", List(LangTypeProperty("nested", required = false, PredefType("", "String"))), None))
      )
    )
  }

  it should "create type reusing same sub type" in {
    parse(
      """
        |{
        |"id": "product",
        |"type":"object",
        |"properties": {
        |"a":{"$ref": "#/definitions/typea"},
        |"b":{"$ref": "#/definitions/typea"}
        |},
        |"definitions":{
        | "typea":{
        | "type":"object",
        | "properties":{
        | "nested":{"type":"string"}
        | }
        | }
        |},
        |"required":["a","b"]
        |}
      """.stripMargin).map(_.asInstanceOf[ClassType].properties) shouldBe \/-(
      List(
        LangTypeProperty("a", required = true, ClassType("product.definitions", "Typea", List(LangTypeProperty("nested", required = false, PredefType("", "String"))), None)),
        LangTypeProperty("b", required = true, ClassType("product.definitions", "Typea", List(LangTypeProperty("nested", required = false, PredefType("", "String"))), None))
      )
    )

    parse(
      """
        |{
        | "id": "product",
        |"type":"object",
        |"properties": {
        |"a":{"$ref": "#/definitions/typea"},
        |"b":{"$ref": "#/definitions/typea"}
        |},
        |"definitions":{
        | "typea":{
        | "id":"#/definitions/typea",
        | "type":"object",
        | "properties":{
        | "b":{"type":"string"}
        | }
        | }
        |},
        |"required":["a","b"]
        |}
      """.stripMargin).map(_.asInstanceOf[ClassType].properties) shouldBe \/-(
      List(
        LangTypeProperty("a", required = true, ClassType("product.definitions", "Typea", List(LangTypeProperty("b", required = false, PredefType("", "String"))), None)),
        LangTypeProperty("b", required = true, ClassType("product.definitions", "Typea", List(LangTypeProperty("b", required = false, PredefType("", "String"))), None))
      )
    )
  }
  it should "create enum type " in {
    parse(
      """
        |{
        |"type":"string",
        |"enum": ["a","b"]
        |}
      """.stripMargin).map(_.asInstanceOf[EnumType].enums) shouldBe \/-(
      Set(
        "a", "b"
      )
    )
    parse(
      """
        |{
        |"type":"number",
        |"enum": [1,2]
        |}
      """.stripMargin).map(_.asInstanceOf[EnumType].enums) shouldBe \/-(
      Set(
        1d, 2d
      )
    )
  }


  it should "type with additionalProperties has a map" in {
    parse(
      """
        |{
        | "id": "product",
        |"type":"object",
        |"additionalProperties":{"$ref": "#/definitions/typea"},
        |"definitions":{
        | "typea":{
        | "id":"#/definitions/typea",
        | "type":"object",
        | "properties":{
        | "nested":{"type":"string"}
        | }
        | }
        |}
        |}
      """.stripMargin).map(_.asInstanceOf[ClassType].additionalNested.map(_.identifier)) shouldBe \/-(
      Some(
        "Typea"
      )
    )
  }

  it should "correctly unpack a union type" in {
    parse(
      """
        |{
        |  "id": "root",
        |  "type": "object",
        |  "additionalProperties": {
        |    "oneOf": [
        |      {"type": "string"},
        |      {"type": "number"},
        |      {"type": "string"}
        |    ]
        |  }
        |}
      """.stripMargin) shouldBe \/-(
        UnionType("", "Root", List(PredefType("", "String"), PredefType("", "Double"), PredefType("", "String")))
      )
  }

  it should "preserve scope of referenced types" in {
    val models: SValidation[Set[LangType]] = parseAll(
      """
        |{
        |"id": "product",
        |"type":"object",
        |"properties":{
        | "typea":{
        |   "$ref":"nested-schema-entity"
        |  }
        | },
        | "schema2": {
        |   "id": "nested-schema-entity",
        |   "type":"object"
        | }
        |}
      """.stripMargin)
    models.map(_.size) shouldBe \/-(2)
    models.map(_.find(_.identifier == "Product").map(_.scope)) shouldBe \/-(Some(""))
    models.map(_.find(_.identifier == "Entity").map(_.scope)) shouldBe \/-(Some("nested.schema"))
  }

}
