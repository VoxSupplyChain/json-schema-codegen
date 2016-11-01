package json.schema.codegen

import json.schema.parser.JsonSchemaParser
import org.scalatest.{FlatSpec, Matchers}

import scalaz.{\/-, Success}

class ScalaGeneratorTest extends FlatSpec with Matchers with ScalaGenerator with ConsoleLogging {


  def parse(s: String): SValidation[Set[LangType]] = JsonSchemaParser.parse(s).flatMap(ScalaModelGenerator(_))

  def gen(s: String): SValidation[String] = parse(s) map {
    ts => ts.map(genTypeDeclaration).mkString("\n").trim
  }

  "ScalaGenerator" should "generate type with optional properties" in {
    gen(
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
      """.stripMargin) shouldBe \/-( """case class Product(a:String, b:Option[Double])""".stripMargin.trim)
  }

  it should "generate type with array properties" in {
    gen(
      """
        |{
        | "id": "http://some/product",
        |"type":"object",
        |"properties": {
        |"a":{"type":"array", "items":{"type":"string"}},
        |"b":{"type":"array", "items":{"type":"number"}}
        |},
        |"required":["a"]
        |}
      """.stripMargin) shouldBe \/-( """case class Product(a:List[String], b:Option[List[Double]])""".stripMargin.trim)
  }

  it should "generate type with nested types" in {
    gen(
      """
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
      """.stripMargin) shouldBe \/-(
      """
        |case class Product(a:List[product.definitions.Nested], b:Option[List[Double]])
        |
        |case class Nested()
        | """.stripMargin.trim)
  }

  it should "generate enumeration with values " in {
    gen(
      """
        |{
        | "id": "http://some/product",
        |"type":"string",
        |"enum":["a 1","b"]
        |}
      """.stripMargin) shouldBe \/-(
      """
        |object Product extends Enumeration { val a1 = Value("a 1")
        |val b = Value("b") }""".stripMargin.trim)
    gen(
      """
        |{
        | "id": "http://some/product",
        |"type":"integer",
        |"enum":[1,2]
        |}
      """.stripMargin).map(_.replaceAll("\\s", "")) shouldBe \/-(
      """
        |object Product extends Enumeration { val v1 = Value(1)
        |val v2 = Value(2) }""".stripMargin.trim.replaceAll("\\s", ""))
  }


  it should "generate type with additional properties in a map" in {
    gen(
      """
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
      \/-(
        """
          |case class Product(_additional:Option[Map[String, product.definitions.Nested]])
          |case class Nested()
          | """.stripMargin.trim)
  }


  it should "generate type with escaped properties" in {
    gen(
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
      """.stripMargin) shouldBe \/-( """case class Product(_type:String, b:Option[Double])""".stripMargin.trim)
    gen(
      """
        |{
        | "id": "http://some/product",
        |"type":"object",
        |"properties": {
        |"big number":{"type":"number"}
        |}
        |}
      """.stripMargin) shouldBe \/-( """case class Product(bigNumber:Option[Double])""".stripMargin.trim)
  }

  it should "generate type with escaped name" in {
    gen(
      """
        |{
        | "id": "http://some/type",
        |"type":"string",
        |"enum":["a 1","b"]
        |}
      """.stripMargin) shouldBe \/-(
      """
        |object Type extends Enumeration { val a1 = Value("a 1")
        |val b = Value("b") }""".stripMargin.trim)
  }

  it should "generate recursive references to a single class" in {
    gen("""
      |{
      |  "id": "http://some/reference",
      |  "type": "object",
      |  "properties": {
      |    "a": {
      |      "$ref": "#/definitions/b"
      |    }
      |  },
      |  "definitions": {
      |    "b": {
      |      "type": "object",
      |      "required": ["us"],
      |      "properties": {
      |        "us": {
      |          "$ref": "#/definitions/b"
      |        }
      |      }
      |    }
      |  }
      |}
      |""".stripMargin.trim) shouldBe \/-("""
      |case class Reference(a:Option[reference.definitions.B])
      |case class B(us:reference.definitions.B)
      |""".stripMargin.trim)
  }

  it should "generate recursive references through multiple classes" in {
    gen("""
      |{
      |  "id": "http://some/reference",
      |  "type": "object",
      |  "properties": {
      |    "a": {
      |      "$ref": "#/definitions/b"
      |    }
      |  },
      |  "definitions": {
      |    "b": {
      |      "type": "object",
      |      "required": ["next"],
      |      "properties": {
      |        "next": {
      |          "$ref": "#/definitions/c"
      |        }
      |      }
      |    },
      |    "c": {
      |      "type": "object",
      |      "required": ["next"],
      |      "properties": {
      |        "next": {
      |          "$ref": "#/definitions/b"
      |        }
      |      }
      |    }
      |  }
      |}
      |""".stripMargin.trim) shouldBe \/-("""
      |case class Reference(a:Option[reference.definitions.B])
      |case class B(next:reference.definitions.C)
      |case class C(next:reference.definitions.B)
      |""".stripMargin.trim)
  }

  it should "expand allOf" in {
    gen("""
      |{
      |  "id": "http://some/reference",
      |  "type": "object",
      |  "properties": {
      |    "a": {
      |      "$ref": "#/definitions/a"
      |    },
      |    "b": {
      |      "$ref": "#/definitions/b"
      |    },
      |    "c": {
      |      "$ref": "#/definitions/c"
      |    }
      |  },
      |  "definitions": {
      |    "a": {
      |      "type": "object",
      |      "allOf": [
      |        {"$ref": "#/definitions/b"},
      |        {"$ref": "#/definitions/c"}
      |      ]
      |    },
      |    "b": {
      |      "type": "object",
      |      "properties": {
      |        "str": {
      |          "type": "string"
      |        }
      |      }
      |    },
      |    "c": {
      |      "type": "object",
      |      "properties": {
      |        "num": {
      |          "type": "number"
      |        }
      |      }
      |    }
      |  }
      |}
      |""".stripMargin.trim) shouldBe \/-("""
      |case class A(str:Option[String], num:Option[Double])
      |case class Reference(a:Option[reference.definitions.A], b:Option[reference.definitions.B], c:Option[reference.definitions.C])
      |case class B(str:Option[String])
      |case class C(num:Option[Double])
      |""".stripMargin.trim)
  }

  it should "create union type" in {
    gen(
      """
      |{
      |  "type": "object",
      |  "id": "root",
      |  "properties": {
      |    "thing": {
      |      "$ref": "#/definitions/thing"
      |    }
      |  },
      |  "required": ["thing"],
      |  "definitions": {
      |    "thing": {
      |      "type": "object",
      |      "additionalProperties": {
      |        "oneOf": [
      |          {"$ref": "#/definitions/a"},
      |          {"$ref": "#/definitions/b"},
      |          {"$ref": "#/definitions/c"}
      |        ]
      |      }
      |    },
      |    "a": {
      |      "type": "object",
      |      "properties": {
      |        "foo": { "type": "string" }
      |      }
      |    },
      |    "b": {
      |      "type": "object",
      |      "properties": {
      |        "bar": { "type": "string" }
      |      }
      |    },
      |    "c": {
      |      "type": "object",
      |      "properties": {
      |        "baz": { "type": "string" }
      |      }
      |    }
      |  }
      |}
      """.stripMargin) shouldBe \/-(
      s"""
      |case class C(baz:Option[String])
      |case class A(foo:Option[String])
      |case class B(bar:Option[String])
      |case class Root(thing:Either[root.definitions.A, Either[root.definitions.B, root.definitions.C]])
      """.stripMargin.trim
    )
  }

}
