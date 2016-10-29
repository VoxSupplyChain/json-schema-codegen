package json.schema.codegen

import json.schema.parser.{SchemaDocument, SimpleType}

import scalaz.Failure
import scalaz.Scalaz._


object TypeScriptModelGenerator {

  private val preDefScope = ""

  // JS parser of JSON doesn't parse custom complex objects, so format to type conversion is not supported.
  val format2ts: Map[(PredefType, String), PredefType] = Map()

  val json2ts: Map[SimpleType.SimpleType, PredefType] = Map(
    SimpleType.string -> PredefType(preDefScope, "string"),
    SimpleType.integer -> PredefType(preDefScope, "number"),
    SimpleType.boolean -> PredefType(preDefScope, "boolean"),
    SimpleType.number -> PredefType(preDefScope, "number"),
    SimpleType.aNull -> PredefType(preDefScope, "any")
  )

  def apply[N: Numeric](schema: SchemaDocument[N]): SValidation[Set[LangType]] = {

    val generator: ModelGenerator[N] = new ModelGenerator[N](json2ts, format2ts) with TypeScriptNaming {
      override def enum(schema: Schema, name: Option[String]): SValidation[LangType] = "enum not supported".left
    }

    val typeName = generator.className(schema.scope).some

    generator.any(schema, typeName) map {
      t =>
        (t :: generator.definedSchemas.values.toList).toSet // to remove duplicate types
    }
  }

}
