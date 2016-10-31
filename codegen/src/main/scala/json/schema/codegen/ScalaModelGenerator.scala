package json.schema.codegen

import json.schema.parser.{SchemaDocument, SimpleType}

import scalaz.Scalaz._


object ScalaModelGenerator {


  private val preDefScope = ""


  val format2scala: Map[(PredefType, String), PredefType] = Map(
    (PredefType(preDefScope, "String"), "uri") -> PredefType("java.net", "URI"),
    (PredefType(preDefScope, "String"), "date-time") -> PredefType("java.util", "Date"),
    (PredefType(preDefScope, "String"), "ipv6") -> PredefType("java.net", "Inet6Address"),
    (PredefType(preDefScope, "String"), "ipv4") -> PredefType("java.net", "Inet4Address"),
    (PredefType(preDefScope, "String"), "email") -> PredefType(preDefScope, "String"),
    (PredefType(preDefScope, "String"), "hostname") -> PredefType(preDefScope, "String")
  )

  def apply[N: Numeric](schema: SchemaDocument[N]): SValidation[Set[LangType]] = {

    val json2scala: Map[SimpleType.SimpleType, PredefType] = Map(
      SimpleType.string -> PredefType(preDefScope, "String"),
      SimpleType.integer -> PredefType(preDefScope, "Long"),
      SimpleType.boolean -> PredefType(preDefScope, "Boolean"),
      // same as schema's document type param
      SimpleType.number -> PredefType(preDefScope, implicitly[Numeric[N]].zero.getClass.getSimpleName),
      SimpleType.aNull -> PredefType(preDefScope, "Any")
    )

    val generator: ModelGenerator[N] = new ModelGenerator[N](json2scala, format2scala) with ScalaNaming

    val typeName = generator.className(schema.scope).some

    generator.any(schema, typeName) map {
      t =>
        (t :: generator.definedSchemas.values.toList).toSet // to remove duplicate types
    }
  }

}
