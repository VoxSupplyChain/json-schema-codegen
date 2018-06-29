package json.schema.codegen

import argonaut.Json
import json.schema.parser.{SchemaDocument, SimpleType}
import scalaz.-\/
import scalaz.std.list._
import scalaz.syntax.either._
import scalaz.syntax.std.boolean._
import scalaz.syntax.std.option._
import scalaz.syntax.traverse._

abstract class ModelGenerator[N: Numeric](json2predef: Map[SimpleType.SimpleType, PredefType], format2predef: Map[(PredefType, String), PredefType]) extends Naming {

  type Schema = SchemaDocument[N]

  val definedSchemas = scala.collection.mutable.Map.empty[Schema, LangType]

  def `object`(schema: Schema, name: Option[String]): SValidation[LangType] = {

    schema.obj.toRightDisjunction(s"not object type: ${schema.types}").flatMap {
      obj =>
        val schemaClassName: SValidation[String] = className(schema, name)

        val propertyTypes: List[SValidation[LangTypeProperty]] = obj.properties.value.map {
          case (propName, propDefinition) =>

            val existingType = definedSchemas.get(propDefinition.schema).toRightDisjunction("no type")

            val propDef = existingType orElse any(propDefinition.schema, propName.some) map {
              t =>
                LangTypeProperty(propName, propDefinition.required, t)
            }

            propDef.leftMap(e => s"Type for field ${schemaClassName.toOption}.$propName not found: $e")

        }.toList

        val propTypes: SValidation[List[LangTypeProperty]] = propertyTypes.sequence

        for {
          props <- propTypes
          className <- schemaClassName
          additional <- obj.additionalProperties.toList.map(nested => any(nested, (className + "Additional").some))
            .sequence.map(_.headOption)
        } yield {
          val newType = ClassType(packageName(schema.id.getOrElse(schema.scope)), className, props, additional)
          definedSchemas.put(schema, newType)
          newType.asInstanceOf[LangType]
        }
    }
  }

  def ref(schema: Schema): SValidation[LangType] = {
    for {
      ref <- schema.nestedSchemas.get("$ref").toRightDisjunction(s"not ref type")
    } yield ClassType(packageName(ref.id.getOrElse(ref.scope)), className(ref.id.getOrElse(ref.scope)), List.empty, Option.empty)
  }

  def array(schema: Schema, name: Option[String]): SValidation[LangType] = for {
    array <- schema.array.toRightDisjunction(s"not array type: ${schema.types}")
    arrayschema <- array.items.value.headOption.toRightDisjunction(s"items ${name.getOrElse("")} is empty: ${schema.types}")
    typeName <- className(schema, name)
    nestedName: Option[String] = name.map(_ + "0")
    nested <- any(arrayschema, nestedName)
  } yield ArrayType(packageName(schema.id.getOrElse(schema.scope)), typeName, array.uniqueItems, nested)

  def simple(schema: Schema, name: Option[String]): SValidation[LangType] = if (schema.enums.isEmpty)
    for {
      simpleType <- schema.types.headOption.flatMap(json2predef.get).toRightDisjunction("Type is not simple")
      typeName <- className(schema, name)
    } yield {
      // if there is a format, try to find type that corresponds to the format
      val formatType = schema.format.flatMap(format => format2predef.get((simpleType, format.toLowerCase))).getOrElse(simpleType)
      val finalType = if (typeName.isEmpty || schema.id.isEmpty) formatType else AliasType(packageName(schema.id.getOrElse(schema.scope)), typeName, formatType)
      definedSchemas.put(schema, finalType)
      finalType
    }
  else
    -\/("Simple but enum")

  def enum(schema: Schema, name: Option[String]): SValidation[LangType] = {

    for {
      t <- schema.types.headOption.toRightDisjunction("Type is required")
      className <- className(schema, name)
      enums <- schema.enums.isEmpty ? "Enum not defined".left[Set[Json]] | schema.enums.right[String]
      enumNestedSchema = schema.copy(enums = Set.empty, id = None)
      nestedType <- any(enumNestedSchema, (className + "Value").some)
    } yield {

      val enumNested = t match {
        case SimpleType.string => enums.flatMap(_.string)
        case SimpleType.number => enums.flatMap(_.number.map(_.truncateToDouble))
        case SimpleType.integer => enums.flatMap(_.number.map(n => n.toLong.getOrElse(n.truncateToLong)))
        case SimpleType.boolean => enums.flatMap(_.bool)
      }

      val newType = EnumType(packageName(schema.scope), className, nestedType, enumNested)
      definedSchemas.put(schema, newType)
      newType
    }

  }


  def any(schema: Schema, name: Option[String]): SValidation[LangType] = {
    if (schema.types.size != 1)
      ref(schema) orElse s"One type is required in: $schema".left
    else
      enum(schema, name) orElse array(schema, name) orElse `object`(schema, name) orElse simple(schema, name) orElse -\/(s"Unclear definition: ${name.orEmpty}")
  }

}
