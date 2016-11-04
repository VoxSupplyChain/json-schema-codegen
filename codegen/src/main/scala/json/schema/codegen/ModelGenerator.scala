package json.schema.codegen

import argonaut.Json
import json.schema.parser.{Property, SchemaDocument, SimpleType}

import scalaz.Scalaz._

abstract class ModelGenerator[N: Numeric](json2predef: Map[SimpleType.SimpleType, PredefType], format2predef: Map[(PredefType, String), PredefType]) extends Naming {

  type Schema = SchemaDocument[N]

  val definedSchemas = scala.collection.mutable.Map.empty[Schema, LangType]

  def `object`(schema: Schema, name: Option[String]): SValidation[LangType] = {

    schema.obj.toRightDisjunction(s"not object type: ${schema.types}").flatMap {
      obj =>
        val schemaClassName: SValidation[String] = className(schema, name)

        def buildProp(propName: String, propDefinition: Property[N], parentPrefix: Option[String]): SValidation[LangTypeProperty] = {
            val existingType = definedSchemas.get(propDefinition.schema).toRightDisjunction("no type")

            val propDef = existingType orElse any(propDefinition.schema, propName.some, parentPrefix) map {
              t =>
                LangTypeProperty(propName, propDefinition.required, t)
            }

            propDef.leftMap(e => s"Type for field ${schemaClassName.toOption}.$propName not found: $e")
        }

        for {
          className <- schemaClassName
          props <- {
            val propertyTypes: List[SValidation[LangTypeProperty]] = obj.properties.value.map({ case (name, definition) => buildProp(name, definition, Some(className)) }).toList
            val allOfPropTypes: List[SValidation[LangTypeProperty]] = schema.allOf.flatMap(_.obj.map(_.properties.value.map({ case (name, definition) => buildProp(name, definition, None) }).toList)).flatten

            (propertyTypes ++ allOfPropTypes).sequence
          }
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

  def array(schema: Schema, name: Option[String]): SValidation[LangType] = {
    schema.array.toRightDisjunction(s"not array type: ${schema.types}").flatMap {
      array =>
        val genClassName: Option[String] = name.map(_ + "0")
        val arrayDef = any(array.items.value.head, genClassName) map {
          nested =>
            ArrayType(packageName(schema.id.getOrElse(schema.scope)), array.uniqueItems, nested)
        }

        arrayDef.leftMap(e => s"Type of Array $genClassName not found: $e")
    }
  }

  def simple(schema: Schema): SValidation[LangType] = {
    schema.types.headOption.flatMap(json2predef.get).toRightDisjunction("Type is not simple") map {
      simpleType =>
        // if there is a format, try to find type that corresponds to the format
        val formatType = schema.format.flatMap(format => format2predef.get((simpleType, format.toLowerCase))).getOrElse(simpleType)

        definedSchemas.put(schema, formatType)
        formatType
    }
  }

  def enum(schema: Schema, name: Option[String], parentPrefix: Option[String] = None): SValidation[LangType] = {

    for {
      t <- schema.types.headOption.toRightDisjunction("Type is required")
      className <- className(schema, name, parentPrefix)
      enums: Set[Json] <- schema.enums.isEmpty ? "Enum not defined".left[Set[Json]] | schema.enums.right[String]
      enumNestedSchema = schema.copy(enums = Set.empty)
      nestedType <- any(enumNestedSchema, (className + "Value").some)
    } yield {

      val enumNested = t match {
        case SimpleType.string => enums.flatMap(_.string.toSet)
        case SimpleType.number => enums.flatMap(_.number.map(_.toDouble).toSet)
        case SimpleType.integer => enums.flatMap(_.number.map( n=> n.toLong.getOrElse(n.toDouble.toLong)).toSet)
        case SimpleType.boolean => enums.flatMap(_.bool.toSet)
      }

      val newType = EnumType(packageName(schema.scope), className, nestedType, enumNested)
      definedSchemas.put(schema, newType)
      newType
    }

  }

  def oneOf(schema: Schema, name: Option[String]): SValidation[LangType] = {
    (for {
      obj <- schema.obj
      additionalProperties <- obj.additionalProperties
      if additionalProperties.oneOf.nonEmpty
      clazzName <- className(schema, name).toOption
      downstream <- additionalProperties.oneOf.map(any(_, None)).sequence.toOption
    } yield {
      UnionType(packageName(schema.scope), clazzName, downstream)
    }).toRightDisjunction("Unable to find additionalProperties")
  }

  def any(schema: Schema, name: Option[String], parentPrefix: Option[String] = None): SValidation[LangType] = {
    if (schema.types.size != 1)
      ref(schema) orElse s"One type is required in: $schema".left
    else
      oneOf(schema, name) orElse enum(schema, name, parentPrefix) orElse array(schema, name) orElse `object`(schema, name) orElse simple(schema)
  }

}
