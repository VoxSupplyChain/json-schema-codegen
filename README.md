[![CircleCI](https://circleci.com/gh/VoxSupplyChain/json-schema-codegen.svg?style=svg)](https://circleci.com/gh/VoxSupplyChain/json-schema-codegen)

# json-schema-codegen

Model and Serialization source code generator from a defined Json-Schema.

Scala code generator supports the following of Json-schema spec:

 * Object types with properties

 * required and optional properties

 * Object types with additionProperties

 * Arrays of any type

 * Unique or non-unique arrays

 * Simple types

 * Type definitions

 * Enum of string and integer values

 * Formats - uri, date-time, email, hostname, ipv4, ipv6

Not supported:

 * anyOf, not, allOf, oneOf

 * multiple types ( e.g. ```"type": ["object", "array"]``` )

 * enum of objects or variable types

 * validation constructs that do not affect the structures

## SBT Plugin  

You can run the scala code generator as part of your build.

By default the schema definitions should be placed in src/main/json-schema.

In project/plugins.sbt add:

```scala

resolvers += "releases" at "http://nexus.tundra.com/repository/maven-releases/"

addSbtPlugin("com.voxsupplychain" %% "json-schema-codegen-sbt" % "0.3.0")

```

In build.sbt :

```scala
  // json-schema to be watched for changes in ~compile
  watchSources <++= baseDirectory map { dir =>
    Seq(
      dir / "src/main/json-schema"
    )
  }
```

Code is generated during the compile phase. 

## Resources

 * JSON without JSON-schema is a "crazy mess" - GOTO 2016 • What I Wish I Had Known Before Scaling Uber to 1000 Services • Matt Ranney https://youtu.be/kb-m2fasdDY?t=870


## History

 * 0.5.0 
 
    Added support for type aliases in Typescript and Scala generators. This makes type declarations even more expressive. 
    For example: 
    
          {
            "id": "http://some/currencyCode",
            "type": "string"
          }
          // will generate 
          type CurrencyCode = String

    Support for root types. The above declaration would work in "definitions" and as an independent JSON-schema.
     