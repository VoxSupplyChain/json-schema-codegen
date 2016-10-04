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

resolvers ++= Seq(
  Resolver.url("vox-public-ivy", url("http://dl.bintray.com/content/voxsupplychain/ivy-public"))(Resolver.ivyStylePatterns)
)

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

