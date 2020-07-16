package json.schema.codegen

import java.net.URI

import org.scalatest.{FlatSpec, Matchers}

class ScalaNamingTest extends FlatSpec with Matchers with ScalaNaming {

  "ScalaNaming" should "name package" in {
    packageName(new URI("http://host/b/c#")) shouldBe "b"
    packageName(new URI("http://host/b/c#")) shouldBe "b"
    packageName(new URI("http://host/b/c#/d")) shouldBe "b.c"
    packageName(new URI("#/d/e")) shouldBe "d"
    packageName(new URI("a#d/e")) shouldBe "a.d"
    packageName(new URI("file:/a/b/c#d/e")) shouldBe "c.d"
    packageName(new URI("a-b-c#d/e")) shouldBe "a.b.c.d"
  }

  it should "name class" in {
    className(new URI("http://host/b/c#/d")) shouldBe "D"
    className(new URI("#/d/e")) shouldBe "E"
    className(new URI("a#/d/e")) shouldBe "E"
    className(new URI("file:a#d/e")) shouldBe "E"
    className(new URI("file:a#class")) shouldBe "Class"
    className(new URI("a-b-c")) shouldBe "C"
    className(
      new URI(
        "file:/Users/todor/Documents/vox/json-schema-codegen/examples/src/main/json-schema/vox-buy-model-shop-requisition.json#/definitions/line"
      )
    ) shouldBe "Line"
  }

}
