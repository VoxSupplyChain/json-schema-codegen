package json.schema.codegen

import argonaut.Argonaut._
import argonaut.{CodecJson, DecodeResult, HCursor}
import org.scalatest.{FlatSpec, Matchers}

class CodecTest extends FlatSpec with Matchers {

  "Argonaut" should "encode/decode fields having a scala keyword name" in {

    object Mime extends Enumeration {
      val text = Value("text")
      val file = Value("file")
    }

    implicit val mimeCodec: CodecJson[Mime.Value] =
      CodecJson[Mime.Value](
        (v: Mime.Value) => v.toString.asJson,
        (j: HCursor) =>
          j.as[String].flatMap { s: String =>
            try DecodeResult.ok(Mime.withName(s))
            catch {
              case _: NoSuchElementException => DecodeResult.fail("_type", j.history)
            }
          }
      )

    case class SpecialFields(_type: Mime.Value, message: String)

    // when want to encode/decode based on types only and eventually different fields names, use case codecs
    implicit val testCodec = CodecJson.casecodec2(SpecialFields.apply, SpecialFields.unapply)("type", "message")

    // when want to encode/decode based on field names and types use derived option
    //import ArgonautShapeless._
    //implicit val testCodec = CodecJson.derived(EncodeJson.of[SpecialFields], DecodeJson.of[SpecialFields])

    val input   = "{\"type\":\"text\",\"message\":\"aloha\"}"
    val payload = input.decodeOption[SpecialFields].getOrElse(sys.error("unable to decode"))

    payload._type shouldBe Mime.text
    payload.message shouldBe "aloha"
  }
}
