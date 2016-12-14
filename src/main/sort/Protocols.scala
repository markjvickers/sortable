/**
 * Protocols for converting model to/from JSON
 */
package sort

import spray.json._
import DefaultJsonProtocol._
import java.util.Date
import javax.xml.bind.DatatypeConverter

object ProductProtocol extends DefaultJsonProtocol {
  implicit object ProductJsonFormat extends RootJsonFormat[Product] {

    def write(p: Product) = throw new DeserializationException("Not Supported")

    def read(value: JsValue) = {
      val o = value.asJsObject
      val family = o.getFields("family") match {
        case Seq(JsString(f)) => Some(f)
        case _                => None
      }
      o.getFields("product_name", "manufacturer", "model", "announced-date") match {
        case Seq(JsString(name), JsString(manufacturer), JsString(model), JsString(announced)) =>
          Product(name, manufacturer, model, family, toDate(announced))
        case x =>
          throw new DeserializationException("Product expected")
      }
    }

    private def toDate(date: String): Option[Date] = {
      try {
        return Some(DatatypeConverter.parseDateTime(date).getTime())
      } catch {
        case _: Throwable => return None
      }
    }

  }
}

object ListingProtocol extends DefaultJsonProtocol {
  implicit object ListingJsonFormat extends RootJsonFormat[Listing] {

    def write(l: Listing) = JsObject(
      "title" -> JsString(l.title),
      "manufacturer" -> JsString(l.manufacturer),
      "currency" -> JsString(l.currency),
      "price" -> JsString(toString(l.price.get)))

    def read(value: JsValue) = {
      value.asJsObject.getFields("title", "manufacturer", "currency", "price") match {
        case Seq(JsString(title), JsString(manufacturer), JsString(currency), JsString(price)) =>
          Listing(title, manufacturer, currency, toFloat(price))
        case _ =>
          throw new DeserializationException("Listing expected")
      }
    }
    
    private def toString(float: Float): String = f"$float%.2f"
    
    private def toFloat(floatStr: String): Option[Float] = {
      try {
        return Some(floatStr.toFloat)
      } catch {
        case _: Throwable => return None
      }
    }

  }
}

import ListingProtocol._
object MatchProtocol extends DefaultJsonProtocol {
  implicit object MatchJsonFormat extends RootJsonFormat[Match] {

    private def doWrite(m: Match)(implicit w: JsonWriter[Listing]) = JsObject(
      "product_name" -> JsString(m.product.name),
      "listings" -> JsArray(m.listings.map { _.toJson }))

    def write(m: Match) = doWrite(m)

    def read(value: JsValue) = throw new DeserializationException("Not Supported")

  }
}