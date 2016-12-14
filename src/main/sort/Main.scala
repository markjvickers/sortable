package sort

import scala.io.BufferedSource
import spray.json._
import ListingProtocol._
import ProductProtocol._
import MatchProtocol._
import sort.products.Products

/**
 * Sortable Challenge
 */
object Main extends App {

  val PRODUCT_FILE = 0
  val LISTINGS_FILE = 1
  val RESULT_FILE = 2

  val listings = loadListings
  val products = loadProducts
  save(products.getGroupedMatches(listings))

  private def loadListings = load[Listing](LISTINGS_FILE).filter(_.hasValidPrice)
  private def loadProducts = Products.apply(load[Product](PRODUCT_FILE))
  private def save(matches: Seq[Match]) = write(RESULT_FILE, matches)

  private def write[T](argNum: Int, list: Seq[T])(implicit w: JsonWriter[T]) = {
    val toWrite = list.par.map(_.toJson.toString + "\n").toList
    import java.io._
    val file = new File(args(argNum))
    val pw = new PrintWriter(file)
    toWrite.foreach(pw.write(_))
    pw.flush
    pw.close
  }

  private def load[T](argNum: Int)(implicit r: JsonReader[T]): Seq[T] = {
    io.Source.fromFile(args(argNum))
      .getLines().toList.par
      .map(_.parseJson.convertTo[T])
      .toList
  }

}