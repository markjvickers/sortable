/**
 * Product, Listing and Match case classes.
 */
package sort
import java.util.Date

case class Product(name: String, manufacturer: String, model: String, family: Option[String], announcedDate: Option[Date])

case class Listing(title: String, manufacturer: String, currency: String, price: Option[Float]) {

  def getNormalizedPrice(): Option[Float] =
    price.flatMap(f => CurrencyConverter.asCanadian(f, currency))

  def hasValidPrice(): Boolean = getNormalizedPrice().isDefined

}

case class Match(product: Product, listings: List[Listing]) {

  private val REJECTION_THRESHOLD = .3

  def removeOutliers() = {
    val avg = getAvgPrice
    copy(listings = listings.filter(!isWithinVariance(_, avg)))
  }

  // lifted from sample
  private def isWithinVariance(listing: Listing, avg: Float) = {
    val variance = avg * REJECTION_THRESHOLD
    val min = avg - variance
    val max = avg + variance
    val price = listing.getNormalizedPrice.get
    price < min || price > max
  }

  private def getAvgPrice(): Float = {
    val prices = listings.flatMap(_.getNormalizedPrice())
    prices.sum / prices.size
  }

}

// lifted from sample
private object CurrencyConverter {

  private val map = Map[String, Float]()
    .+("EUR" -> 1.22361f)
    .+("USD" -> 0.992594f)
    .+("GBP" -> 1.55627f)
    .+("INR" -> 0.0179352f)
    .+("AUD" -> 1.04357f)
    .+("SGD" -> 0.796451f)
    .+("CAD" -> 1f)

  def asCanadian(amount: Float, currency: String): Option[Float] =
    map.get(currency).map(_ * amount)
}