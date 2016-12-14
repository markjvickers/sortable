package sort
package products

import scala.annotation.tailrec
import scala.collection.parallel._

trait Products {
  def getGroupedMatches(listings: Seq[Listing]): Seq[Match]
}

object Products {
  def apply(products: Seq[Product]): Products = ProductTree(products)
}

/**
 * For grouping products hierarchically to assist matching with Listings.
 */
private[products] object ProductTree {

  def apply(products: Seq[Product]): ProductTree = {
    @tailrec
    def make(p: ProductTree, list: Seq[Product]): ProductTree = {
      list match {
        case Nil          => p
        case head :: tail => make(p.addProduct(head), tail)
      }
    }
    make(ProductTree(), products).normalize
  }

  case class TokenBag(set: Set[String]) {
    def hasAll(bag: TokenBag): Boolean = set.forall(bag.set contains)
  }

  object TokenBag {

    def apply(seed: Option[String]): TokenBag = seed match {
      case Some(s) => apply(s)
      case None    => TokenBag(Set.empty[String])
    }

    def apply(seed: String): TokenBag = TokenBag(asTokens(seed))

    private def asTokens(s: String): Set[String] =
      s.split(Array('-', '_', ' ')).map(_.toLowerCase).toSet

  }

  trait Node

  trait TokenNode[T] extends Node with TokenHolder {
    def addProduct(product: Product): T
    def normalize(): Option[T]
  }

  trait TokenHolder {
    val tokens: TokenBag
    def matches(t: TokenBag) = tokens.hasAll(t)
  }

  trait Branch[T <: TokenNode[T]] extends Node {

    val children: Set[T]
    val childPropertyLookup: Product => Option[String]
    val childMaker: (TokenBag, Product) => T
    val map: Map[TokenBag, T] = children.map(t => (t.tokens, t)).toMap

    def makeChildren(product: Product): Set[T] = {
      val tokens = toTokens(product)
      val existing = map.get(tokens)
      val child = existing match {
        case Some(c) => c.addProduct(product)
        case None    => childMaker(tokens, product)
      }
      val set = existing match {
        case Some(c) => (children - c)
        case None    => children
      }
      set + child
    }

    def toTokens(p: Product) = TokenBag(childPropertyLookup(p))

  }

  trait BranchTokenNode[T <: TokenNode[T], U <: Branch[T]] extends Branch[T] with TokenNode[U] {
    val copier: Set[T] => U
    def normalize() = {
      val n = copier(children.flatMap { _.normalize })
      if (n.children.isEmpty) None else Some(n)
    }
    def addProduct(product: Product) = copier(makeChildren(product))
  }

  trait Leaf extends Node { val products: Set[Product] }

  case class ProductTree(children: Set[Manufacturer] = Set.empty) extends Branch[Manufacturer] with Products {
    val childPropertyLookup = (p: Product) => Some(p.manufacturer)
    val childMaker = (t: TokenBag, p: Product) => Manufacturer(tokens = t)
    def addProduct(product: Product) = copy(children = makeChildren(product))
    def normalize = copy(children = children.flatMap { _.normalize })

    def getMatch(listing: Listing): Option[Product] = {
      val manufacturerTokens = TokenBag(listing.manufacturer)
      val titleTokens = TokenBag(listing.title)
      children.foldLeft[Option[Product]](None)((o, m) => if (o.isDefined) o else m.getMatch(manufacturerTokens, titleTokens))
    }

    def getGroupedMatches(listings: Seq[Listing]): Seq[Match] =
      getMatches(listings.par)
        .groupBy(_.product)
        .mapValues(_.map(_.listing))
        .map(e => Match(e._1, e._2.toList))
        .map(m => m.removeOutliers())
        .filter(m => !m.listings.isEmpty).toList

    private def getMatches(listings: ParSeq[Listing]) =
      listings.map(l => (l, getMatch(l)))
        .filter(t => t._2.isDefined)
        .map(t => SingleMatch(t._1, t._2.get))

    private case class SingleMatch(listing: Listing, product: Product)
  }

  case class Manufacturer(children: Set[Family] = Set.empty, tokens: TokenBag) extends BranchTokenNode[Family, Manufacturer] {
    val childPropertyLookup = (p: Product) => p.family
    val childMaker = (t: TokenBag, p: Product) => Family(tokens = t)
    val copier = (set: Set[Family]) => copy(children = set)
    def getMatch(manufacturerTokens: TokenBag, titleTokens: TokenBag): Option[Product] =
      if (matches(manufacturerTokens))
        children.foldLeft[Option[Product]](None)((o, m) => if (o.isDefined) o else m.getMatch(titleTokens))
      else
        None
  }

  case class Family(children: Set[Model] = Set.empty, tokens: TokenBag) extends BranchTokenNode[Model, Family] {
    val childPropertyLookup = (p: Product) => Some(p.model)
    val childMaker = (t: TokenBag, p: Product) => Model(t, Set(p))
    val copier = (set: Set[Model]) => copy(children = set)
    def getMatch(t: TokenBag): Option[Product] =
      if (matches(t))
        children.collectFirst({ case m if m.matches(t) => m.getProduct() }).flatten
      else
        None
  }

  case class Model(tokens: TokenBag, products: Set[Product]) extends Leaf with TokenNode[Model] {
    override def addProduct(product: Product) = copy(products = products + product)
    override def normalize(): Option[Model] = if (products.size == 1) Some(this) else None
    def getProduct() = if (products.size == 1) Some(products.iterator.next) else None
  }

}