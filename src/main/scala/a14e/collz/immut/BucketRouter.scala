package a14e.collz.immut

import scala.collection.immutable.TreeSet
import scala.util.Try


object BucketRouter {
  def apply[T: Ordering](size: Int): BucketRouter[T] =
    new BucketRouter(Vector.fill(size)(Bucket.empty[T]))

  def empty[T: Ordering]: BucketRouter[T] = BucketRouter[T](Vector.empty[Bucket[T]])

  def apply[T: Ordering](masterAndSlaves: (T, TraversableOnce[T])*): BucketRouter[T] = {
    val buckets = masterAndSlaves.map(Bucket[T]).toVector
    BucketRouter(buckets)
  }
}

/**
  * Created by Borisenko Andrew on 03.02.2017.
  */
case class BucketRouter[T: Ordering](buckets: Vector[Bucket[T]]) {

  def bucketNumber: Int = buckets.length

  private def testIndex(idx: Int): Unit = {
    if (idx < 0 || idx >= bucketNumber)
      throw new IndexOutOfBoundsException(s"invalid index of bucket: $idx, expected 0...$bucketNumber")
  }

  def bucketAt(idx: Int): Bucket[T] = {
    testIndex(idx)
    buckets(idx)
  }

  def route(key: Any): T = {
    if (buckets.isEmpty)
      throw new IllegalArgumentException("route on empty router")
    val idx = indexByHash(key)
    buckets(idx) match {
      case Bucket(None, _) => throw new IllegalArgumentException("route to empty bucket")
      case Bucket(Some(el), _) => el
    }
  }

  def routeOption(key: Any): Option[T] = Try(route(key)).toOption

  def indexByHash(key: Any): Int = {
    val hash = Router.improveHash(key.hashCode())
    val idx = (hash % bucketNumber).abs
    idx
  }

  def bucketForKey(key: Any): Bucket[T] = {
    val idx = indexByHash(key)
    bucketAt(idx)
  }

  def bucketIndexByElem(el: T): Option[Int] = {
    buckets.indexWhere(_.nodes(el)) match {
      case -1 => None
      case i => Some(i)
    }
  }

  def bucketByElem(el: T): Option[Bucket[T]] = {
    bucketIndexByElem(el).map(bucketAt)
  }

  def emptyBucketIndex(): Option[Int] =
    buckets.indexWhere(_.isEmpty) match {
      case -1 => None
      case i => Some(i)
    }

  def withNewBucket(idx: Int, newBucket: Bucket[T]): BucketRouter[T] = {
    testIndex(idx)
    val newBuckets = buckets.updated(idx, newBucket)
    BucketRouter[T](newBuckets)
  }


  def withEll(idx: Int, el: T): BucketRouter[T] = {
    val newBucket = bucketAt(idx).withNode(el)
    withNewBucket(idx, newBucket)
  }

  def withoutEll(idx: Int, el: T): BucketRouter[T] = {
    val newBucket = bucketAt(idx).withoutNode(el)
    withNewBucket(idx, newBucket)
  }

  def withMaster(idx: Int, el: T): BucketRouter[T] = {
    val newBucket = bucketAt(idx: Int).withMaster(el)
    withNewBucket(idx, newBucket)
  }
}

case class Bucket[T: Ordering](master: Option[T], nodes: TreeSet[T]) {

  def isEmpty: Boolean = master.isEmpty

  private lazy val router = new Router(nodes)

  def routeByKey(key: Any): T = router.route(key)

  def withNode(el: T): Bucket[T] =
    master match {
      case None => Bucket(Some(el), TreeSet(el))
      case Some(_) => Bucket(master, nodes + el)
    }

  def +(el: T): Bucket[T] = withNode(el)

  def withMaster(el: T): Bucket[T] = {
    Bucket(Some(el), nodes + el)
  }

  def withoutNode(el: T): Bucket[T] = master match {
    case None => this
    case Some(x) if x == el =>
      if (nodes.size == 1) Bucket.empty[T]
      else {
        val newNodes = nodes - el
        Bucket[T](newNodes.headOption, newNodes)
      }
    case Some(_) => Bucket[T](master, nodes - el)
  }

  def -(el: T): Bucket[T] = withoutNode(el)
}

object Bucket {
  def empty[T: Ordering] = Bucket(None, TreeSet.empty[T])


  def apply[T: Ordering](masterSlaves: (T, TraversableOnce[T])): Bucket[T] = {
    val (master, slaves) = masterSlaves
    Bucket(Some(master), slaves.to[TreeSet] + master)
  }
}
