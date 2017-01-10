package a14e.collz.mut

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable


object PrefixSet {

  implicit def canBuildFrom = new CanBuildFrom[mutable.Set[_], String, PrefixSet] {

    def apply(): mutable.Builder[String, PrefixSet] = PrefixSet()

    override def apply(from: mutable.Set[_]): mutable.Builder[String, PrefixSet] = apply()
  }

  def apply(xs: String*): PrefixSet = new PrefixSet(PrefixMap[Unit]()) ++= xs
}

/**
  * Created by User on 09.01.2017.
  */
class PrefixSet private[collz](val underlying: PrefixMap[Unit])
  extends mutable.Set[String] with mutable.Builder[String, PrefixSet] {

  override def size: Int = underlying.size

  override def isEmpty: Boolean = underlying.isEmpty

  override def newBuilder: mutable.Builder[String, mutable.Set[String]] = PrefixSet()

  override def result(): PrefixSet = this

  override def clear(): Unit =  {
    underlying.clear()
  }

  override def +=(elem: String): PrefixSet.this.type = {
    underlying(elem) = Unit
    this
  }

  override def -=(elem: String): PrefixSet.this.type = {
    underlying -= elem
    this
  }

   def hasPrefix(prefix: String): Boolean = underlying.hasPrefix(prefix)

  def findForPrefix(prefix: String): Iterator[String] = underlying.findForPrefix(prefix).map(_._1)

  override def contains(elem: String): Boolean = underlying.contains(elem)

  override def iterator: Iterator[String] = underlying.keysIterator

  override def foreach[U](f: (String) => U): Unit = for ((k, _) <- underlying) f(k)

}
