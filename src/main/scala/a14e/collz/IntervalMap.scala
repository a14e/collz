package a14e.collz

import java.util

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.ListBuffer
import scala.collection.{Iterable, mutable}


/**
  * Created by Andrew on 14.04.2016.
  */

object IntervalMap {


  /** $genericCanBuildFromInfo */
  implicit def canBuildFrom[A: Ordering, B]: CanBuildFrom[TraversableOnce[Interval[A, B]], Interval[A, B], IntervalMap[A, B]] =

    new CanBuildFrom[TraversableOnce[Interval[A, B]], Interval[A, B], IntervalMap[A, B]] {

      override def apply(from: TraversableOnce[Interval[A, B]]): mutable.Builder[Interval[A, B], IntervalMap[A, B]] =
        newBuilder[A, B]

      override def apply(): mutable.Builder[Interval[A, B], IntervalMap[A, B]] = newBuilder[A, B]
    }


  def newBuilder[A: Ordering, B]: mutable.Builder[Interval[A, B], IntervalMap[A, B]] =
    new mutable.Builder[Interval[A, B], IntervalMap[A, B]] {
      private val ord = implicitly[Ordering[A]]

      lazy val tree = new java.util.TreeMap[A, Interval[A, B]](ord)

      override def +=(elem: Interval[A, B]): this.type = {
        if (tree.isEmpty) {
          tree.put(elem.right, elem)
        } else {
          addIntervalToTree(tree, elem)
        }
        this
      }

      override def result(): IntervalMap[A, B] = new IntervalMap(tree)

      override def clear(): Unit = tree.clear()
    }

  def apply[A: Ordering, B]() = newBuilder[A, B].result()

  def apply[A, B](first: ((A, A), B), others: ((A, A), B)*)(implicit ord: Ordering[A]): IntervalMap[A, B] = {
    val intervals = others.iterator.map(x => Interval(x))
    val builder = newBuilder[A, B]
    builder += Interval(first)
    builder ++= intervals
    builder.result()
  }

  def apply[A, B](first: Interval[A, B], others: Interval[A, B]*)(implicit ord: Ordering[A]): IntervalMap[A, B] = {
    val builder = newBuilder[A, B]
    builder += first
    builder ++= others
    builder.result()
  }

  def fill[A, B](intervals: TraversableOnce[A])(value: => B)(implicit ord: Ordering[A]): IntervalMap[A, B] = {
    val builder = newBuilder[A, B]
    if (intervals.isEmpty)
      builder.result()
    else {
      val iter = intervals.toIterator
      var previous = iter.next()
      if (iter.isEmpty)
        builder.result()
      else {
        while (iter.hasNext) {
          import ord._
          val current = iter.next()
          if (current <= previous)
            throw new IllegalArgumentException("cant create interval list: data should be sorted")

          builder += Interval(previous, current, value)
          previous = current
        }
        builder.result()
      }
    }
  }


  def fillByData[A, B](intervals: TraversableOnce[A])
                      (data: TraversableOnce[B])(implicit ord: Ordering[A]): IntervalMap[A, B] =
    if (data.isEmpty)
      newBuilder[A, B].result()
    else {
      val it = data.toIterator
      fill[A, B](intervals) {
        if (!it.hasNext)
          throw new IllegalArgumentException("cant create interval list: too small data size for these intervals.")
        it.next()
      }
    }

  def fillIntervals[A, B](intervals: A*)
                         (value: => B)(implicit ord: Ordering[A]): IntervalMap[A, B] = fill[A, B](intervals)(value)

  def fillIntervalsByData[A, B](intervals: A*)
                               (data: TraversableOnce[B])
                               (implicit ord: Ordering[A]): IntervalMap[A, B] = fillByData[A, B](intervals)(data)

  def splitByIntervals[A](intervals: TraversableOnce[A])
                         (data: TraversableOnce[A])(implicit ord: Ordering[A]): IntervalMap[A, List[A]] = {
    val map = fill[A, List[A]](intervals)(Nil)
    data.foreach(x => map.applyIfExist(x)(list => x :: list))
    map
  }

  def countByIntervals[A](intervals: TraversableOnce[A])
                         (data: TraversableOnce[A])(implicit ord: Ordering[A]): IntervalMap[A, Int] = {
    val map = fill[A, Int](intervals)(0)
    data.foreach(x => map.applyIfExist(x)(_ + 1))
    map
  }


  /** добавляет интервал к дереву, проверяем на пересечения и
    * записываем поверх прошлых значений,
    * если есть пересечения */
  protected def addIntervalToTree[A, B](tree: util.TreeMap[A, Interval[A, B]],
                                        interval: Interval[A, B]): util.TreeMap[A, Interval[A, B]] =
    if (tree.isEmpty) {
      tree.put(interval.right, interval)
      tree
    } else {
      lazy val toRemoveList = new ListBuffer[Interval[A, B]]()
      val toAddList = new ListBuffer[Interval[A, B]]()
      toAddList += interval
      val subTree = tree.tailMap(interval.left)
      val iterator = subTree.entrySet().iterator()

      import interval.ord._
      var continue = true
      var wasInWhile = false
      while (continue && iterator.hasNext) {
        wasInWhile = true

        val current = iterator.next().getValue
        if (!equiv(current.right, interval.left)) {
          if (interval.right <= current.left)
            continue = false
          else {
            toRemoveList += current
            if (current.left < interval.left)
              toAddList += current.withNewRight(interval.left)
            else if (current.right > interval.right)
              toAddList += current.withNewLeft(interval.right)
          }
        }
      }

      /** если заходили в цикл */
      if (wasInWhile)
        toRemoveList.foreach(x => tree.remove(x.right))

      toAddList.foreach(x => tree.put(x.right, x))

      tree
    }


}

class IntervalMap[A, B] private(val rightBoundsTree: java.util.TreeMap[A, Interval[A, B]])
  extends Iterable[Interval[A, B]] {
  override def iterator: Iterator[Interval[A, B]] =
    if (isEmpty) Iterator.empty
    else new Iterator[Interval[A, B]] {
      private val it = rightBoundsTree.entrySet().iterator()

      override def hasNext: Boolean = it.hasNext

      override def next(): Interval[A, B] = if (it.hasNext) it.next().getValue else Iterator.empty.next()
    }


  lazy val length: Int = rightBoundsTree.size()

  override def size = length

  override def isEmpty = rightBoundsTree == null || rightBoundsTree.isEmpty

  def apply(key: A): Option[B] = get(key)

  protected def withFind[C](key: A)(block: Interval[A, B] => C): Option[C] =
    if (isEmpty) None
    else {
      val found = rightBoundsTree.ceilingEntry(key)
      if (found == null)
        None
      else {
        val interval = found.getValue
        import interval.ord._
        if (key < interval.left) None
        else Some(block(interval))
      }
    }

  def get(key: A): Option[B] = withFind(key)(_.value)

  def +(elem: Interval[A, B]): IntervalMap[A, B] = {
    import elem.ord
    IntervalMap.newBuilder[A, B] ++= this += elem
  }.result()

  def contains(key: A): Boolean = get(key).isDefined

  def update(key: A, value: => B): Boolean =
    withFind(key) { interval =>
      rightBoundsTree.put(interval.right, interval.withValue(value))
    }.isDefined

  def applyIfExist(key: A)(f: B => B): Boolean =
    withFind(key) { interval =>
      rightBoundsTree.put(interval.right, interval.map(f))
    }.isDefined

  override def equals(other: Any): Boolean = other match {
    case x: Array[Interval[A, B]] =>
      size == x.length && {
        isEmpty ||
          x.iterator.zip(iterator).forall { case (l, r) => l.equals(r) }
      }
    case _ => super.equals(other)
  }

  override def toString = mkString("IntervalMap(", ", ", ")")
}


object Interval {

  def apply[A, B](data: ((A, A), B))(implicit ord: Ordering[A]): Interval[A, B] = {
    val ((left, right), value) = data
    apply(left, right, value)
  }

  def apply[A, B](left: A, right: A, value: B)(implicit ord: Ordering[A]): Interval[A, B] =
    new NonEmptyInterval[A, B](left, right, value)

  def unapply[A, B](x: Interval[A, B]): Option[(A, A, B)] = Some((x.left, x.right, x.value))


  class NonEmptyInterval[A, B](val left: A, val right: A, val value: B)
                              (implicit val ord: Ordering[A]) extends Interval[A, B] {
    def withNewRight(newRight: A): Interval[A, B] = Interval[A, B](left, newRight, value)

    def withNewLeft(newLeft: A): Interval[A, B] = Interval[A, B](newLeft, right, value)

    override def toString = "Interval(left: " + left + ", right: " + right + ", value: " + value + ")"

    override def map(f: (B) => B): Interval[A, B] = withValue(f(value))
  }

}

trait Interval[A, B] {
  implicit val ord: Ordering[A]
  val left: A
  val right: A

  def value: B


  def withNewRight(newRight: A): Interval[A, B]

  def withNewLeft(newLeft: A): Interval[A, B]

  def withValue(newValue: B): Interval[A, B] = Interval(left, right, newValue)

  import ord._

  def hasValue(x: A) = left <= x && x <= right

  def map(f: B => B): Interval[A, B]

  override def equals(other: Any): Boolean = other match {
    case x: Interval[A, B] =>
      equiv(left, x.left) &&
        equiv(right, x.right) &&
        value == x.value
    case _ => false
  }
}