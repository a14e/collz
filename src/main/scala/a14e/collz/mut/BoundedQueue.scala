/*
* This source code is licensed under the MIT license found in the
* LICENSE.txt file in the root directory of this source tree
*/
package a14e.collz.mut

import scala.collection.{AbstractIterator, mutable}
import scala.collection.mutable.ListBuffer

trait Queue[T] extends Traversable[T] with Iterable[T] {
  def push(value: T): Queue[T]

  def pushValues(values: T*): Queue[T]

  def pushAll(values: TraversableOnce[T]): Queue[T]

  def pull(): T

  def pullOption(): Option[T]


  def pullAll(count: Int): Seq[T]

  def pullWhile(cond: T => Boolean): Seq[T]

  def +=(elem: T): Queue[T]

  def ++=(elems: TraversableOnce[T]): Queue[T]

  def apply(idx: Int): T

  def isEmpty: Boolean

  def length: Int

  def clear(): Unit

}

object BoundedQueue {
  def apply[T](maxSize: Int): BoundedQueue[T] = {
    if (maxSize <= 0)
      throw new IllegalArgumentException(s"$maxSize: BoundedQueue should have maxSize > 0")
    new BoundedQueue[T](maxSize)
  }

  def fill[T](maxSize: Int)(el: => T): BoundedQueue[T] = {
    val res = BoundedQueue[T](maxSize)
    var counter = maxSize
    while (counter != 0) {
      res += el
      counter -= 1
    }
    res
  }

  def of[T](xs: T*): Queue[T] = BoundedQueue[T](xs.size) ++= xs
}


class BoundedQueue[T] private[collz](capacity: Int) extends Queue[T] {
  self =>
  private var _size: Int = 0

  private var read: Array[Any] = new Array[Any](capacity)
  private var readOffset: Int = 0
  private var readEnd: Int = 0

  private var write: Array[Any] = new Array[Any](capacity)
  private var writeEnd: Int = 0


  override def stringPrefix: String = "BoundedQueue"

  override def clear(): Unit = {
    _size = 0
    read = new Array[Any](capacity)
    readOffset = 0
    readEnd = 0
    write = new Array[Any](capacity)
    writeEnd = 0
  }

  private def appendRead(elem: T): Unit = {
    read(readEnd) = elem
    readEnd += 1
  }

  private def appendWrite(elem: T): Unit = {
    write(writeEnd) = elem
    writeEnd += 1
  }

  private def moveWriteToRead(): Unit = {
    read = write
    write = new Array[Any](capacity)
    readOffset = 0
    readEnd = writeEnd
    writeEnd = 0
  }

  def push(elem: T): Queue[T] = {
    if (readEnd != capacity) {
      appendRead(elem)
    } else {
      appendWrite(elem)
      val newReadOffset = readOffset + 1
      if (newReadOffset == capacity)
        moveWriteToRead()
      else readOffset = newReadOffset
    }
    _size += 1
    if (size > capacity) _size = capacity
    this
  }

  override def pushValues(values: T*): Queue[T] = pushAll(values)


  override def pushAll(values: TraversableOnce[T]): Queue[T] = {
    values.foreach(push)
    this
  }

  private def nextInQueue(): T = {
    read(readOffset).asInstanceOf[T]
  }

  private def movePointerAfterRead(): Unit = {
    readOffset += 1
    if (readOffset == capacity) {
      moveWriteToRead()
    }
    _size -= 1
    if (_size < 0) _size = 0
  }

  override def pull(): T = {
    if (isEmpty) throw new UnsupportedOperationException("pull on empty queue")
    val res = nextInQueue()
    movePointerAfterRead()
    res
  }

  override def pullOption(): Option[T] = if (isEmpty) None else Some(pull())

  override def pullAll(count: Int): Seq[T] = {
    var counter = math.min(_size, count)

    pullWhile { _ =>
      counter -= 1
      counter != -1
    }
  }

  override def pullWhile(cond: (T) => Boolean): Seq[T] = {
    val res = new ListBuffer[T]()
    while (nonEmpty) {
      val next = nextInQueue()
      if (!cond(next)) return res.result()

      res += next
      movePointerAfterRead()
    }
    res.result()
  }

  override def +=(elem: T): Queue[T] = push(elem)

  override def ++=(elems: TraversableOnce[T]): Queue[T] = pushAll(elems)

  override def apply(idx: Int): T = {
    if (idx < 0 || idx >= _size)
      throw new IndexOutOfBoundsException(s"$idx is not between 0...$size")
    val indInRead = idx + readOffset
    val res =
      if (indInRead < readEnd) read(indInRead)
      else {
        val indInWrite = indInRead - readEnd
        write(indInWrite)
      }
    res.asInstanceOf[T]
  }

  override def length: Int = _size

  override def size: Int = length

  override def foreach[U](f: (T) => U): Unit = {
    var i = readOffset
    while (i < readEnd) {
      val el = read(i).asInstanceOf[T]
      f(el)
      i += 1
    }

    var k = 0
    while (k < writeEnd) {
      val el = write(k).asInstanceOf[T]
      f(el)
      k += 1
    }
  }

  // lazy interpretation of foreach =)
  override def iterator: Iterator[T] =
    new AbstractIterator[T] {
      private var i = readOffset
      private var k = 0

      private var firstNonStop = i < readEnd
      private var secondNonStop = k < writeEnd

      private def stepInRead(): T = {
        val ind = i
        i += 1
        firstNonStop = i < readEnd
        read(ind).asInstanceOf[T]
      }

      private def stepInWrite(): T = {
        val ind = k
        k += 1
        secondNonStop = k < writeEnd
        write(ind).asInstanceOf[T]
      }

      override def next(): T =
        if (firstNonStop) stepInRead()
        else if (secondNonStop) stepInWrite()
        else Iterator.empty.next()

      override def hasNext: Boolean = firstNonStop || secondNonStop
    }
}
