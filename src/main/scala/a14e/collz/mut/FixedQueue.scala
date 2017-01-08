package a14e.collz.mut

import scala.collection.AbstractIterator
import scala.collection.mutable.ListBuffer

/**
  * Created by Andrew on 31.12.2016.
  */
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

}

object FixedQueue {
  def apply[T](maxSize: Int): FixedQueue[T] = new FixedQueue[T](maxSize)

  def fill[T](maxSize: Int)(el: => T): FixedQueue[T] = {
    val res = FixedQueue[T](maxSize)
    var counter = maxSize
    while (counter != 0) {
      res += el
      counter -= 1
    }
    res
  }

  def of[T](xs: T*): Queue[T] = FixedQueue[T](xs.size) ++= xs
}


class FixedQueue[T](maxSize: Int) extends Queue[T] {
  self =>
  private var _size: Int = 0

  private var read: Array[Any] = new Array[Any](maxSize)
  private var readOffset: Int = 0
  private var readEnd: Int = 0

  private var write: Array[Any] = new Array[Any](maxSize)
  private var writeEnd: Int = 0


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
    write = new Array[Any](maxSize)
    readOffset = 0
    readEnd = writeEnd
    writeEnd = 0
  }

  def push(elem: T): Queue[T] = {
    if (readEnd != maxSize) {
      appendRead(elem)
    } else {
      appendWrite(elem)
      val newReadOffset = readOffset + 1
      if (newReadOffset == maxSize)
        moveWriteToRead()
      else readOffset = newReadOffset
    }
    _size += 1
    if (size > maxSize) _size = maxSize
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
    if (readOffset == maxSize) {
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
    val res = new ListBuffer[T]()
    while (counter != 0) {
      res += pull()
      counter -= 1
    }
    res.result()
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
