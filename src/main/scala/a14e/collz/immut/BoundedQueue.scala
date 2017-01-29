/*
* This source code is licensed under the MIT license found in the
* LICENSE.txt file in the root directory of this source tree
*/
package a14e.collz.immut

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class BoundedQueue[T](readOffset: Int,
                      read: Vector[T],
                      write: Vector[T],
                      val capacity: Int)
  extends Queue[T] with Seq[T] with Serializable {

  override def isEmpty: Boolean = (read.size - readOffset == 0) && write.isEmpty

  override def iterator: Iterator[T] = read.iterator.drop(readOffset) ++ write.iterator

  override def push(value: T): this.type = this :+ value

  override def pushValues(values: T*): this.type = pushAll(values)

  override def pushAll(values: TraversableOnce[T]): this.type = this :++ values

  //TODO протестировать
  override def filter(p: (T) => Boolean): this.type = {
    var res = BoundedQueue[T](capacity)
    for (x <- this)
      if (p(x))
        res = res :+ x
    res.asInstanceOf[this.type]
  }

  //  override def newBuilder: mutable.Builder[T, Queue[T]] = BoundedQueue.newBuilder[T](capacity)

  override def pull(): (this.type, T) = {
    val newOffset = readOffset + 1
    if (newOffset == capacity) {
      val value = read.last
      val queue = new BoundedQueue[T](0, write, Vector.empty[T], capacity)
      (queue.asInstanceOf[this.type], value)
    } else {
      if (read.isEmpty)
        throw new IndexOutOfBoundsException("pull on empty queue")
      val value = read(readOffset)
      val queue = new BoundedQueue[T](newOffset, read, write, capacity)
      (queue.asInstanceOf[this.type], value)
    }
  }

  //TODO протестировать
  override def pullOption(): (this.type, Option[T]) = {
    if (isEmpty) (this, None)
    else {
      val (q, v) = pull()
      (q.asInstanceOf[this.type], Some(v))
    }
  }

  override def pullToBuff(buffer: mutable.Buffer[T]): this.type = {
    val newOffset = readOffset + 1
    if (newOffset == read.length) {
      buffer += read.last
      new BoundedQueue[T](0, write, Vector.empty[T], capacity).asInstanceOf[this.type]
    } else {
      if (read.isEmpty) this
      else {
        buffer += read(readOffset)
        new BoundedQueue[T](newOffset, read, write, capacity).asInstanceOf[this.type]
      }
    }
  }

  override def pullAll(count: Int): (this.type, Seq[T]) = {
    val buff = new ListBuffer[T]()

    val newQueue = pullAllToBuff(count, buff)
    (newQueue, buff.result())
  }

  override def pullAllToBuff(count: Int, buffer: mutable.Buffer[T]): this.type = {
    var resQueue: this.type = this
    var left = math.min(count, length)
    while (left > 0) {
      resQueue = resQueue.pullToBuff(buffer)
      left -= 1
    }

    resQueue
  }

  override def pullWhile(cond: (T) => Boolean): (this.type, Seq[T]) = {
    val buff = new ListBuffer[T]()
    val newQueue = pullWhileToBuff(cond, buff)
    (newQueue, buff.result())
  }

  override def pullWhileToBuff(cond: (T) => Boolean,
                               buffer: mutable.Buffer[T]): this.type = {
    var current: this.type = this
    while (current.nonEmpty) {
      val (newQueue, value) = current.pull()
      if (!cond(value))
        return current
      current = newQueue.asInstanceOf[this.type]
      buffer += value
    }
    current
  }

  override def length: Int = read.size + write.size - readOffset

  override def size: Int = length

  override def head: T = {
    if (read.isEmpty)
      throw new IndexOutOfBoundsException("head on empty queue")
    read(readOffset)
  }

  override def last: T = {
    if (write.nonEmpty)
      write.last
    else if (read.size - readOffset != 0)
      read.last
    else
      throw new IndexOutOfBoundsException("last on empty queue")
  }

  override def foreach[U](f: (T) => U): Unit = {
    if (readOffset == 0) {
      read.foreach(f)
      write.foreach(f)
    } else {
      read.iterator.drop(readOffset).foreach(f)
      write.foreach(f)
    }
  }

  override def :+(elem: T): this.type = {
    val readSize = read.size
    if (readSize != capacity) {
      val newRead = read :+ elem
      new BoundedQueue[T](readOffset, newRead, write, capacity)
    } else {
      val newWrite = write :+ elem
      val newReadOffset = readOffset + 1
      if (newReadOffset == capacity)
        new BoundedQueue[T](0, newWrite, Vector.empty[T], capacity)
      else
        new BoundedQueue[T](newReadOffset, read, newWrite, capacity)
    }
  }.asInstanceOf[this.type]

  override def :++(elems: TraversableOnce[T]): this.type = {
    var res: this.type = this
    elems.foreach(x => res = res :+ x)
    res
  }

  override def apply(idx: Int): T = {
    if (idx < 0 || idx >= length)
      throw new IndexOutOfBoundsException(idx.toString)

    val readIndex = readOffset + idx
    if (read.size > readIndex)
      read(readIndex)
    else {
      val writeIndex = readIndex - read.size
      write(writeIndex)
    }
  }

  override def stringPrefix: String = "BoundedQueue"
}

object BoundedQueue {

  def fill[T](maxSize: Int)(el: => T): BoundedQueue[T] = {
    BoundedQueue[T](maxSize) :++ List.fill(maxSize)(el)
  }

  //TODO протестировать
  def of[T](xs: T*): BoundedQueue[T] = (newBuilder[T](xs.size) ++= xs).result()


  def apply[T](maxSize: Int): BoundedQueue[T] = {
    if (maxSize <= 0)
      throw new IllegalArgumentException(s"$maxSize: BoundedQueue should have maxSize > 0")
    new BoundedQueue[T](0, Vector.empty[T], Vector.empty[T], maxSize)
  }

  //TODO протестировать
  def newBuilder[A](capacity: Int): mutable.Builder[A, BoundedQueue[A]] = new mutable.Builder[A, BoundedQueue[A]] {

    private var queue: BoundedQueue[A] = BoundedQueue[A](capacity)

    override def +=(elem: A): this.type = {
      queue = queue :+ elem
      this
    }

    override def clear(): Unit = queue = BoundedQueue(capacity)

    override def result(): BoundedQueue[A] = queue
  }
}