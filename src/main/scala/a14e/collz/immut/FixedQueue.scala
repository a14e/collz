package a14e.collz.immut
//import scala.collection.immutabl
import scala.collection.mutable
import scala.collection.mutable.ListBuffer


trait Queue[T] extends Traversable[T] with Iterable[T] {
  scala.collection.mutable.Queue

  def push(value: T): Queue[T]

  def pushValues(values: T*): Queue[T]

  def pushAll(values: TraversableOnce[T]): Queue[T]

  def pull(): (Queue[T], T)

  def pullOption(): (Queue[T], Option[T])

  def pullToBuff(buffer: collection.mutable.Buffer[T]): Queue[T]

  def pullAll(count: Int): (Queue[T], Seq[T])

  def pullAllToBuff(count: Int, buffer: collection.mutable.Buffer[T]): Queue[T]

  def pullWhile(cond: T => Boolean): (Queue[T], Seq[T])

  def pullWhileToBuff(cond: T => Boolean, buffer: collection.mutable.Buffer[T]): Queue[T]


  def :+(elem: T): Queue[T]

  def :++(elems: TraversableOnce[T]): Queue[T]

  def apply(idx: Int): T

  def isEmpty: Boolean

  def length: Int


}
//TODO сделать изменяемую очередь
/**
  * Created by Andrew on 18.12.2016.
  */
class FixedQueue[T](readOffset: Int, read: Vector[T], write: Vector[T], maxSize: Int)
  extends Queue[T] with Serializable {

  override def isEmpty: Boolean = (read.size - readOffset == 0) && write.isEmpty

  override def iterator: Iterator[T] = read.iterator.drop(readOffset) ++ write.iterator

  override def push(value: T): Queue[T] = this :+ value

  override def pushValues(values: T*): Queue[T] = pushAll(values)

  override def pushAll(values: TraversableOnce[T]): Queue[T] = this :++ values

  override def pull(): (Queue[T], T) = {
    val newOffset = readOffset + 1
    if (newOffset == maxSize) {
      val value = read.last
      val queue = new FixedQueue[T](0, write, Vector.empty[T], maxSize)
      (queue, value)
    } else {
      if (read.isEmpty)
        throw new IndexOutOfBoundsException("pull on empty queue")
      val value = read(readOffset)
      val queue = new FixedQueue[T](newOffset, read, write, maxSize)
      (queue, value)
    }
  }

  override def pullOption(): (Queue[T], Option[T]) = {
    val newOffset = readOffset + 1
    if (newOffset == maxSize) {
      val value = read.headOption
      val queue = new FixedQueue[T](0, write, Vector.empty[T], maxSize)
      (queue, value)
    } else {
      val value = if (read.isEmpty) None else Some(read(readOffset))
      val queue = new FixedQueue[T](newOffset, read, write, maxSize)
      (queue, value)
    }
  }

  override def pullToBuff(buffer: mutable.Buffer[T]): Queue[T] = {
    val newOffset = readOffset + 1
    if (newOffset == read.length) {
      buffer += read.last
      new FixedQueue[T](0, write, Vector.empty[T], maxSize)
    } else {
      if (read.isEmpty) this
      else {
        buffer += read(readOffset)
        new FixedQueue[T](newOffset, read, write, maxSize)
      }
    }
  }

  override def pullAll(count: Int): (Queue[T], Seq[T]) = {
    val buff = new ListBuffer[T]()

    val newQueue = pullAllToBuff(count, buff)
    (newQueue, buff.result())
  }

  override def pullAllToBuff(count: Int, buffer: mutable.Buffer[T]): Queue[T] = {
    var resQueue: Queue[T] = this
    var left = math.min(count, length)
    while (left > 0) {
      resQueue = resQueue.pullToBuff(buffer)
      left -= 1
    }

    resQueue
  }

  override def pullWhile(cond: (T) => Boolean): (Queue[T], Seq[T]) = {
    val buff = new ListBuffer[T]()
    val newQueue = pullWhileToBuff(cond, buff)
    (newQueue, buff.result())
  }

  override def pullWhileToBuff(cond: (T) => Boolean,
                               buffer: mutable.Buffer[T]): Queue[T] = {
    var current: Queue[T] = this
    while (current.nonEmpty) {
      val (newQueue, value) = current.pull()
      if (!cond(value))
        return current
      current = newQueue
      buffer += value
    }
    current
  }

  def length: Int = read.size + write.size - readOffset

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

  override def :+(elem: T): Queue[T] = {
    val readSize = read.size
    if (readSize != maxSize) {
      val newRead = read :+ elem
      new FixedQueue[T](readOffset, newRead, write, maxSize)
    } else {
      val newWrite = write :+ elem
      val newReadOffset = readOffset + 1
      if (newReadOffset == maxSize) {
        new FixedQueue[T](0, newWrite, Vector.empty[T], maxSize)
      } else {
        new FixedQueue[T](newReadOffset, read, newWrite, maxSize)
      }
    }
  }

  override def :++(elems: TraversableOnce[T]): Queue[T] = {
    var res: Queue[T] = this
    elems.foreach { x => res = res.push(x) }
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

  override def toString(): String = mkString("FixedQueue(", ", ", " )")
}

object FixedQueue {

  def apply[T](maxSize: Int): FixedQueue[T] = {
    if (maxSize <= 0)
      throw new IllegalArgumentException("FixedQueue should have maxSize > 0")
    new FixedQueue[T](0, Vector.empty[T], Vector.empty[T], maxSize)
  }

  def newBuilder[A](maxSize: Int): mutable.Builder[A, FixedQueue[A]] = new mutable.Builder[A, FixedQueue[A]] {
    private var queue: FixedQueue[A] = FixedQueue[A](maxSize)

    override def +=(elem: A): this.type = {
      queue = (queue :+ elem).asInstanceOf[FixedQueue[A]]
      this
    }

    override def clear(): Unit = queue = FixedQueue(maxSize)

    override def result(): FixedQueue[A] = queue
  }

}