/*
* This source code is licensed under the MIT license found in the
* LICENSE.txt file in the root directory of this source tree
*/
package a14e.collz.immut

import scala.collection.mutable


object BoundedBucketQueue {
  def apply[A](maxSize: Int): BoundedBucketQueue[A] = {
    if (maxSize <= 0)
      throw new UnsupportedOperationException(s"max size should be > 0, but $maxSize reached")
    new BoundedBucketQueue(BoundedQueue[A](maxSize), Nil)
  }

  //  def empty


  //TODO протестировать
  def newBuilder[A](capacity: Int): mutable.Builder[A, BoundedBucketQueue[A]] =
    new mutable.Builder[A, BoundedBucketQueue[A]] {

      private var queue: BoundedBucketQueue[A] = BoundedBucketQueue[A](capacity)

      override def +=(elem: A): this.type = {
        queue = queue :+ elem
        this
      }

      override def clear(): Unit = queue = BoundedBucketQueue(capacity)

      override def result(): BoundedBucketQueue[A] = queue
    }
}

/**
  * TODO протестировать
  *
  * контейнер для очереди фиксированного размера и корзины в вие списка
  *
  * в отличае от BoundedQueue старые элементы не удаляются навсегда, а добавляются
  * в корзину
  *
  * Created by Borisenko Andrew on 28.01.2017.
  */
class BoundedBucketQueue[T](val queue: BoundedQueue[T], val bucket: List[T]) extends Queue[T] {

  //TODO протестировать
  override def push(value: T): this.type = {
    val newQueue = queue.push(value)
    val newBucket =
      if (queue.size == queue.capacity)
        queue.head :: bucket
      else
        bucket

    new BoundedBucketQueue[T](newQueue, newBucket).asInstanceOf[this.type]
  }

  //TODO протестировать
  override def pushValues(values: T*): this.type = pushAll(values)

  //TODO протестировать
  override def pushAll(values: TraversableOnce[T]): this.type = {
    var newBucket = bucket
    var newQueue = queue
    for (x <- values) {
      if (newQueue.size == queue.capacity)
        newBucket = newQueue.head :: newBucket
      newQueue = newQueue.push(x)
    }
    new BoundedBucketQueue[T](newQueue, newBucket).asInstanceOf[this.type]
  }

  //TODO протестировать
  override def pull(): (this.type, T) = {
    if (isEmpty)
      throw new UnsupportedOperationException("pull on empty queue")
    val (newQueue, newValue) = queue.pull()
    val newBucketQueue = new BoundedBucketQueue[T](newQueue, bucket).asInstanceOf[this.type]
    (newBucketQueue, newValue)
  }

  //TODO протестировать
  override def pullOption(): (this.type, Option[T]) =
    if (isEmpty) (this, None: Option[T])
    else {
      val (newQueue, newValue) = this.pull()

      (newQueue.asInstanceOf[this.type], Some(newValue))
    }

  //TODO протестировать
  override def pullToBuff(buffer: mutable.Buffer[T]): this.type = {
    val newQueue = queue.pullToBuff(buffer)
    new BoundedBucketQueue[T](newQueue, bucket).asInstanceOf[this.type]
  }

  //TODO протестировать
  override def pullAll(count: Int): (this.type, Seq[T]) = {
    val (newQueue, seq) = queue.pullAll(count)
    val resQueue = new BoundedBucketQueue[T](newQueue, bucket).asInstanceOf[this.type]
    (resQueue, seq)
  }

  //TODO протестировать
  def clearBucket: (this.type, List[T]) = {
    val res = new BoundedBucketQueue[T](queue, Nil).asInstanceOf[this.type]
    (res, bucket)
  }

  //TODO протестировать
  def withEmptyBucket: this.type = {
    new BoundedBucketQueue[T](queue, Nil).asInstanceOf[this.type]
  }

  //TODO протестировать
  override def pullAllToBuff(count: Int, buffer: mutable.Buffer[T]): this.type = {
    val newQueue = queue.pullAllToBuff(count, buffer)
    new BoundedBucketQueue[T](newQueue, bucket).asInstanceOf[this.type]
  }

  //TODO протестировать
  override def pullWhile(cond: (T) => Boolean): (this.type, Seq[T]) = {
    val (newQueue, seq) = queue.pullWhile(cond)
    val resQueue = new BoundedBucketQueue[T](newQueue, bucket).asInstanceOf[this.type]
    (resQueue, seq)
  }

  //TODO протестировать
  override def pullWhileToBuff(cond: (T) => Boolean, buffer: mutable.Buffer[T]): this.type = {
    val newQueue = queue.pullWhileToBuff(cond, buffer)
    new BoundedBucketQueue[T](newQueue, bucket).asInstanceOf[this.type]
  }

  //TODO протестировать
  override def :+(elem: T): this.type = push(elem)

  //TODO протестировать
  override def :++(elems: TraversableOnce[T]): this.type = pushAll(elems)

  //TODO протестировать
  override def apply(idx: Int): T = queue(idx)

  //TODO протестироватьы
  override def isEmpty: Boolean = queue.isEmpty

  override def toString: String = "BoundedBucketQueue(" + queue + ", " + bucket + ")"
}
