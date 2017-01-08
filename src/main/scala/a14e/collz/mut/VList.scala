package a14e.collz.mut

import scala.collection.generic.{CanBuildFrom, GenericCompanion, GenericTraversableTemplate, SeqFactory}
import scala.collection.{AbstractIterator, mutable}
import scala.reflect.ClassTag


object VList extends SeqFactory[VList] {
  override def apply[T](xs: T*): VList[T] = new VList[T]() ++= xs


  def range(start: Int, end: Int) = new VList(start to end)

  override def newBuilder[A]: mutable.Builder[A, VList[A]] = new mutable.Builder[A, VList[A]] {
    var list = new VList[A]()

    override def +=(elem: A): this.type = {
      list.appendOne(elem)
      this
    }

    override def ++=(elems: TraversableOnce[A]): this.type = {
      list.appendAll(elems)
      this
    }

    override def result(): VList[A] = list

    override def clear(): Unit = list.clear()
  }

  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, VList[A]] = ReusableCBF.asInstanceOf[GenericCanBuildFrom[A]]

}

/* TODO сделать тесты лучше */
/* TODO сделать время доступа const вместо log(n) */
/**
  * Created by User on 11.04.2016.
  */
class VList[T] protected(protected var underlying: Array[Array[Any]],
                         protected var currentIndex: Int,
                         protected var bigArraySize: Int,
                         protected var len: Int) extends
  mutable.AbstractBuffer[T]
  with GenericTraversableTemplate[T, VList]
  with mutable.BufferLike[T, VList[T]]
  with mutable.Iterable[T]
  with Seq[T]
  with Serializable {
  self =>
  //

  override def companion: GenericCompanion[VList] = VList

  def this() = this(new Array(3), -1, 0, 0)

  def this(xs: TraversableOnce[T]) = {
    this()
    this.appendAll(xs)
  }


  override def iterator: Iterator[T] = new AbstractIterator[T] {
    private var currentArray = if (bigArraySize > 0) underlying(0) else null
    private var index = 0
    private var endIndex: Int = if (currentArray != null) currentArray.length else -1
    private var bigIndex = 0

    override def hasNext: Boolean = currentArray != null


    override def next(): T =
      if (hasNext) {
        val res = currentArray(index).asInstanceOf[T]
        index += 1
        if (index == endIndex) {
          bigIndex += 1
          if (bigIndex >= bigArraySize)
            currentArray = null
          else {
            index = 0
            currentArray = underlying(bigIndex)
            endIndex = if (bigArraySize - 1 == bigIndex) currentIndex + 1 else currentArray.length
          }
        }
        res
      } else Iterator.empty.next()


  }


  override def reverseIterator: Iterator[T] = new AbstractIterator[T] {
    private var currentArray = if (bigArraySize > 0) underlying(bigArraySize - 1) else null
    private var index = currentIndex
    private var bigIndex = bigArraySize - 1

    override def hasNext: Boolean = currentArray != null


    override def next(): T =
      if (hasNext) {
        val res = currentArray(index).asInstanceOf[T]
        index -= 1
        if (index < 0) {
          bigIndex -= 1
          if (bigIndex < 0)
            currentArray = null
          else {
            currentArray = underlying(bigIndex)
            index = currentArray.length - 1
          }
        }
        res
      } else Iterator.empty.next()
  }

  override def length: Int = len

  override def apply(idx: Int): T = withFound(idx)((a, i, _) => a(i).asInstanceOf[T])

  override def update(idx: Int, x: T): Unit = withFound(idx)((a, i, _) => a(i) = x)

  override def clear(): Unit = {
    underlying = new Array[Array[Any]](3)
    currentIndex = -1
    bigArraySize = 0
    len = 0
  }

  override def +=:(elem: T): this.type = {
    val listOfElems = iterator.toList
    clear()
    appendOne(elem)
    appendAll(listOfElems)
    this
  }

  override def ++=:(elems: TraversableOnce[T]): this.type = {
    val listOfElems = iterator.toList
    clear()
    appendAll(elems)
    appendAll(listOfElems)
    this
  }

  override def +=(x: T): this.type = {
    appendOne(x)
    this
  }

  override def ++=(xs: TraversableOnce[T]): this.type = {
    appendAll(xs)
    this
  }


  override def map[B, That](f: T => B)(implicit bf: CanBuildFrom[VList[T], B, That]): That = {
    def builder = {
      // extracted to keep method size under 35 bytes, so that it can be JIT-inlined
      val b = bf(repr)
      b.sizeHint(this)
      b
    }

    val b = builder
    b ++= this.iterator.map(f)
    b.result
  }

  protected def incrementSize(): Unit = {
    val oldArraySize = this.bigArraySize
    val newSize = oldArraySize + 1
    if (newSize - underlying.length < 0) {
      val newArraySize = underlying.length + 3
      val newArray = new Array[Array[Any]](newArraySize)
      scala.compat.Platform.arraycopy(underlying, 0, newArray, 0, oldArraySize)
      underlying = newArray
    }
    this.bigArraySize = newSize
    val nexLen = if (newSize == 1) 1 else 2 * underlying(newSize - 2).length
    underlying(newSize - 1) = new Array[Any](nexLen)
  }

  // TODO обновить и убрать первый if =)
  def appendOne(x: T): Unit =
    if (bigArraySize == 0) {
      incrementSize()
      len += 1
      currentIndex = 0
      underlying(0)(currentIndex) = x
    } else {
      currentIndex += 1
      len += 1
      val array = underlying(bigArraySize - 1)
      if (currentIndex >= array.length) {
        incrementSize()
        currentIndex = 0
        underlying(bigArraySize - 1)(currentIndex) = x
      } else {
        array(currentIndex) = x
      }
    }

  override def append(xs: T*): Unit = appendAll(xs)

  override def appendAll(xs: TraversableOnce[T]): Unit =
    if (xs.nonEmpty) {
      val it = xs.toIterator
      appendOne(it.next)
      if (it.hasNext) {
        var array = underlying(bigArraySize - 1)
        while (it.hasNext) {
          currentIndex += 1
          len += 1
          if (currentIndex >= array.length) {
            incrementSize()
            currentIndex = 0
            array = underlying(bigArraySize - 1)
          }
          array(currentIndex) = it.next
        }
      }
    }

  override def foldLeft[B](z: B)(op: (B, T) => B): B = {
    var res = z
    foreach(x => res = op(res, x))
    res
  }

  override def foldRight[B](z: B)(op: (T, B) => B): B = {
    var res = z
    reverseForeach(x => res = op(x, res))
    res
  }


  override def filter(f: T => Boolean): VList[T] = {
    val list = new VList[T]()
    foreach(x => if(f(x)) list += x)
    list
  }

  def reverseFilter(f: T => Boolean): VList[T] = {
    val list = new VList[T]()
    reverseForeach(x => if(f(x)) list += x)
    list
  }

  override def reverse: VList[T] = {
    val list = new VList[T]()
    reverseForeach(list += _)
    list
  }

  override def foreach[U](f: T => U): Unit = if (nonEmpty) {
    var i = 0
    val end = bigArraySize - 1
    while (i < end) {
      var k = 0
      val array = underlying(i)
      val len = array.length
      while (k < len) {
        f(array(k).asInstanceOf[T])
        k += 1
      }
      i += 1
    }

    var k = 0
    val array = underlying(i)
    while (k <= currentIndex) {
      f(array(k).asInstanceOf[T])
      k += 1
    }
  }

  def reverseForeach[U](f: T => U): Unit = if (nonEmpty) {
    var i = bigArraySize - 1
    var array = underlying(i)
    var k = currentIndex
    do {
      while (k >= 0) {
        f(array(k).asInstanceOf[T])
        k -= 1
      }
      i -= 1
    } while ((i >= 0) && {
      array = underlying(i)
      k = array.length - 1
      true
    })
  }

  override def isEmpty: Boolean = len == 0

  override def head: T = underlying.head.apply(0).asInstanceOf[T]

  override def last: T = underlying(bigArraySize - 1).apply(currentIndex).asInstanceOf[T]

  override def tail: VList[T] = {
    val res = new VList[T]()
    res.appendAll(iterator.drop(1))
    res
  }

  protected def withFound[B](idx: Int)(f: (Array[Any], Int, Int) => B): B = {
    if (idx < 0 || idx >= len)
      throw new IndexOutOfBoundsException(s"index '$idx' is out of bounds: [0...$len]")

    var bigIndex = this.bigArraySize - 1
    var currentArray = underlying(bigIndex)
    var index = len - idx - 1
    var maxIndex = currentIndex

    do {
      if (index <= maxIndex) {
        return f(currentArray, maxIndex - index, bigIndex)
      } else {
        index -= maxIndex + 1
        if (bigIndex > 0) {
          bigIndex -= 1
          currentArray = underlying(bigIndex)
          maxIndex = currentArray.length - 1
        }
      }
    } while (true)

    /** only for compiling */
    null.asInstanceOf[B]
  }


  override def remove(idx: Int): T = {
    //    val nextLen = len - idx - 1
    val iter = iterator.drop(idx + 1).toList

    withFound(idx) {
      (a, i, bigIndex) =>
        val res = a(i).asInstanceOf[T]

        /** не чистим память, поэтому итератор должен работать */
        if (i != 0) {
          currentIndex = i - 1
          bigArraySize = bigIndex + 1
        } else {
          if (bigIndex != 0) {
            bigArraySize = bigIndex
            val array = underlying(bigIndex - 1)
            currentIndex = array.length - 1
          } else {
            bigArraySize = 0
            currentIndex = -1
          }
        }
        len = idx
        appendAll(iter)
        res
    }
  }

  // TODO заменить на более эфективное копирование массивов
  override def toArray[B >: T : ClassTag]: Array[B] = {
    val result = new Array[B](len)
    var i = len - 1

    /** так как обратный чуточку быстрее */
    reverseForeach { x =>
      result(i) = x
      i -= 1
    }
    result
  }

  override def toList: List[T] = {
    var res: List[T] = Nil
    reverseForeach(x => res = x :: res)
    res
  }


  /** TODO улучшить */
  override def insertAll(idx: Int, elems: Traversable[T]): Unit =
    if (idx == 0)
      elems ++=: this
    else if (idx == length) {
      this ++= elems
    } else {
      val listOfPrevious = iterator.drop(idx).toList

      withFound(idx) {
        (a, i, bigIndex) =>
          if (i != 0) {
            currentIndex = i - 1
            bigArraySize = bigIndex + 1
          } else {
            bigArraySize = bigIndex
            val array = underlying(bigIndex - 1)
            currentIndex = array.length - 1
          }
          len = idx

          appendAll(elems)
          appendAll(listOfPrevious)
      }
    }

  override def equals(x: Any): Boolean = x match {
    case arr: Array[T] =>
      //      scala.compat.Platform.
      arr.length == length && {
        var i = 0
        val iter = iterator
        while (iter.hasNext) {
          if (iter.next() != arr(i))
            return false
          i += 1
        }
        true
      }
    case _ => super.equals(x)
  }


  override def stringPrefix: String = "VList"


}
