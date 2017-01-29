/*
* This source code is licensed under the MIT license found in the
* LICENSE.txt file in the root directory of this source tree
*/
package a14e.collz.mut

import scala.collection.generic.{CanBuildFrom, GenericCompanion, GenericTraversableTemplate, SeqFactory}
import scala.collection.{AbstractIterator, mutable}
import scala.reflect.ClassTag


object VList extends SeqFactory[VList] {
  override def apply[T](xs: T*): VList[T] = new VList[T]() ++= xs


  def range(start: Int, end: Int) = new VList(start to end)

  override def newBuilder[A]: mutable.Builder[A, VList[A]] = new VList[A]()

  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, VList[A]] = ReusableCBF.asInstanceOf[GenericCanBuildFrom[A]]

}

/* TODO сделать время доступа const вместо log(n) */
class VList[T] protected(protected var underlying: Array[Array[Any]],
                         protected var currentIndex: Int,
                         protected var bigArraySize: Int,
                         protected var len: Int) extends
  mutable.AbstractBuffer[T]
  with GenericTraversableTemplate[T, VList]
  with mutable.BufferLike[T, VList[T]]
  with mutable.Builder[T, VList[T]]
  with mutable.Iterable[T]
  with Serializable {
  self =>
  //

  override def companion: GenericCompanion[VList] = VList

  def this() = {
    this(null, 0, 0, 0)
    clear() // так как в clear() сделано будет все, что надо
  }

  def this(xs: TraversableOnce[T]) = {
    this()
    this.appendAll(xs)
  }


  override def iterator: Iterator[T] = new AbstractIterator[T] {
    private var currentArray = if (len != 0) underlying(0) else null
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
    private var currentArray = if (len != 0) underlying(bigArraySize - 1) else null
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
    underlying(0) = new Array[Any](1)
    currentIndex = -1
    bigArraySize = 1
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
    val iter = iterator.toList
    clear()
    appendAll(elems)
    appendAll(iter)
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


  private def addElementToArray(array: Array[Any], x: T): Array[Any] = {
    var toAddArray = array
    currentIndex += 1
    len += 1
    if (currentIndex >= array.length) {
      incrementSize()
      currentIndex = 0
      toAddArray = underlying(bigArraySize - 1)
    }
    toAddArray(currentIndex) = x
    toAddArray
  }

  // TODO обновить и убрать первый if =)
  def appendOne(x: T): Unit = {
    val array = underlying(bigArraySize - 1)
    addElementToArray(array, x)
  }


  override def appendAll(xs: TraversableOnce[T]): Unit = {
    var array = underlying(bigArraySize - 1)
    for (x <- xs) {
      array = addElementToArray(array, x)
    }
  }


  override def foldRight[B](z: B)(op: (T, B) => B): B = {
    var res = z
    reverseForeach(x => res = op(x, res))
    res
  }


  override def filter(cond: T => Boolean): VList[T] = {
    val list = new VList[T]()
    foreach(x => if (cond(x)) list += x)
    list
  }

  //  def reverseFilter(f: T => Boolean): VList[T] = {
  //    val list = new VList[T]()
  //    reverseForeach(x => if (f(x)) list += x)
  //    list
  //  }

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

  override def head: T = {
    if (isEmpty)
      throw new UnsupportedOperationException("head on empty VList")
    underlying.head.head.asInstanceOf[T]
  }

  override def last: T = {
    if (isEmpty)
      throw new UnsupportedOperationException("last on empty VList")
    underlying(bigArraySize - 1).apply(currentIndex).asInstanceOf[T]
  }

  override def newBuilder: mutable.Builder[T, VList[T]] = companion.newBuilder[T]

  /**
    * поиск элемента по индексу и выполнение функции f после нахождения элемента
    * в случае, когда индекс выходит за пределы размера коллекции вылетает
    * исключение
    *
    * поиск осуществляется за время O(log2(n)) в худшем случае (когда ведется поиск первого элемента)
    * и за O(1) в большинстве случаев, так как 50...75% элементов находятся в последних 2-х массивах
    *
    * исключение в конце массива находится за бесконнечным циклом и в принципе недостижимо
    * кроме ситуации гонки, возможной при работе при многопоточности,
    * не рекомендуется вызывать этот метод и вообще использовать этот класс при многопоточной работе,
    * так как это может потенциально приводить к дедлокам, исключениям и прочим гадостям.
    *
    * @param idx индекс элемента
    * @param f   функция, которая будет вызвана после нахождения элемента
    *            первый аргумент -- массив, где находится элемент, второй --
    *            индекс внутри массива и трейтий -- номер массива
    * @tparam B тип возвращаемого значения функцией f(...)
    * @return возврващает результат выполнения функции f, либо падает
    *         с исключением о выходе за пределы массива.
    */
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

    throw new IllegalStateException("unreachable state")
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
            bigArraySize = 1
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
    idx match {
      case 0 => elems ++=: this
      case _ if idx == length => this ++= elems
      case _ =>
        val listOfPrevious = iterator.drop(idx).toList

        withFound(idx) {
          (_, i, bigIndex) =>
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


  override def result(): VList[T] = this

  override def stringPrefix: String = "VList"

}
