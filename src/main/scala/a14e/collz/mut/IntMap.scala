package a14e.collz.mut


import scala.collection.generic.{CanBuildFrom, GenericCompanion}
import scala.collection.{AbstractIterator, GenMap, GenMapLike, mutable}
import scala.language.higherKinds
import scala.util.Random


object IntMap {

  // type CC[B] <: GenMap[Int, B] with GenMapLike[Int, B, CC[ B]]

  //  def newBuilder[B]: collection.mutable.Builder[(Int, B), IntMap[B]] = new IntMap[B]()

  implicit def canBuildFrom[B] = new CanBuildFrom[mutable.Map[_, _], (Int, B), IntMap[B]] {
    def apply(from: mutable.Map[_, _]): mutable.Builder[(Int, B), IntMap[B]] = apply()

    def apply(): mutable.Builder[(Int, B), IntMap[B]] = new IntMap[B]()
  }

  def apply[T](xs: (Int, T)*): IntMap[T] = new IntMap[T]() ++= xs

  private[collz] class EntryValue(val key: Int, var value: Any)

  private[collz] class EntryArray(var underlying: Array[AnyRef], var size: Int)

  // маска для того, чтобы достать из числа индекс
  final val mask = 0xF
  // количество бит, в которых хранится индекс для каждого уровня
  final val bitLevel = 4
  // количество элементов в одном узле(на одном уровне)
  final val levelSize = 16


}

import IntMap._

/**
  * Created by Andrew on 30.12.2016.
  *
  * Реализация изменяемого IntMap основанная на 16 битном префиксном дереве (16 bit trie)
  * за каждый символ принимаются куски ключа по 4 бит, таким образом максимальная глубина дерева
  * для 32 битновго числа составляет 8 слоев.
  * асимптотическая сложность поиска, добавления, удаления элементов log 16(n)
  *
  * структура данных изменяемая и не потокобезопасная.
  *
  * многие функцие рекурентные, часть из них не на основе хвостой рекрусии. Это Ok, так
  * как мы заранее знаем, что глубина стека не превысит 8 =)
  *
  * структура очень похожа на стандартный неизменяемый HashMap,
  * но обновление и поиск в ней значительно быстрее.
  *
  * простые бенчмарки показывают большое преимущество в скорости в сравнении
  * со стандартным изменяемым mutable.HashMap, immutable.HashMap и AnyRefMap,
  * IntMap, TreeMap при добавлении (в 2 ... 8 раз)
  * при поиске значительной разницы в скорости нет =)
  *
  *
  */
class IntMap[T](private[collz] var underlying: Array[AnyRef] = new Array[AnyRef](IntMap.levelSize),
                private var _size: Int = 0)
  extends scala.collection.mutable.Map[Int, T] with collection.mutable.Builder[(Int, T), IntMap[T]] {

  override def newBuilder: mutable.Builder[(Int, T), mutable.Map[Int, T]] = new IntMap[T]()

  override def clear(): Unit = {
    underlying = new Array[AnyRef](levelSize)
    _size = 0
  }

  override def isEmpty: Boolean = _size == 0

  override def result(): this.type = this

  override def size: Int = _size


  private def recurContains(array: Array[AnyRef],
                            currentKey: Int, initKey: Int): Boolean = {
    val index = currentKey & mask
    array(index) match {
      case null => false
      case x: EntryValue => x.key == initKey
      case arrayValue: EntryArray =>
        recurContains(arrayValue.underlying, currentKey >>> bitLevel, initKey)
    }
  }

  override def contains(key: Int): Boolean = recurContains(underlying, key, key)

  //  override def ma

  private def mergeNodes(currentKey1: Int, node1: EntryValue,
                         currentKey2: Int, node2: EntryValue): EntryArray = {

    val index1 = currentKey1 & mask
    val index2 = currentKey2 & mask
    val res = new EntryArray(new Array[AnyRef](levelSize), 1)
    if (index1 == index2) {
      res.underlying(index1) =
        mergeNodes(currentKey1 >>> bitLevel, node1, currentKey2 >>> bitLevel, node2)
    } else {
      res.underlying(index1) = node1
      res.underlying(index2) = node2
      res.size += 1
    }
    res
  }

  private def recurUpdate(array: Array[AnyRef],
                          currentKey: Int, initKey: Int,
                          value: T,
                          level: Int,
                          lastArray: EntryArray): Unit = {
    val index = currentKey & mask
    array(index) match {
      case null =>
        array(index) = new EntryValue(initKey, value)
        _size += 1
        if (lastArray != null)
          lastArray.size += 1

      case oldNode: EntryValue =>
        if (oldNode.key == initKey) {
          array(index) = new EntryValue(initKey, value)
        } else {
          val currentKey1 = currentKey >>> bitLevel
          val currentKey2 = oldNode.key >>> (bitLevel * (level + 1))
          val newNode = new IntMap.EntryValue(initKey, value)
          array(index) = mergeNodes(currentKey1, newNode, currentKey2, oldNode)
          _size += 1
        }
      case arrayValue: EntryArray =>
        recurUpdate(arrayValue.underlying, currentKey >>> bitLevel, initKey, value, level + 1, arrayValue)
    }
  }

  override def update(key: Int, value: T): Unit = {
    recurUpdate(underlying, key, key, value, 0, null)
  }

  override def +=(kv: (Int, T)): IntMap.this.type = {
    this (kv._1) = kv._2
    this
  }

  private def recurRemove(array: Array[AnyRef],
                          currentKey: Int,
                          initKey: Int,
                          level: Int,
                          lastArray: EntryArray): Unit = {
    val index = currentKey & mask
    array(index) match {
      case null =>
      case oldNode: EntryValue =>
        if (oldNode.key == initKey) {
          array(index) = null
          _size -= 1
          if (lastArray != null) {
            lastArray.size -= 1
          }
        }
      case arrayValue: EntryArray =>
        recurRemove(arrayValue.underlying, currentKey >>> bitLevel, initKey, level + 1, arrayValue)

        // убиваем если оказался пустым
        if (lastArray != null && arrayValue.size == 0) {
          array(index) = null
          lastArray.size -= 1
        }
    }
  }

  override def -=(key: Int): IntMap.this.type = {
    recurRemove(underlying, key, key, 0, null)
    this
  }

  override def get(key: Int): Option[T] = Option(getOrNull(key))

  private def recurGetOrNull(array: Array[AnyRef], currentKey: Int, initKey: Int): Any = {
    val index = currentKey & mask
    array(index) match {
      case null => null
      case x: EntryValue =>
        if (x.key == initKey) x.value else null
      case arrayValue: EntryArray =>
        recurGetOrNull(arrayValue.underlying, currentKey >>> bitLevel, initKey)
    }
  }

  def getOrNull(key: Int): T = recurGetOrNull(underlying, key, key).asInstanceOf[T]

  override def foreach[U](f: ((Int, T)) => U): Unit = {
    def arrayForeach(array: Array[AnyRef]): Unit = {
      array.foreach {
        case null =>
        case value: EntryValue =>
          val tuple = (value.key, value.value.asInstanceOf[T])
          f(tuple)
        case x: EntryArray => arrayForeach(x.underlying)
      }
    }

    arrayForeach(underlying)
  }

  override def iterator: Iterator[(Int, T)] = new AbstractIterator[(Int, T)] {
    private var left = _size
    private var currentArray = underlying
    private var currentIndex = 0
    private var indexStack: List[Int] = Nil
    private var arrayStack: List[Array[AnyRef]] = Nil

    private def findNext(): (Int, T) = {
      def reduceStack(): Unit = {
        if (indexStack.nonEmpty) {
          currentArray = arrayStack.head
          currentIndex = indexStack.head
          arrayStack = arrayStack.tail
          indexStack = indexStack.tail
        }
      }

      if (currentIndex < levelSize) {
        currentArray(currentIndex) match {
          case null =>
            currentIndex += 1
            findNext()
          case obj: EntryArray =>
            val newArray = obj.underlying
            currentIndex += 1
            indexStack = currentIndex :: indexStack
            arrayStack = currentArray :: arrayStack
            currentArray = newArray
            currentIndex = 0
            findNext()
          case obj: EntryValue =>
            val res = (obj.key, obj.value.asInstanceOf[T])
            currentIndex += 1
            if (currentIndex >= levelSize)
              reduceStack()
            res
        }
      } else {
        reduceStack()
        findNext()
      }
    }


    override def next(): (Int, T) = {
      if (hasNext) {
        left -= 1
        findNext()
      }
      else Iterator.empty.next()
    }

    override def hasNext: Boolean = left != 0
  }


  override def stringPrefix: String = "IntMap"
}
