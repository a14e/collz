/*
* This source code is licensed under the MIT license found in the
* LICENSE.txt file in the root directory of this source tree
*/
package a14e.collz.mut

import scala.annotation.tailrec
import scala.collection.{AbstractIterator, mutable}
object PrefixMap {

  sealed trait Node

  class Leaf(val key: String,
             val value: Any,
             val startIndex: Int,
             val validCount: Int) extends Node

  class NonEmptyNode(val leaves: IntMap[Node],
                     val key: String,
                     val startIndex: Int,
                     val validCount: Int) extends Node


  def leafIterator[T](leaf: Leaf): Iterator[(String, T)] = Iterator.single((leaf.key, leaf.value.asInstanceOf[T]))

  def nodeIterator[T](leaves: IntMap[Node]): Iterator[(String, T)] = new AbstractIterator[(String, T)] {

    private var stack: List[Iterator[(Int, Node)]] = Nil
    private var currentIterator: Iterator[(Int, Node)] = leaves.iterator


    @tailrec
    private final def calcNext(): (String, T) = {
      if (currentIterator.hasNext) {
        currentIterator.next()._2 match {
          case l: Leaf => (l.key, l.value.asInstanceOf[T])
          case n: NonEmptyNode =>
            stack = currentIterator :: stack
            currentIterator = n.leaves.iterator
            calcNext()
        }

      } else {
        if (stack.isEmpty) null
        else {
          currentIterator = stack.head
          stack = stack.tail
          calcNext()
        }
      }
    }

    private var nextValue = calcNext()

    override def next(): (String, T) =
      if (hasNext) {
        val res = nextValue
        nextValue = calcNext()
        res
      } else Iterator.empty.next()

    override def hasNext: Boolean = nextValue != null
  }

  private final def minOf3(x: Int, y: Int, z: Int): Int = {
    math.min(math.min(x, y), z)
  }

  /**
    * функция для вычисления количества общих символов, в строках
    * first и second начиная с индекса start, причем количество вычесленных
    * символов не будет превышать maxCount
    *
    * @param first    первая строка
    * @param second   вторая строка
    * @param start    начальный индекс
    * @param maxCount максимальный результат
    * @return общее количество символов в строках first и second начиная с индекса start, но
    *         не большее чем maxCount
    */
  private[collz] def countEquals(first: String, second: String, start: Int, maxCount: Int): Int = {
    val minLen = minOf3(first.length, second.length, start + maxCount)
    if (minLen <= start)
      return 0

    var count = 0
    var i = start
    while (i < minLen) {
      val char1 = first.charAt(i)
      val char2 = second.charAt(i)

      if (char1 != char2) return count
      else count += 1

      i += 1
    }

    count
  }

  def apply[T](kvs: (String, T)*): PrefixMap[T] = new PrefixMap[T](new IntMap[Node](), 0) ++= kvs

  // особый индекс для символа, который отвечает за пустую строку =)
  // такой вид, так как если я захочу приводить к беззнаковому виду
  // но найбольший номер у char будет 0xFF, а это не единичку больше =)
  final val emptyStringIndex = 0x100
}

import PrefixMap._

/**
  * коллекция оптимизированная для работы со строками. При создании за основу было взято префиксное дерево
  * Trie. в каждом узле которого находится mut.IntMap. Таким образом
  * худшее время поиска, добавления, удаления элементов не зависит от числа элементов будет занимать
  * O(n * log16(k)) n -- длина строки, k - размер алфавита. Так как размер алфавита в нашем случае
  * 256, то имеем сложность 2 * O(n) для худшего случае, зависящую только от длины строки.
  * Более точным будет вместо длины строки принимать количество коллизий для ключа, что обычно
  * значительно меньше чем длинна строки.
  * в лучшем случае, когда нет ни одного другого ключа, начинающегося на ту же букву, сложность большинства
  * операций будет занимать 2 * O(1), что примерно равно сложности работы с IntMap, когда максимальное число 256.
  * для удобства поиска пустых строк был добавлен новый номер индекса 0xFF + 1 = 256 для пустых строк =)
  *
  * в описании данной стрктуры термины ключ и строка применяются в одном смысле
  *
  * @param root корневой узел
  * @param _size размер коллекции
  * @tparam T тип значений в коллекции
  */
class PrefixMap[T] private[collz](private val root: IntMap[Node],
                                  private var _size: Int) extends mutable.Map[String, T] {


  override def size: Int = _size

  override def clear(): Unit = {
    root.clear()
    _size = 0
  }

  /**
    * вычисление индекса по ключу и смещению относительно начала строки
    * если startIndex равен длине строки(индекс для пустого символа), тогда мы считаем, что индекс равен
    * 0x100,
    * иначе просто приводим Char по инлексу startIndex к типу Int
    * @param key строка, по которой стоит брать ключ
    * @param startIndex индекс в строке для ключа
    * @return индекс в узле для данного ключа
    */
  private def calcIndex(key: String, startIndex: Int): Int = {
    if (key.length == startIndex)
      emptyStringIndex
    else
      key.charAt(startIndex).toInt
  }

  /**
    * рекурентная функция для поиска значения в префиксном редеве по ключу
    * сложность поиска в худшем случае зависит только от длины ключа(строк)
    * O(n), где n -- длина строки
    * @param startIndex индекс в строке для проверки в текущем узле
    * @param key ключ для поиск
    * @param leaves IntMap для хранения данных в нелистовых узлах
    * @return найденное значение, приведенное к типу Any, либо null
    */
  @tailrec
  private def recGetOrNull(startIndex: Int,
                           key: String,
                           leaves: IntMap[Node]): Any = {
    val internalKey = calcIndex(key, startIndex)

    leaves.getOrNull(internalKey) match {
      case null => null
      case l: Leaf =>
        val count = countEquals(key, l.key, l.startIndex, l.validCount)
        if (count == l.validCount) l.value
        else null
      case n: NonEmptyNode =>
        val count = countEquals(key, n.key, n.startIndex, n.validCount)
        if (count == n.validCount) recGetOrNull(startIndex + count, key, n.leaves)
        else null
    }
  }

  /**
    * функция возвращает значение с данным ключем если найдено, либо null
    * сложность операции O(n), где n -- длина ключа
    * @param key ключ для поиска
    * @return значение с данным ключем если найдено, либо null
    */
  def getOrNull(key: String): T = recGetOrNull(0, key, root).asInstanceOf[T]

  /**
    * функция возвращает Some(...) c значеним с данным ключем если найдено, либо None
    * сложность операции O(n), где n -- длина ключа
    * @param key ключ для поиска
    * @return Some(...) c значеним с данным ключем если найдено, либо None
    */
  override def get(key: String): Option[T] = Option(getOrNull(key))

  /**
    * функция возвращает true если данная коллекция содержит ключ key, иначе false
    * сложность операции O(n), где n -- длина ключа
    * @param key ключ для поиска
    * @return true если данная коллекция содержит ключ key, иначе false
    */
  override def contains(key: String): Boolean = getOrNull(key) != null

  @tailrec
  private def recHasPrefix(startIndex: Int,
                           key: String,
                           leaves: IntMap[Node]): Boolean = {
    val internalKey = calcIndex(key, startIndex)

    leaves.getOrNull(internalKey) match {
      case null => false
      case l: Leaf =>
        val count = countEquals(key, l.key, l.startIndex, l.validCount)
        count + startIndex == key.length
      case n: NonEmptyNode =>
        val count = countEquals(key, n.key, n.startIndex, n.validCount)
        if (count == n.validCount) recHasPrefix(startIndex + count, key, n.leaves)
        else false
    }
  }

  def hasPrefix(key: String): Boolean = recHasPrefix(0, key, root)


  @tailrec
  private def recFindByPrefix(startIndex: Int,
                              key: String,
                              leaves: IntMap[Node]): Iterator[(String, T)] = {
    val internalKey = calcIndex(key, startIndex)

    leaves.getOrNull(internalKey) match {
      case null => Iterator.empty
      case leaf: Leaf =>
        val count = countEquals(key, leaf.key, leaf.startIndex, leaf.validCount)
        if (count + startIndex == key.length) leafIterator[T](leaf)
        else Iterator.empty
      case node: NonEmptyNode =>
        val count = countEquals(key, node.key, node.startIndex, node.validCount)

        if (count + startIndex == key.length)
          PrefixMap.nodeIterator[T](node.leaves)
        else if (count == node.validCount)
          recFindByPrefix(startIndex + count, key, node.leaves)
        else Iterator.empty
    }
  }

  def findForPrefix(key: String): Iterator[(String, T)] = {
    if (key.isEmpty) this.iterator
    else recFindByPrefix(0, key, root)
  }


  private def leafIndex(leaf: Leaf): Int = {
    // проверяем на пустые строки
    if (leaf.validCount == 0)
      emptyStringIndex
    else
      leaf.key.charAt(leaf.startIndex).toInt
  }

  private def mergeLeaves(init: Leaf, toAdd: Leaf): Node = {
    val maxCount = math.min(init.validCount, toAdd.validCount)
    val count = countEquals(init.key, toAdd.key, init.startIndex, maxCount)

    val firstInSecond = count == init.validCount
    val secondInFirst = count == toAdd.validCount
    if (firstInSecond && secondInFirst) // если строки совпадают
      return toAdd


    val node =
      new NonEmptyNode(IntMap[Node](), toAdd.key, init.startIndex, count)
    val newStartIndex = init.startIndex + count
    val newLeaf1 = new Leaf(init.key, init.value, newStartIndex, init.validCount - count)
    val newLeaf2 = new Leaf(toAdd.key, toAdd.value, newStartIndex, toAdd.validCount - count)

    val key1 = leafIndex(newLeaf1)
    val key2 = leafIndex(newLeaf2)

    node.leaves(key1) = newLeaf1
    node.leaves(key2) = newLeaf2

    node
  }

  private def mergeNodeAndLeaf(init: NonEmptyNode, toAdd: Leaf, count: Int): Node = {


    val node =
      new NonEmptyNode(IntMap[Node](), toAdd.key, init.startIndex, count)
    val newStartIndex = init.startIndex + count

    val node1 = new NonEmptyNode(init.leaves, init.key, newStartIndex, init.validCount - count)
    val node2 = new Leaf(toAdd.key, toAdd.value, newStartIndex, toAdd.validCount - count)

    val key1 = if (node1.validCount == 0) emptyStringIndex else node1.key.charAt(newStartIndex).toInt
    val key2 = leafIndex(node2)

    node.leaves(key1) = node1
    node.leaves(key2) = node2

    node
  }


  @tailrec
  private def recAdd(startIndex: Int,
                     key: String,
                     value: Any,
                     leaves: IntMap[Node]): Unit = {


    def newLeaf() = new Leaf(key, value, startIndex, key.length - startIndex)

    val internalKey = calcIndex(key, startIndex)

    leaves.getOrNull(internalKey) match {
      case null =>
        leaves(internalKey) = newLeaf()
        _size += 1
      case leaf: Leaf =>
        val created = newLeaf()
        val merged = mergeLeaves(leaf, created)
        leaves(internalKey) = merged

        if (created ne merged)
          _size += 1
      case foundNode: NonEmptyNode =>
        val count = countEquals(foundNode.key, key, foundNode.startIndex, foundNode.validCount)
        if (count == foundNode.validCount) {
          recAdd(startIndex + count, key, value, foundNode.leaves)
        } else {
          leaves(internalKey) = mergeNodeAndLeaf(foundNode, newLeaf(), count)
          _size += 1
        }
    }
  }

  override def update(key: String, value: T): Unit = {
    recAdd(0, key, value, root)
  }

  override def +=(kv: (String, T)): PrefixMap.this.type = {
    this (kv._1) = kv._2
    this
  }


  @tailrec
  private def recRemove(startIndex: Int,
                        key: String,
                        leaves: IntMap[Node],
                        previousLeaves: IntMap[Node],
                        previousLeaveKey: Int): Unit = {

    val internalKey = calcIndex(key, startIndex)

    leaves.getOrNull(internalKey) match {
      case null =>
      case leaf: Leaf =>
        val leafCount = countEquals(leaf.key, key, leaf.startIndex, leaf.validCount)
        val validDelete = leafCount == leaf.validCount && leafCount + startIndex == key.length
        if (validDelete) {
          leaves -= internalKey
          _size -= 1
          if (leaves.isEmpty && (previousLeaves ne null))
            previousLeaves -= previousLeaveKey
        }
      case foundNode: NonEmptyNode =>
        val count = countEquals(foundNode.key, key, foundNode.startIndex, foundNode.validCount)
        if (count == foundNode.validCount) {
          /** иногда чистим пустые узлы чтобы было меньше
            * утечек мусора. для полного их избежания
            * пока не хватает фантазии =)
            * */
          if (foundNode.leaves.isEmpty)
            leaves -= internalKey
          else
            recRemove(startIndex + count, key, foundNode.leaves, leaves, internalKey)
        }
    }
  }

  override def -=(key: String): PrefixMap.this.type = {
    recRemove(0, key, root, null, -1)
    this
  }


  override def iterator: Iterator[(String, T)] = PrefixMap.nodeIterator[T](root)
}
