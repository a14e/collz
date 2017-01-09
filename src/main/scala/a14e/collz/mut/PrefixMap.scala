package a14e.collz.mut

import scala.annotation.tailrec
import scala.collection.{AbstractIterator, mutable}

/**
  * Created by User on 09.01.2017.
  */
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


    def calcNext(): (String, T) = {
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


  private[collz] def countEquals(first: String, second: String, start: Int): Int = {
    val minLen = math.min(first.length, second.length)
    if (minLen <= start)
      return 0
    var i = start
    var count = 0
    while (i < minLen) {
      val char1 = first.charAt(i)
      val char2 = second.charAt(i)

      if (char1 == char2) count += 1
      else return count

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

//важно, что корнейвой узел всегда пустая строка, что
//позволяет упростить некоторые алгоритмы
import PrefixMap._

class PrefixMap[T]private[collz](root: IntMap[Node], var _size: Int) extends mutable.Map[String, T] {


  override def size: Int = _size

  override def clear(): Unit = {
    root.clear()
    _size = 0
  }

  @tailrec
  private def recGetOrNull(startIndex: Int,
                           key: String,
                           leaves: IntMap[Node]): Any = {
    val internalKey =
      if (key.length == startIndex) // проверяем на пустые строки
        emptyStringIndex else key.charAt(startIndex).toInt

    leaves.getOrNull(internalKey) match {
      case null => null
      case l: Leaf =>
        val count = countEquals(key, l.key, l.startIndex)
        if (count == l.validCount) l.value
        else null
      case n: NonEmptyNode =>
        val count = countEquals(key, n.key, n.startIndex)
        if (count != n.validCount) null
        else {
          recGetOrNull(startIndex + count, key, n.leaves)
        }
    }
  }

  def getOrNull(key: String): T = recGetOrNull(0, key, root).asInstanceOf[T]

  override def get(key: String): Option[T] = Option(getOrNull(key))

  override def contains(key: String): Boolean = getOrNull(key) != null

  @tailrec
  private def recHasPrefix(startIndex: Int,
                           key: String,
                           leaves: IntMap[Node]): Boolean = {
    val internalKey =
      if (key.length == startIndex) // проверяем на пустые строки
        emptyStringIndex else key.charAt(startIndex).toInt

    leaves.getOrNull(internalKey) match {
      case null => false
      case l: Leaf =>
        val count = countEquals(key, l.key, l.startIndex)
        count + startIndex == key.length
      case n: NonEmptyNode =>
        val count = countEquals(key, n.key, n.startIndex)
        if (count != n.validCount) false
        else {
          recHasPrefix(startIndex + count, key, n.leaves)
        }
    }
  }

  def hasPrefix(key: String): Boolean = recHasPrefix(0, key, root)


  @tailrec
  private def recFindByPrefix(startIndex: Int,
                              key: String,
                              leaves: IntMap[Node]): Iterator[(String, T)] = {
    val internalKey =
      if (key.length == startIndex) // проверяем на пустые строки
        emptyStringIndex else key.charAt(startIndex).toInt

    leaves.getOrNull(internalKey) match {
      case null => Iterator.empty
      case leaf: Leaf =>
        val count = countEquals(key, leaf.key, leaf.startIndex)
        if (count + startIndex == key.length) leafIterator[T](leaf)
        else Iterator.empty
      case node: NonEmptyNode =>
        val count = countEquals(key, node.key, node.startIndex)

        if (count + startIndex == key.length)
          PrefixMap.nodeIterator[T](node.leaves)
        else if (count != node.validCount)
          Iterator.empty
        else
          recFindByPrefix(startIndex + count, key, node.leaves)
    }
  }

  def findForPrefix(key: String): Iterator[(String, T)] = recFindByPrefix(0, key, root)

  private def mergeLeaves(init: Leaf, toAdd: Leaf): Node = {
    val count = countEquals(init.key, toAdd.key, init.startIndex)

    val firstInSecond = count == init.validCount
    val secondInFirst = count == toAdd.validCount
    if (firstInSecond && secondInFirst) // если строки совпадают
      return toAdd


    val node =
      new NonEmptyNode(IntMap[Node](), toAdd.key, init.startIndex, count)
    val newStartIndex = init.startIndex + count
    val newLeaf1 = new Leaf(init.key, init.value, newStartIndex, init.validCount - count)
    val newLeaf2 = new Leaf(toAdd.key, toAdd.value, newStartIndex, toAdd.validCount - count)

    val key1 = if (newLeaf1.validCount == 0) emptyStringIndex else newLeaf1.key.charAt(newLeaf1.startIndex).toInt
    val key2 = if (newLeaf2.validCount == 0) emptyStringIndex else newLeaf2.key.charAt(newLeaf2.startIndex).toInt

    node.leaves(key1) = newLeaf1
    node.leaves(key2) = newLeaf2

    node
  }

  private def mergeNodeAndLeaf(init: NonEmptyNode, toAdd: Leaf, count: Int): Node = {


    val node =
      new NonEmptyNode(IntMap[Node](), toAdd.key, init.startIndex, count)
    val newStartIndex = init.startIndex + count

    val node1 = new NonEmptyNode(init.leaves, init.key, newStartIndex, toAdd.validCount - count)
    val node2 = new Leaf(toAdd.key, toAdd.value, newStartIndex, toAdd.validCount - count)

    val key1 = if (node1.validCount == 0) emptyStringIndex else node1.key.charAt(node1.startIndex).toInt
    val key2 = if (node2.validCount == 0) emptyStringIndex else node2.key.charAt(node2.startIndex).toInt

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

    val internalKey =
      if (key.length == startIndex) // проверяем на пустые строки
        emptyStringIndex else key.charAt(startIndex).toInt

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
        val count = countEquals(foundNode.key, key, foundNode.startIndex)
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
                        node: NonEmptyNode,
                        previousNode: NonEmptyNode,
                        previousLeaveKey: Int): Unit = {

    val count = countEquals(node.key, key, startIndex)

    if (count == node.validCount) {
      val internalKey =
        if (key.length == startIndex) // проверяем на пустые строки
          emptyStringIndex else key.charAt(startIndex).toInt
      val found = node.leaves.getOrNull(internalKey)
      found match {
        case null =>
        case leaf: Leaf =>
          val leafCount = countEquals(leaf.key, key, leaf.startIndex)
          if (leafCount == leaf.validCount) {
            node.leaves -= internalKey
            _size -= 1
            if (node.leaves.isEmpty)
              previousNode.leaves -= previousLeaveKey
          }

        case foundNode: NonEmptyNode =>
          recRemove(startIndex + count, key, foundNode, node, internalKey)
      }
    }
  }


  @tailrec
  private def recRemove(startIndex: Int,
                        key: String,
                        leaves: IntMap[Node],
                        previousLeaves: IntMap[Node],
                        previousLeaveKey: Int): Unit = {

    val internalKey =
      if (key.length == startIndex) // проверяем на пустые строки
        emptyStringIndex else key.charAt(startIndex).toInt

    leaves.getOrNull(internalKey) match {
      case null =>
      case leaf: Leaf =>
        val leafCount = countEquals(leaf.key, key, leaf.startIndex)
        if (leafCount == leaf.validCount) {
          leaves -= internalKey
          _size -= 1
          if (leaves.isEmpty && (previousLeaves ne null))
            previousLeaves -= previousLeaveKey
        }
      case foundNode: NonEmptyNode =>
        val count = countEquals(foundNode.key, key, foundNode.startIndex)
        if (count == foundNode.validCount) {
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
