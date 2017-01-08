package a14e.collz.immut

import scala.annotation.tailrec
import scala.collection.{AbstractIterator, GenTraversableOnce}


/**
  * Created by Borisenko Andrew on 23.11.2016.
  * immutable String Set implementation, based on Trie or Prefix Tree
  */
class PrefixTree private[collz](val nodes: PrefixTreeLeaves,
                                override val size: Int) extends collection.immutable.Set[String] {

  self =>

  override def contains(elem: String): Boolean = {
    if (nodes eq null) false
    else nodes.testContains(elem)
  }

  override def +(elem: String): PrefixTree = {
    if (nodes eq null) new PrefixTree(PrefixTreeLeaves(PrefixTreeNode(elem)), 1)
    else {
      val newNodes = nodes.addValue(elem, 0, Nil, Nil)
      if (newNodes eq nodes) this
      else new PrefixTree(newNodes, size + 1)
    }
  }

  override def ++(elems: GenTraversableOnce[String]): PrefixTree = {
    var res: PrefixTree = this
    elems.foreach(res += _)
    res
  }

  override def --(elems: GenTraversableOnce[String]): PrefixTree = {
    var res = this
    elems.foreach(res -= _)
    res
  }

  override def -(elem: String): PrefixTree = {
    if (nodes eq null) this
    else {
      val newNodes = nodes.removeValue(elem, Nil, Nil)
      if (newNodes eq nodes) this
      else new PrefixTree(newNodes, size - 1)
    }
  }


  override def iterator: Iterator[String] = {
    if (nodes eq null) Iterator.empty
    else
      new AbstractIterator[String] {
        private var queue = self.nodes.iterator :: Nil
        private var _left = self.size

        override def hasNext: Boolean = _left != 0

        @tailrec
        def findNextValue(): String = {
          val currentIter = queue.head
          if (currentIter.hasNext) {
            val currentValue = currentIter.next()
            if (currentValue.hasValue) {
              currentValue.value
            } else {
              queue = currentValue.iterator :: queue
              findNextValue()
            }
          } else {
            queue = queue.tail
            findNextValue()
          }
        }

        override def next(): String = {
          if (hasNext) {
            _left -= 1
            findNextValue()
          } else Iterator.empty.next()
        }
      }
  }
}

object PrefixTree {
  def apply(elems: String*): PrefixTree = empty ++ elems

  def empty = new PrefixTree(null, 0)


}

case class PrefixTreeNode private[collz](value: String,
                                         offset: Int,
                                         offsetPlusLimit: Int,
                                         leaves: PrefixTreeLeaves) {

  import PrefixTreeNode._

  def withNodes(newNodes: PrefixTreeLeaves): PrefixTreeNode = copy(leaves = newNodes)

  def withOffset(newOffset: Int): PrefixTreeNode = copy(offset = newOffset)


  def withValue(newValue: String): PrefixTreeNode = copy(value = newValue)

  def containsStatus(other: String): Int = {
    val otherLen = other.length

    //empty element
    if (offsetPlusLimit == offset) {
      if (offset == otherLen)
        return CONTAIN
      else
        return NOT_CONTAIN
    }

    if (otherLen <= offset)
      return NOT_CONTAIN

    def isMatches = value.regionMatches(offset, other, offset, offsetPlusLimit - offset)

    //whole string in elem
    if (offsetPlusLimit == otherLen) {
      if (value.charAt(offset) == other.charAt(offset)) {
        if (isMatches) {
          if (leaves eq null)
            return CONTAIN
          else
            return CAN_CONTAIN
        } else return DEFINITELY_NOT_CONTAIN
      } else
        return NOT_CONTAIN
    }


    if (leaves eq null) {
      if (value.charAt(offset) != other.charAt(offset))
        NOT_CONTAIN
      else {
        if (isMatches && other.length == offsetPlusLimit)
          CONTAIN
        else
          DEFINITELY_NOT_CONTAIN
      }
    } else {
      if (isMatches) CAN_CONTAIN
      else NOT_CONTAIN
    }
  }


  def mergedWith(other: String, newOffsetPlusLimit: Int): PrefixTreeNode = {
    val internalNode1 = new PrefixTreeNode(other, newOffsetPlusLimit, other.length, null)
    val internalNode2 = withOffset(newOffsetPlusLimit)

    val nodesList = internalNode1 +: PrefixTreeLeaves(internalNode2)

    val resultNode = new PrefixTreeNode(other, offset, newOffsetPlusLimit, nodesList)
    resultNode
  }

  /**
    * calc positive number if can merge or one of merge statuses
    *
    * @param other string ti test merge
    * @return one of statuses
    *
    *         MERGE_NEW_MAX - any positive numble
    *         NO_MERGE = -1 -- cant merge
    *         CAN_MERGE = -2 -- can merge in child nodes
    *         ALREADY_MERGED = -3 -- already contain such element
    */
  def mergeStatus(other: String): Int = {
    val countEqual = countEquals(value, other, offset, offsetPlusLimit)
    if (countEqual == 0) {
      if (offset == offsetPlusLimit &&
        other.length == offset) return ALREADY_MERGED // для нулевых строк
      else return NO_MERGE
    }

    val newMax = countEqual + offset

    if (newMax == offsetPlusLimit) {
      if (newMax == other.length) {
        if (leaves eq null) return ALREADY_MERGED
      }
      if (leaves eq null) return newMax
      // если все совпали
      return CAN_MERGE
    }
    newMax
  }


  def hasValue: Boolean = leaves eq null

  //can throw null pointer exception
  def iterator: Iterator[PrefixTreeNode] = leaves.iterator
}

private[collz] object PrefixTreeNode {

  //DO NOT use with negative sizes
  def countEquals(str1: String, str2: String, offset: Int, offsetPlusLimit: Int): Int = {
    val realLimit = math.min(math.min(str1.length, str2.length), offsetPlusLimit)
    var count = 0
    var i = offset
    while (i < realLimit) {
      if (str1.charAt(i) == str2.charAt(i))
        count += 1
      else
        return count
      i += 1
    }
    count
  }


  /** search statuses */
  val CONTAIN: Int = 0
  val CAN_CONTAIN: Int = 1
  val NOT_CONTAIN: Int = 2
  val DEFINITELY_NOT_CONTAIN: Int = 3 // appear if some match exists

  def apply(element: String, offset: Int): PrefixTreeNode = {
    new PrefixTreeNode(element, offset, element.length, null)
  }

  def apply(element: String): PrefixTreeNode = apply(element, 0)

  //val MERGE_NEW_MAX = 0
  val NO_MERGE: Int = -1
  val CAN_MERGE: Int = -2
  val ALREADY_MERGED: Int = -3

}

//TODO сделать оптимизацию для длинных списоков в IntMap
case class PrefixTreeLeaves private[collz](elem: PrefixTreeNode,
                                           protected var _tail: PrefixTreeLeaves) {
  self =>

  def tail: PrefixTreeLeaves = _tail

  @tailrec
  final def testContains(other: String): Boolean = {
    import PrefixTreeNode._
    var current = this
    var state = NOT_CONTAIN
    var notStop = true
    while (notStop) {
      state = current.elem.containsStatus(other)
      if (state != NOT_CONTAIN)
        notStop = false
      else {
        current = current.tail
        if (current eq null) {
          notStop = false
        }
      }
    }

    state match {
      case CONTAIN => true
      case CAN_CONTAIN => current.elem.leaves.testContains(other)
      case DEFINITELY_NOT_CONTAIN => false
      case NOT_CONTAIN => false
    }
  }

  private def buildLeaf(el: PrefixTreeNode, list: PrefixTreeLeaves): PrefixTreeLeaves = {
    val res = PrefixTreeLeaves(el)
    res._tail = list
    res
  }


  @tailrec
  final def addValue(str: String,
                     offset: Int,
                     previousLists: List[PrefixTreeLeaves],
                     previousNodes: List[PrefixTreeNode]): PrefixTreeLeaves = {

    import PrefixTreeNode._
    var current = this
    var state = NO_MERGE
    var notStop = true
    while (notStop) {
      state = current.elem.mergeStatus(str)
      if (state != NO_MERGE)
        notStop = false
      else {
        current = current.tail
        if (current eq null) {
          notStop = false
        }
      }
    }

    state match {
      case NO_MERGE =>
        val newLeaves = buildLeaf(PrefixTreeNode(str, offset), this)
        if (previousNodes.isEmpty) newLeaves
        else {
          val newNode = previousNodes.head.withNodes(newLeaves)
          reverseBuildTree(previousLists, previousNodes, newNode)
        }
      case CAN_MERGE =>
        val el = current.elem
        el.leaves.addValue(str, el.offsetPlusLimit, this :: previousLists, el :: previousNodes)
      case ALREADY_MERGED =>
        if (previousLists.isEmpty) this else previousLists.last
      case newMax =>
        val el = current.elem
        val merged = el.mergedWith(str, newMax)

        reverseBuildTree(this :: previousLists, el :: previousNodes, merged)
    }
  }

  @tailrec
  protected final def reverseBuildTree(lists: List[PrefixTreeLeaves],
                                       nodes: List[PrefixTreeNode],
                                       replaceNode: PrefixTreeNode): PrefixTreeLeaves = {
    val currentList = lists.head
    val currentNode = nodes.head
    val withRemoved = currentList.withoutElementByRef(currentNode)

    val newList = buildLeaf(replaceNode, withRemoved)

    val listsTail = lists.tail

    if (listsTail.isEmpty) newList
    else {
      val nodesTail = nodes.tail
      val newNode = nodesTail.head.withNodes(newList)
      reverseBuildTree(listsTail, nodesTail, newNode)
    }
  }

  @tailrec
  final def removeValue(str: String,
                        previousLists: List[PrefixTreeLeaves],
                        previousNodes: List[PrefixTreeNode]): PrefixTreeLeaves = {

    import PrefixTreeNode._
    var current = this
    var state = NOT_CONTAIN
    var notStop = current ne null
    while (notStop) {
      state = current.elem.containsStatus(str)
      if (state != NOT_CONTAIN)
        notStop = false
      else {
        current = current.tail
        if (current eq null) {
          notStop = false
        }
      }
    }

    state match {
      case CONTAIN =>

        val withoutCurrent: PrefixTreeLeaves = withoutElementByRef(current)
        if (previousLists.isEmpty)
          return withoutCurrent

        val prevNode = previousNodes.head

        val newNode = if (PrefixTreeLeaves.isSizeOne(withoutCurrent)) {
          val elem = withoutCurrent.elem
          val res = prevNode.copy(
            leaves = elem.leaves,
            value = elem.value,
            offsetPlusLimit = elem.offsetPlusLimit
          )
          res
        } else prevNode.withNodes(withoutCurrent)

        reverseBuildTree(previousLists, previousNodes, newNode)
      case CAN_CONTAIN =>
        val elem = current.elem
        elem.leaves.removeValue(str, this :: previousLists, elem :: previousNodes)
      case NOT_CONTAIN | DEFINITELY_NOT_CONTAIN =>
        if (previousLists.isEmpty) this else previousLists.last
    }

  }

  //  }


  def +:(el: PrefixTreeNode) = PrefixTreeLeaves(el, this)

  //use only for tests
  //DO NOT use with negative indexes
  def drop(index: Int): PrefixTreeLeaves = {
    var i = 0
    var current = this
    while (current != null && i != index) {
      i += 1
      current = current.tail
    }
    current
  }

  private def mutableConcat(el1: PrefixTreeLeaves, el2: PrefixTreeLeaves): PrefixTreeLeaves = {
    el1._tail = el2
    el1
  }

  private def mutablePushBackReturnLast(el1: PrefixTreeLeaves, el2: PrefixTreeNode): PrefixTreeLeaves = {
    el1._tail = new PrefixTreeLeaves(el2, null)
    el1._tail
  }

  def withoutElementByRef(obj: PrefixTreeLeaves): PrefixTreeLeaves = {
    if (obj eq this) tail
    else {
      val first = new PrefixTreeLeaves(elem, null)
      var last = first
      var current = tail
      while (current ne null) {
        if (current eq obj) {
          current = current.tail

          mutableConcat(last, current)
          return first
        } else {
          last = mutablePushBackReturnLast(last, current.elem)
          current = current.tail
        }
      }
      this
    }
  }


  def withoutElementByRef(obj: PrefixTreeNode): PrefixTreeLeaves = {
    if (obj eq elem) tail
    else {
      val first = new PrefixTreeLeaves(elem, null)
      var last = first
      var current = tail
      while (current ne null) {
        if (current.elem eq obj) {
          current = current.tail

          mutableConcat(last, current)
          return first
        } else {
          last = mutablePushBackReturnLast(last, current.elem)
          current = current.tail
        }
      }
      this
    }
  }

  def size: Int = {
    var current = this
    var res = 0
    while (current != null) {
      res += 1
      current = current.tail
    }
    res
  }

  def foreach(f: PrefixTreeNode => Unit): Unit = iterator.foreach(f)

  def exists(f: PrefixTreeNode => Boolean): Boolean = iterator.exists(f)

  def toList: List[PrefixTreeNode] = iterator.toList

  def iterator: Iterator[PrefixTreeNode] = new AbstractIterator[PrefixTreeNode]() {
    private var current = self

    override def hasNext(): Boolean = current ne null

    override def next(): PrefixTreeNode = {
      if (current eq null) Iterator.empty.next()
      else {
        val res = current.elem
        current = current.tail
        res
      }
    }
  }


}

private[collz] object PrefixTreeLeaves {

  def isSizeOne(col: PrefixTreeLeaves): Boolean = (col ne null) && (col.tail eq null)

  def apply(elem1: PrefixTreeNode): PrefixTreeLeaves = {
    new PrefixTreeLeaves(elem1, null)
  }

  def apply(elem1: PrefixTreeNode, elem2: PrefixTreeNode, others: PrefixTreeNode*): PrefixTreeLeaves = {
    var last = PrefixTreeLeaves(elem2)
    val result = new PrefixTreeLeaves(elem1, last)

    if (others.nonEmpty)
      others.foreach { current =>
        val newValue = PrefixTreeLeaves(current)
        last._tail = newValue
        last = newValue
      }
    result
  }
}
