package a14e.collz.mut

import scala.collection.mutable

/**
  * Created by User on 09.01.2017.
  */
object PrefixMap {

  sealed class Node

  class Leaf(val key: String,
             val value: AnyRef,
             val startIndex: Int,
             val validCount: Int) extends Node

  class NonEmptyNode(val leaves: IntMap[Node],
                     val key: String,
                     val startIndex: Int,
                     val validCount: Int) extends Node


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

  // особый индекс для символа, который отвечает за пустую строку =)
  // такой вид, так как если я захочу приводить к беззнаковому виду
  // но найбольший номер у char будет 0xFF, а это не единичку больше =)
  final val emptyStringIndex = 0x100
}


class PrefixMap[T](root: PrefixMap.Node) extends mutable.Map[String, T] {

  import PrefixMap._

  private def recGetOrNull(startIndex: Int,
                           key: String,
                           node: Node): Any = node match {
    case l: Leaf =>
      val count = countEquals(key, l.key, l.startIndex)
      if (count == l.validCount) l.value
      else null
    case n: NonEmptyNode =>
      val count = countEquals(key, n.key, n.startIndex)
      if (count != n.validCount) null
      else {
        val internalKey =
          if (key.length == startIndex) // проверяем на пустые строки
            emptyStringIndex else key.charAt(startIndex).toInt
        val found = n.leaves.getOrNull(internalKey)
        if (found == null) null
        else {
          recGetOrNull(startIndex + n.validCount, key, found)
        }
      }
  }

  def getOrNull(key: String): T = recGetOrNull(0, key, root).asInstanceOf[T]

  override def get(key: String): Option[T] = Option(getOrNull(key))

  override def contains(key: String): Boolean = getOrNull(key) != null

  override def +=(kv: (String, T)): PrefixMap.this.type = ???

  override def -=(key: String): PrefixMap.this.type = ???


  override def iterator: Iterator[(String, T)] = ???
}
