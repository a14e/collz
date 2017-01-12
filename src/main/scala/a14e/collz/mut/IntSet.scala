/*
* This source code is licensed under the MIT license found in the
* LICENSE.txt file in the root directory of this source tree
*/
package a14e.collz.mut

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable


object IntSet {

  implicit def canBuildFrom = new CanBuildFrom[mutable.Set[_], Int, IntSet] {
    def apply(from: mutable.Set[_]): mutable.Builder[Int, IntSet] = apply()

    def apply(): mutable.Builder[Int, IntSet] = IntSet()
  }

  def apply(xs: Int*): IntSet = new IntSet(IntMap[Unit]()) ++= xs
}

/**
  * коллекция обертка поверх IntMap для реализации множества оптимизированного для работы с Int
  * операции добавления, удаления и поиска выполняются за O(log16(n)) в худшем случае
  *
  * более подробно реализациб см в mut.IntMap
  * @param underlying коллекция на базе которой построено множество
  */
class IntSet private[collz](val underlying: IntMap[Unit]) extends mutable.Set[Int] with mutable.Builder[Int, IntSet] {

  override def size: Int = underlying.size

  override def isEmpty: Boolean = underlying.isEmpty

  override def newBuilder: mutable.Builder[Int, mutable.Set[Int]] = IntSet()

  override def result(): IntSet = this

  override def clear(): Unit =  {
    underlying.clear()
  }

  /**
    * добавление элемента elem в множество
    * сложность  O(log16(n))
    * @param elem элемент для добавления
    * @return текущая коллекция
    */
  override def +=(elem: Int): IntSet.this.type = {
    underlying(elem) = Unit
    this
  }
  /**
    * удаление elem из множества. если элемента
    * в коллекци нет, то ничего не произойдет
    * сложность  O(log16(n))
    * @param elem элемент для удаления
    * @return текущая коллекция
    */
  override def -=(elem: Int): IntSet.this.type = {
    underlying -= elem
    this
  }

  /**
    * проверка на пренадлежность elem к множеству
    * сложность  O(log16(n))
    * @param elem элемент для проверки
    * @return true если содержится в коллекции и false в других случаях
    */
  override def contains(elem: Int): Boolean = underlying.contains(elem)

  override def iterator: Iterator[Int] = underlying.keysIterator

  override def foreach[U](f: (Int) => U): Unit = for ((k, _) <- underlying) f(k)

}
