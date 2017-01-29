/*
* This source code is licensed under the MIT license found in the
* LICENSE.txt file in the root directory of this source tree
*/
package a14e.collz.mut


import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}

import scala.annotation.tailrec
import scala.collection.concurrent.TrieMap
import scala.collection.generic.CanBuildFrom
import scala.collection.{AbstractIterator, mutable}
import scala.language.higherKinds
import scala.util.Random


object IntMap {

  implicit def canBuildFrom[B] = new CanBuildFrom[mutable.Map[_, _], (Int, B), IntMap[B]] {
    def apply(from: mutable.Map[_, _]): mutable.Builder[(Int, B), IntMap[B]] = apply()

    def apply(): mutable.Builder[(Int, B), IntMap[B]] = new IntMap[B]()
  }

  def apply[T](xs: (Int, T)*): IntMap[T] = new IntMap[T]() ++= xs

  /**
    * лист колелкции, где хранятся ключ и значение
    *
    * @param key   ключ узла
    * @param value значение в узле
    */
  private[collz] class EntryValue(val key: Int, var value: Any)

  /**
    * не листовой узел, где хранится массив, в котором
    * находся другие узлы типов EntryValue, EntryArray, либо нулевые элементы.
    * также содержит количество дочерних узлов. Размер хранится чтобы можно было удалять
    * узел, когда он станет пустым.
    *
    * индексы по которым находсят узлы в массиве зависят от ключа и глубины
    * вложенности, например, на глубине n индекс в массиве будет (key >>> bitLevel * n) & mask
    *
    * @param underlying массив дочерних узлов
    * @param size       количество дочерних узлов в массиве
    */
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
  *
  * Реализация изменяемого IntMap основанная на 16 битном префиксном дереве (16 bit trie)
  * за каждый символ(если считать что мы по символьно ъраним строку) принимаются куски ключа по
  * 4 бит, таким образом максимальная глубина дерева
  * для 32 битновго числа составляет 8 слоев.
  *
  * асимптотическая сложность поиска, добавления, удаления элементов O(log 16(n))
  *
  * структура данных изменяемая и не потокобезопасная.
  * значения null недопустимы
  *
  * многие функцие рекурентные, часть из них не на основе хвостой рекрусии. Это Ok, так
  * как мы заранее знаем, что глубина стека не превысит 8 =)
  *
  * Внутренняя структура очень похожа на стандартный неизменяемый HashMap, но значительно проще.
  *
  * простые бенчмарки показывают большое преимущество в скорости в сравнении
  * со стандартным изменяемым mutable.HashMap, immutable.HashMap и AnyRefMap,
  * IntMap, TreeMap при добавлении/удалеии (в 2 ... 8 раз)
  * при поиске значительной разницы в скорости нет =)
  *
  * итерации по коллекции значительно ммедленнее чем по изменяемым AnyRefMap и HashMap
  * так как приходится пробегать по большому количеству пустых полей
  *
  * значения внутри коллекции неупорядочены по ключам и при итерации могут идти
  * в непоследовательном порядке, но в некоторых случаях при итерации будут возникать
  * последоватьльно, что не должно вводить людей в заблуждение,
  * например ключи 0, 1, 2, 3 будут итерироваться последовательно, но
  * 16, 0, 1, 2, 3 будут идти в порядке 0, 16, 1, 2, 3
  */
class IntMap[T](private[collz] var underlying: Array[AnyRef] = new Array[AnyRef](IntMap.levelSize),
                private var _size: Int = 0)
  extends scala.collection.mutable.Map[Int, T] with collection.mutable.Builder[(Int, T), IntMap[T]] {

  override def clear(): Unit = {
    underlying = new Array[AnyRef](levelSize)
    _size = 0
  }

  override def isEmpty: Boolean = _size == 0

  override def result(): this.type = this

  override def size: Int = _size


  /**
    * фунуция для рекурсивного поиска элемента и определения содержится ли ключ внутри
    *
    * @param array      массив элементов внутри текущего узла
    * @param currentKey ключ для поиска, смещенный побитово в право на величину
    *                   bitLevel * n, где n - глубина вложенности
    * @param initKey    исходный ключ, так как мы currentKey смещаем каждый раз, то необходимо
    *                   тащить с собой исходный ключ, чтобы в конце проверить на равенство
    * @return true если array содержит внутри себя на некоторой глубине величину с ключем initKey
    *         иначе false
    */
  @tailrec
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

  /**
    * поиск ключа в коллекции
    * сложность операции O(log16(n)) в худшем случае
    * @param key ключ для поиска
    * @return true если есть такой клоюч и false в других случаях
    */
  override def contains(key: Int): Boolean = recurContains(underlying, key, key)

  //  override def ma

  /**
    * фукция для обьединения двух листовых узлов в один
    *
    * если текущие индексы совпадают, тогда будут рекурсивно
    * создаваться дополнительные уровни вложенности пока не достигнут
    * гулбины, где индексы различаются.
    *
    * для одинаковых ключей войдет в бесконечную (не хвостовую) рекурсию
    * и упадет со StackOveflow
    *
    * ключи должны иметь смещение для одного уровня
    *
    * @param currentKey1 смещенный ключ для текущего уровня первого узла
    * @param node1       первый листовой узед
    * @param currentKey2 смещенный ключ для текущего уровня второго узла
    * @param node2       второй листовой узед
    * @return EntryArray, содержащий узлы node1 и node2 на одном из уровней влощенности
    */
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

  /**
    * функция для рекурсивного поиска и обновления элемента по
    * ключу initKey внутри узлов в массиве array на значение value
    *
    * тащит за собой глубину вложенности level и прошлый узел lastArray
    * для того, чтобы в случае чего обновить размер в прошлом узле,
    * либо чтобы сместить ключ на нужное число уровней при обьединении двух узлов
    *
    * @param array      массив внутри которого ищем узлы
    * @param currentKey текущий ключ, побитово
    *                   смещенный на глубину вложенности умноженную на bitLevel
    * @param initKey    исходный ключ для сравнения при нахождении значения
    * @param value      значение, которое следует добавить в узел с ключем initKey
    * @param level      текущая глубина вложенности
    * @param lastArray  предыдущий узел, в котором содержится EntryArray,
    *                   в самом начале равен null
    */
  @tailrec
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

  /**
    * записывает в коллекцию по ключу key значение value.
    * предыдущее значение, если было, удаляется
    *
    * сложность операции в худшем случае O(log16(n))
    * @param key   ключ для добавления
    * @param value значение для добавления
    */
  override def update(key: Int, value: T): Unit = {
    recurUpdate(underlying, key, key, value, 0, null)
  }

  override def +=(kv: (Int, T)): IntMap.this.type = {
    this (kv._1) = kv._2
    this
  }

  /**
    * рекурсивная (не хвостовая) функция для удаления значения из узлов.
    * старые узлы с массивами удаляются только в том случае, когда size в узле
    * стала равно 0. Это необходимо, чтобы избежать поиска последнего элемента
    * по массиву каждый раз при удалении.
    *
    * если узел с ключем initKey не был найден, тогда функция ничего не делает
    *
    * @param array      массив внутри которого ведется поиск элементов по ключу
    * @param currentKey текущий ключ после смещений
    * @param initKey    текущий ключ без смещения
    * @param lastArray  компонент, где соджержится текущий узел,
    *                   в начале итераций равен 0.
    */
  private def recurRemove(array: Array[AnyRef],
                          currentKey: Int,
                          initKey: Int,
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
        recurRemove(arrayValue.underlying, currentKey >>> bitLevel, initKey, arrayValue)

        // убиваем если оказался пустым
        if (lastArray != null && arrayValue.size == 0) {
          array(index) = null
          lastArray.size -= 1
        }
    }
  }

  /**
    * удаление ключа из коллекции, если ключа нет,
    * то ничего не удаляется
    * сложность операции в худшем случае O(log16(n))
    * @param key ключ для удаления
    * @return возвращает эту коллекцию
    */
  override def -=(key: Int): IntMap.this.type = {
    recurRemove(underlying, key, key, null)
    this
  }

  /**
    * возвращает Option c результатом поиска по ключу
    * сложность операции O(log16(n)) в худшем случае
    * @param key ключ для поиска
    * @return Some(..) со значением если найдено и None в других случаях
    */
  override def get(key: Int): Option[T] = Option(getOrNull(key))

  /**
    * функция для рекурентного поиска значения по дереву от узла array
    * по ключу initKey.
    *
    * если значение найдено возвращает его приведенного к типу Any,
    * иначе возвращает null
    *
    * реализовано тривиально и выполняется очень быстро.
    *
    * @param array      массив внутри которого ведется поиск
    * @param currentKey текущий индекс смещенный вправо на грубину вложенности,
    *                   умноженную на количество бит на уровне
    * @param initKey    исходный ключ для проверки при нахождении узла
    * @return если найдено значение, приведенное к типу Any, иначе null
    */
  @tailrec
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

  /**
    * возвращает значение по ключу, если найдено, иначе null
    * сложность операции O(log16(n)) в худшем случае
    *
    * @param key ключ для поиска
    * @return найденное значение или null
    */
  def getOrNull(key: Int): T = recurGetOrNull(underlying, key, key).asInstanceOf[T]

  /**
    * рекурсивная реализация foreach.
    * выполнена не в виде хвостовой рекурсии.
    *
    * Это Ok, так как заранее известна максимальная глубина
    * вложенности и она не очень велика (32 / 4 = 8)
    *
    * итерации выполняются не упорядоченно по ключам
    *
    * @param f функция которая будет применена ко всем элементам коллекции
    * @tparam U какой-то там тип -- не имеет значения =)
    */
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

  /**
    * итератор для итерирования по коллекции.
    * Не рекомендуется использовать его и одновременно изменять коллекцию,
    * так как это может привести к состоянию гонки за данные
    * и разного рода труднопредсказуемым ошибкам
    *
    * итератор не очень быстрый, так как приходится проверять на null
    * значения, которых может быть достаточно много
    *
    * @return итератор по коллекции
    */
  override def iterator: Iterator[(Int, T)] = new AbstractIterator[(Int, T)] {
    private var left = _size
    private var currentArray = underlying
    private var currentIndex = 0
    private var indexStack: List[Int] = Nil
    private var arrayStack: List[Array[AnyRef]] = Nil

    private def reduceStack(): Unit = {
      currentArray = arrayStack.head
      currentIndex = indexStack.head
      arrayStack = arrayStack.tail
      indexStack = indexStack.tail
    }

    private def increaseStack(newLevel: Array[AnyRef]): Unit = {
      currentIndex += 1
      indexStack = currentIndex :: indexStack
      arrayStack = currentArray :: arrayStack
      currentArray = newLevel
      currentIndex = 0
    }

    @tailrec
    private def findNext(): (Int, T) = {


      if (currentIndex < levelSize) {
        currentArray(currentIndex) match {
          case null =>
            currentIndex += 1
            findNext()
          case obj: EntryArray =>
            increaseStack(obj.underlying)
            findNext()
          case obj: EntryValue =>
            val res = (obj.key, obj.value.asInstanceOf[T])
            currentIndex += 1
            left -= 1
            // проверка left != 0, чтобы
            // избежать перехода на нижний уровень стека
            // на пустом итераторе
            if (currentIndex == levelSize && left != 0)
              reduceStack()
            res
        }
      } else {
        reduceStack()
        findNext()
      }
    }


    override def next(): (Int, T) =
      if (hasNext) findNext()
      else Iterator.empty.next()


    override def hasNext: Boolean = left != 0
  }


  override def stringPrefix: String = "IntMap"
}
