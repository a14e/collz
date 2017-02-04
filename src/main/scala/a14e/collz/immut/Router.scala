/*
* This source code is licensed under the MIT license found in the
* LICENSE.txt file in the root directory of this source tree
*/
package a14e.collz.immut

import scala.collection.generic.{CanBuildFrom, GenericCompanion}
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag

/**
  * простой класс для роутинга и распределения по узлам
  * основная особенность, что распределение ключей по узлам не записит от истории добавления
  * а зависит только от текущих элементов в коллекции.
  * причем не может быть дублирующихся элементов.
  *
  * Добавление и удаление занимает сложность O(log(n))
  * А сам роутинг O(1) (в первый раз для данной конфигурации требует O(n), а дальше O(1)).
  * Поэтому добавления и удаления должны быть по возможности редки
  *
  * пример использования: узлы в кластере и распределение между ними
  *
  * {{{
  *   val router = Router("127.0.0.1:5555", "127.0.0.1:5556", "127.0.0.1:5557")
  *   val id = getIdForUser(...)
  *   val ipForUser = router.route(id)
  * }}}
  *
  * если узел удалился или появился мы можем спокойно добавлять и на разных компах
  * распределение будет одинаковыми будет указывать на одни и те же узлы,
  *
  * {{{
  *   val router = Router("127.0.0.1:5555", "127.0.0.1:5556", "127.0.0.1:5557")
  *   val newRouter = router - "127.0.0.1:5555"
  *   val id = getIdForUser(...)
  *   val ipForUser = newRouter.route(id) // все кроме "127.0.0.1:5555"
  * }}}
  *
  * к сожалению, это требование делает более сложнреализацию механизма, который бы оставлял
  * значения для ключений на прошлых узлах, то есть все значения для ключений при добавлении и удалении
  * перераспределяются между узлами равномерно
  *
  * кто-нибудь сконтребьютите нормальную реализацию =)
  *
  * Created by Borisenko Andrew on 28.01.2017.
  */
class Router[T: Ordering] (val underlying: collection.immutable.TreeSet[T])
  extends Iterable[T] with Traversable[T] {


  override def size: Int = underlying.size

  override def isEmpty: Boolean = underlying.isEmpty


  private lazy val table = underlying.toVector

  def +(x: T): Router[T] = {
    val newSet = underlying + x
    new Router(newSet)
  }

  def ++(xs: TraversableOnce[T]): Router[T] = {
    val newSet = underlying ++ xs
    new Router(newSet)
  }

  def -(x: T): Router[T] = {
    val newSet = underlying - x
    new Router(newSet)
  }


  def route(key: Any): T = {
    if (isEmpty)
      throw new UnsupportedOperationException("route on empty router")

    val h = Router.improveHash(key.hashCode())
    val i = h.abs % underlying.size
    table(i)
  }


  override def foreach[U](f: (T) => U): Unit = underlying.foreach(f)

  override def iterator: Iterator[T] = underlying.iterator
}

object Router {
  def improveHash(hcode: Int): Int = hcode ^ (hcode >>> 16)


  def empty[T: Ordering] = new Router[T](new collection.immutable.TreeSet[T]())

  def apply[T: Ordering](xs: T*): Router[T] = empty[T] ++ xs

  //TODO протестировать
  implicit def canBuildFrom[A : Ordering]: CanBuildFrom[TraversableOnce[_], A, Router[A]] =
    new CanBuildFrom[TraversableOnce[_], A, Router[A]] {

      override def apply(from: TraversableOnce[_]): mutable.Builder[A, Router[A]] = newBuilder[A]

      override def apply(): mutable.Builder[A, Router[A]] = newBuilder[A]
    }

  //TODO протестировать
  def newBuilder[A : Ordering]: mutable.Builder[A, Router[A]] = new mutable.Builder[A, Router[A]] {
    private var internal = empty[A]

    override def +=(elem: A): this.type = {
      internal = internal + elem
      this
    }

    override def ++=(elem: TraversableOnce[A]): this.type = {
      internal = internal ++ elem
      this
    }

    override def clear(): Unit = {
      internal = empty[A]
    }

    override def result(): Router[A] = internal
  }
}


