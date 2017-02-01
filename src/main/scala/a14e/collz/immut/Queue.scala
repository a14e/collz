package a14e.collz.immut


trait Queue[T] {

  def push(value: T): this.type

  def pushValues(values: T*): this.type

  def pushAll(values: TraversableOnce[T]): this.type

  def pull(): (this.type , T)

  def pullOption(): (this.type , Option[T])

  def pullToBuff(buffer: collection.mutable.Buffer[T]): this.type

  def pullAll(count: Int): (this.type , Seq[T])

  def pullAllToBuff(count: Int, buffer: collection.mutable.Buffer[T]): this.type

  def pullWhile(cond: T => Boolean): (this.type , Seq[T])

  def pullWhileToBuff(cond: T => Boolean, buffer: collection.mutable.Buffer[T]): this.type


  def :+(elem: T): this.type

  def :++(elems: TraversableOnce[T]): this.type

//  def apply(idx: Int): T

  def isEmpty: Boolean
}

