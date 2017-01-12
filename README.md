[![Build Status](https://travis-ci.org/a14e/collz.svg?branch=master)](https://travis-ci.org/a14e/collz)

[![codecov.io](https://codecov.io/gh/a14e/collz/coverage.svg?branch=master)](https://codecov.io/gh/a14e/collz?branch=master)


# Collz
Custom Scala Collections

TODO english docs in few months

## Библиотека коллекций для Scala
Библиотека коллекций, которых может недоставать в стандартной библиотеке Scala. 
Пока находится в разработке и будет готова в течении нескольких месяцев. 
В данный момент содержит следующие коллекции:  
1. Мутабельные:  
  
    1) VList  
    2) BoundedQueue  
    3) IntMap  
    4) IntSet  
    5) PrefixMap  
    6) PrefixSet  
      
2. Иммутабельные:  
  
    1) BoundedQueue  

# Использование
Библиотека собрана для версий 2.11.8 и 2.12.1
Для подключения библиотки добавьте себе в .sbt файл строку
```scala
libraryDependencies += "com.github.a14e" %% "collz" % "0.1"
```


    
## Описание реализаций
Все мутабельные коллекции не являются потокобезопасными

### <a name="vlist"></a> VList
Класс как возможная альтернатива ListBuffer или ArrayBuffer. В отличие от ListBuffer 
имеет быстрый доступ к элементам по индексу и более эффективное использование памяти, а
в отличие от ArrayBuffer позволяет добавлять элементы в конец без перестроения всего 
массива.

Реализован как VList c одним изменением: список из подмассивов заменен на массив с буффером,
что позволило несколько упростить реализацию и добавить эффективно реализовать итерации в прямом
и обратном направлениях.
Сложность добавления в конец O(log(log(n))), что примерно ~O(1). Скорость обращения по индексу
в худшем случае (для индекса 0) - O(log(n)), но в большинстве случаев - O(1), так как 50% ... 75%
индексов содержатся в 2х последних подмассивах. 

```scala
import a14e.collz.mut.VList

val list = VList[Int](1, 2, 3)
list += 1 // list == VList(1, 2, 3, 1)
list ++= List(2, 3) // list == VList(1, 2, 3, 1, 2, 3)
val x = list(1) // x == 2

var sum = 0
list.foreach(sum += _) // sum == 12
```

### <a name="fixed_queue"></a> BoundedQueue
Представляет собой очередь типа FIFO с фиксированным размером. 
Новые элементы добавляются в конец методами push, pushAll, :+ и тд.
Элементы из очереди извлекаются и удаляются из начала вызовами методов pull. 
При превышении размера удаляются элементы из начала. Реализована с 
быстрым добавлением в конец и извлечением из начала без перестроения всей очереди. 
Также реализовано с очень быстрым доступом по индексу и быстрыми итерациями по очереди.  

Внутри реализовано с помощью двух массивов с размером, равным размеру очереди. 
В первом массиве есть указатели на начало и конец очереди, а во втором массиве только указатель на
конец. (тут указателем называется номер индекса). При перепонии первого моссива начинает заполнятся второй.
При превышении размера или извлечении элементов из учереди методом pull смещается указатель на начало очереди
и место извдеченного элемента заполняется null, чтобы помочь сборщику мусора.
Когда место во втором массиве заканчивается или указатель на начало во втором массиве доходит до конца,
тогда второй массив заменяет собой первый. 

```scala
import a14e.collz.mut.BoundedQueue

val queue = BoundedQueue[Int](2)
queue += 1 // queue == BoundedQueue(1)
queue += 2 // queue == BoundedQueue(1, 2)
queue += 3 // queue == BoundedQueue(2, 3)

val x1 = queue.pull() // x1 == 2, queue == BoundedQueue(3)
val x2 = queue.pull() // x2 == 3, queue == BoundedQueue()

queue ++= List(1, 2, 3) // queue == BoundedQueue(2, 3)
val x3 = queue(0) // x3 == 2
val x4 = queue(1) // x4 == 3

var sum = 0
queue.foreach(sum += _) // sum === 5
```

иммутабельные реализованы схожим образом. Только вместо массивов используется класс Vector
```scala
import a14e.collz.immut.BoundedQueue

val queue0 = BoundedQueue[Int](2)
val queue1 =  queue0 :+ 1 // queue1 == BoundedQueue(1)
val queue2 =  queue1 :+ 2 // queue2 == BoundedQueue(1, 2)
val queue3 =  queue1 :+ 3 // queue3 == BoundedQueue(2, 3)

val (queue4, x1) = queue3.pull() // x1 == 2, queue4 == BoundedQueue(3)
val (queue5, x2) = queue4.pull() // x2 == 3, queue == BoundedQueue()

val queue6 = queue5 :++ List(1, 2, 3) // BoundedQueue(2, 3)
val x3 = queue6(0) // x3 == 2
val x4 = queue6(1) // x4 == 3

var sum = 0
queue6.foreach(sum += _) // sum === 5
```
В иммутабельной версии часть элементов после pull() может скрыто оставаться в коллекции,
стоит помнить об этом во избежание утечек памяти.

### <a name="int_map"></a> IntMap
В стандартной библиотеке Scala есть эффективная неизменяемая реализация IntMap. Тут же
преставлена эффективная изменяемая реализация.

Выполнена как префиксное дерево, в каждом узле которого до 16 ветвей, номер ветви определяется
как key & F, при увеличении глубины ключ сдвигается на 4 бита вправо. 
Очень близка к иммутабельной реазации HashMap, но имеет сильно более простую реализацию и несколько лучшую
производительность при поиске и добавлении элементов.
 
При поиске по элементу бенчмарки большой разницы по сравнению из 
мутабельными AnyRefMap, HashMap и тд. Но скорость добавления и удаления элементов 
может быть в 4-8 раз быстрее. Сложность операций добавления, удаления, поиска по ключу имеет  вид 
O(log16(n))

Недостатком данной реализации из-за использование массивов по 16 является более высокий расход памяти
и более медленные итерации по коллеции.

Ключи или значения null недопустимы

```scala
import a14e.collz.mut.IntMap
val map = IntMap[Int](1 -> 3, 2 -> 4) // IntMap(1 -> 1, 2 -> 2)

val x1 = map.get(1) // x1 == Some(3)
val x2 = map.get(2) // x2 == Some(4)

val x3 = map.get(0) // x3 == None
val x4 = map.get(3) // x4 == None


map(1) = 5 // map == IntMap(1 -> 5, 2 -> 2)

map(6) = 7 // map == IntMap(1 -> 5, 2 -> 2, 6 -> 7)

val x5 = map.contains(2) // x5 == true
map -= 2 // map == IntMap(1 -> 5, 6 -> 7)
val x6 = map.contains(2) // x6 == false

var sum = 0
map.foreach{ case (key, value) => sum += value } // sum == 12

map.clear() // map = IntMap()
```

### <a name="int_map"></a> IntSet
Реализация mutable.Set[_], оптимизированная для работы с Int. Сделана на базе префиксных деревьев. 
Представляет собой легкую обертку поверх IntMap. 

```scala
import a14e.collz.mut.IntSet
val set = IntSet(1, 2, 3, 4) // IntSet(1, 2, 3, 4)

val res1 = set.contains(1) //res1 == true
val res2 = set.contains(2) //res2 == true
val res3 = set.contains(6) //res3 == false

set += 6 // set == IntSet(1, 2, 3, 4, 6)
val res4 = set.contains(6) //res4 == true

set -= 1 // set == IntSet(2, 3, 4, 6)
val res5 = set.contains(1) //res5 == false

var sum = 0
set.foreach(sum += _) // sum == 15

```


### <a name="prefix_map"></a> PrefixMap
Реализация mutable.Map[_, _], оптимизированная для работы со строками. 
Сделана в виде префиксных деревьев. Для быстрого поиска элемента в узле, каждый узел содержит
IntMap, что позволяет получить худшее возможное время поиска мало зависящим от колличества элементов
в коллекции, а зависящим только от длинны ключа. Время поиска в худшем случае примерно равно времени
поиска в лучшем случае в хэш таблице и разница будет тем больше, чем длиннее ключ. Для длинных строк 
типа URL с большими общими частями можно получить значительный прирост в скорости 
по сравнению с хэш таблицей. Также есть возможность эффективно находить все строки, начинающиеся
с некоторого префикса и определять существуют ли такие строки.

Ключи или значения null недопустимы

```scala
import a14e.collz.mut.PrefixMap
val map = PrefixMap("string" -> 1, "stringWithSuffix" -> 2, "string2" -> 3, "anotherString" -> 4)
val x1 = map.get("string") // s1 == Some(1)
val x2 = map.get("string2") // s1 == Some(3)
val x3 = map.get("string3") // s1 == None

val x4 = map.hasPrefix("stri") // x4 == true
val x5 = map.hasPrefix("unknown") // x5 == false


val x6 = map.findForPrefix("stri").toList 
// x6 == List("string" -> 1, "stringWithSuffix" -> 2, "string2" -> 3)

map -= "string" 
// map == PrefixMap("stringWithSuffix" -> 2, "string2" -> 3, "anotherString" -> 4)
map("string3") = 5 
// map == PrefixMap("stringWithSuffix" -> 2, "string2" -> 3, "anotherString" -> 4, "string3" -> 5)
```

### <a name="prefix_map"></a> PrefixSet
Реализация mutable.Set[_], оптимизированная для работы со строками. 
Реализована в виде тонкой обертки вокруг PrefixMap.

значения null недопустимы

```scala
import a14e.collz.mut.PrefixSet
val set = PrefixSet("string", "stringWithSuffix", "string2", "anotherString" )
val x1 = set.contains("string") // s1 == true
val x2 = set.contains("string2") // s1 == true
val x3 = set.contains("string3") // s1 == false

val x4 = set.hasPrefix("stri") // x4 == true
val x5 = set.hasPrefix("unknown") // x5 == false


val x6 = set.findForPrefix("stri").toList 
// x6 == List("string", "stringWithSuffix", "string2")

set -= "string" 
// set == PrefixSet("stringWithSuffix", "string2", "anotherString")
set += "string3"
// set == PrefixSet("stringWithSuffix", "string2", "anotherString", "string3")
```

## Сборка из исходных кодов
Проект собирается для scala 2.11.8 и 2.12.1
  
скопировать исходники:  
$ git clone https://github.com/a14e/collz.git  
$ cd collz  
собрать:  
$ sbt update  
$ sbt +test  
$ sbt +package  
взять .jar из папки traget


## Roadmap
1. Сделать нормальную документацию на англ языке (English docs)
2. Добавить новые коллекции 
    1) иммутабельные:  
        a. PrefixSet   
        b. PrefixMap  
        c. NonDeleteBoundedQueue
    2) мутабельные:
        c. NonDeleteBoundedQueue  
        d. IntervalMap
3. Добавить бенчмарки в репозиторий


    
        
