[![Build Status](https://travis-ci.org/a14e/collz.svg?branch=master)](https://travis-ci.org/a14e/collz)

[![codecov.io](https://codecov.io/gh/a14e/collz/coverage.svg?branch=master)](https://codecov.io/gh/a14e/collz?branch=master)


# Collz
Custom Scala Collections

TODO english docs in few months

## Collection Library for Scala
A collection library that may be missing in the standard Scala library.
Currently under development and will be ready in a few months.
At present, it contains the following collections:
1. Mutable:

    1) VList  
    2) BoundedQueue  
    3) IntMap  
    4) IntSet  
    5) PrefixMap  
    6) PrefixSet

2. Immutable:


    1) BoundedQueue    
    2) BoundedBucketQueue      
    3) Router  

# Usage
The library is built for versions 2.11.8 and 2.12.1.
To connect the library, add the following line to your .sbt file
```scala
libraryDependencies += "com.github.a14e" %% "collz" % "0.2.2"
```



## Description of Implementations
All mutable collections are not thread-safe.

### <a name="vlist"></a> VList
The class is a possible alternative to ListBuffer or ArrayBuffer. Unlike ListBuffer,
it provides fast access to elements by index and more efficient memory use, and
unlike ArrayBuffer, it allows adding elements to the end without rebuilding the entire
array.

Implemented as a VList with one modification: the list of subarrays is replaced with an array buffer,
which has somewhat simplified the implementation and added efficiency to iterations in both
forward and reverse directions.
The complexity of appending to the end is O(log(log(n))), which is approximately ~O(1). The speed of access by index
in the worst case (for index 0) is O(log(n)), but in most cases it's O(1), since 50% ... 75%
of the indices are contained in the last 2 subarrays.

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
This is a FIFO queue with a fixed size.
New elements are added to the end using methods like push, pushAll, :+ and so on.
Elements are retrieved and removed from the beginning by calling pull methods.
When the size is exceeded, elements from the beginning are removed. It is implemented with
quick addition to the end and extraction from the beginning without rebuilding the entire queue.
Also, it has very fast access by index and quick iterations through the queue.

Internally, it is implemented using two arrays of the same size as the queue.
The first array has pointers to the beginning and the end of the queue, and the second array has only a pointer to
the end. (here, a pointer refers to the index number). When the first array is full, the second one starts to fill.
When the size is exceeded or elements are extracted from the queue by the pull method, the pointer to the beginning of the queue
shifts, and the place of the extracted element is filled with null, to assist the garbage collector.
When the second array runs out of space, or the pointer to the beginning in the second array reaches the end,
then the second array replaces the first.

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

The immutable implementations are done in a similar way. Only instead of arrays, the Vector class is used.
```scala
import a14e.collz.immut.BoundedQueue

val queue0 = BoundedQueue[Int](2)
val queue1 =  queue0 :+ 1 // queue1 == BoundedQueue(1)
val queue2 =  queue1 :+ 2 // queue2 == BoundedQueue(1, 2)
val queue3 =  queue1 :+ 3 // queue3 == BoundedQueue(2, 3)

val (queue4, x1) = queue3.pull() // x1 == 2, queue4 == BoundedQueue(3)
val (queue5, x2) = queue4.pull() // x2 == 3, queue5 == BoundedQueue()

val queue6 = queue5 :++ List(1, 2, 3) // BoundedQueue(2, 3)
val x3 = queue6(0) // x3 == 2
val x4 = queue6(1) // x4 == 3

var sum = 0
queue6.foreach(sum += _) // sum === 5
```
In the immutable version, some elements may secretly remain in the collection after pull(),
it's worth remembering this to avoid memory leaks.

### <a name="int_map"></a> IntMap
The standard Scala library has an efficient immutable IntMap implementation. Here,
an efficient mutable implementation is presented.

Implemented as a prefix tree, each node of which has up to 16 branches, the branch number is determined
as key & F, as the depth increases, the key is shifted 4 bits to the right.
It's very close to the immutable HashMap implementation, but it has a much simpler implementation and slightly better
performance when searching for and adding elements.

When searching by element, benchmarks show no significant difference compared to
mutable AnyRefMap, HashMap, etc. But the speed of adding and removing elements
can be 4-8 times faster. The complexity of adding, removing, and key lookup operations is
O(log16(n)).

A disadvantage of this implementation due to the use of 16 arrays is higher memory consumption
and slower iterations through the collection.

Keys or null values are not allowed.

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

### <a name="int_set"></a> IntSet
An implementation of mutable.Set[_], optimized for working with Int. It's based on prefix trees.
It is a lightweight wrapper over IntMap.

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
An implementation of mutable.Map[_, _], optimized for working with strings.
Implemented as prefix trees. For quick element lookup in a node, each node contains
an IntMap, which allows the worst possible search time to depend little on the number of elements
in the collection, but only on the length of the key. The search time in the worst case is approximately equal to the time
of search in the best case in a hash table, and the difference will be greater the longer the key. For long strings
like URLs with large common parts, a significant increase in speed can be achieved
compared to a hash table. There is also the ability to efficiently find all strings starting
with a certain prefix and determine if such strings exist.

Keys or null values are not allowed.
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

### <a name="prefix_set"></a> PrefixSet
An implementation of mutable.Set[_], optimized for working with strings.
Implemented as a thin wrapper around PrefixMap.

Null values are not allowed.

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

### BoundedBucketQueue
In general, it's very similar to BoundedQueue, but it does not delete past values, instead, it stores them
in a bucket, which is filled according to the stack principle.

Since the collection consists of 2 sub-collections, it doesn't make sense for it to inherit various types of Iterable[_], etc.
Therefore, to access the iterations, you first need to call the queue or bucket methods.

```scala
import a14e.collz.immut.BoundedBucketQueue

val queue0 = BoundedBucketQueue[Int](2)
val queue1 =  queue0 :+ 1 // queue1.queue == BoundedQueue(1) и queue1.bucket == Nil
val queue2 =  queue1 :+ 2 // queue2.queue == BoundedQueue(1, 2) и queue1.bucket == Nil
val queue3 =  queue1 :+ 3 // queue3.queue == BoundedQueue(2, 3) и queue1.bucket == List(1)

val (queue4, x1) = queue3.pull() // x1 == 2, queue4 == BoundedQueue(3) и queue1.bucket == List(1)
val (queue5, x2) = queue4.pull() // x2 == 3, queue5 == BoundedQueue() и queue1.bucket == List(1)

val queue6 = queue5 :++ List(1, 2, 3) // queue == BoundedQueue(2, 3) и queue1.bucket == List(1, 1)
val x3 = queue6(0) // x3 == 2
val x4 = queue6(1) // x4 == 3

```

### Router
A simple class for routing. It's needed for the random selection of incoming elements based on a certain key.
The key can be any descendant of Any. Elements will be selected approximately randomly, but unambiguously for each key.
Regardless of the order of addition, routing will be unambiguous. However, even a single-element change can significantly alter the distribution by
keys. The algorithm is also known as Ketama.

```scala
import a14e.collz.immut.Router


val router = Router("127.0.0.1:2222", "127.0.0.1:2223", "127.0.0.1:2224")


val id1 = 1
val id2 = 2
val id3 = 2

router.route(id1) // 127.0.0.1:2223
router.route(id2) // 127.0.0.1:2224
router.route(id3) // 127.0.0.1:2222


```

## Building from source code
The project is built for Scala 2.11.8 and 2.12.1

To copy the source code:
```bash
$ git clone https://github.com/a14e/collz.git  
$ cd collz  
```
To build:
```bash
$ sbt update  
$ sbt +test  
$ sbt +package  
```
Take the .jar from the target folder.


## Roadmap
1. Make proper documentation in English (English docs)
2. Add new collections
    1) Immutable:  
        a. IntervalMap  
3. Add benchmarks to the repository


    
        
