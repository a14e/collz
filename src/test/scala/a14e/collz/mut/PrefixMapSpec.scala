package a14e.collz.mut

import org.scalatest.{Matchers, WordSpec}

import scala.collection.mutable
import scala.util.Random

/**
  * Created by User on 10.01.2017.
  */
class PrefixMapSpec extends WordSpec with Matchers {
  "Tests for IntMap " when {

    "count equals" should {
      "be 0 if start more then one of string" in {
        PrefixMap.countEquals("123", "3456", 5, 5) shouldBe 0
        PrefixMap.countEquals("123", "3456789", 5, 5) shouldBe 0
        PrefixMap.countEquals("12356789", "34", 5, 5) shouldBe 0
      }

      "have valid count" in {
        PrefixMap.countEquals("123", "3456", 0, 5) shouldBe 0

        PrefixMap.countEquals("123", "12345", 0, 5) shouldBe 3
        PrefixMap.countEquals("123", "12345", 1, 5) shouldBe 2
        PrefixMap.countEquals("123", "12345", 2, 5) shouldBe 1
        PrefixMap.countEquals("123", "12345", 3, 5) shouldBe 0
      }

      "have valid count but not more then maxCount" in {

        PrefixMap.countEquals("1234", "12345", 0, 2) shouldBe 2
        PrefixMap.countEquals("1234", "12345", 1, 2) shouldBe 2
        PrefixMap.countEquals("1234", "12345", 2, 2) shouldBe 2
        PrefixMap.countEquals("1234", "12345", 3, 2) shouldBe 1
        PrefixMap.countEquals("1234", "12345", 4, 2) shouldBe 0
      }
    }

    "valid size" should {
      def testSizeByData(data: (String, Int)*): Unit = {
        val map: Map[String, Int] = Map[String, Int]() ++ data
        val prefixMap = PrefixMap[Int](data: _*)
        map.isEmpty shouldBe prefixMap.isEmpty
        map.size shouldBe prefixMap.size
      }

      "empty" in {
        PrefixMap[Int]().size shouldBe 0
        PrefixMap[Int]().isEmpty shouldBe true
      }
      "single element" in {
        testSizeByData("a" -> 1)
      }

      "single empty element" in {
        testSizeByData("" -> 1)
      }

      "multiple elements in first level" in {
        testSizeByData("a" -> 1, "b" -> 1, "c" -> 1)

      }

      "multiple elements in first level including empty" in {
        testSizeByData("" -> 1, "b" -> 1, "c" -> 1)
      }

      "multiple elements in first level with same elements" in {
        testSizeByData("b" -> 1, "b" -> 1, "c" -> 1)
      }

      "multiple elements in second level" in {
        testSizeByData("ab" -> 1, "ac" -> 1)
      }

      "multiple elements in second level including empty" in {
        testSizeByData("a" -> 1, "ab" -> 1, "ac" -> 1)
      }

      "multiple elements in second level including same" in {
        testSizeByData("ab" -> 1, "ac" -> 1, "ab" -> 1)
      }

      "multiple elements first and second levels" in {
        testSizeByData("ab" -> 1, "ac" -> 1, "c" -> 1, "d" -> 1)

        testSizeByData("ab" -> 1, "ac" -> 1, "a" -> 1)
      }

      "multiple elements first and second levels including empty" in {
        testSizeByData("aa" -> 1, "aab" -> 1, "aac" -> 1, "c" -> 1, "d" -> 1)

        testSizeByData("aa" -> 1, "aab" -> 1, "aac" -> 1, "c" -> 1, "d" -> 1, "" -> 1)

        testSizeByData("aab" -> 1, "aac" -> 1, "c" -> 1, "d" -> 1, "" -> 1)
      }

      "multiple elements first and second levels including same elements" in {
        testSizeByData("aab" -> 1, "aac" -> 1, "c" -> 1, "d" -> 1, "d" -> 1)

        testSizeByData("ab" -> 1, "ac" -> 1, "ac" -> 1, "a" -> 1)

        testSizeByData("aab" -> 1, "aac" -> 1, "aac" -> 1, "c" -> 1, "d" -> 1, "d" -> 1)
      }

      "multiple nesting elements" in {
        testSizeByData("a" -> 1, "ab" -> 1, "abc" -> 1, "ac" -> 1)
      }

      "multiple nested elements and first level" in {
        testSizeByData("a" -> 1, "ab" -> 1, "abc" -> 1, "ac" -> 1, "f" -> 1)
      }

      "multiple ints" in {
        val strings = (1 to 100).map(x => x.toString -> x)
        testSizeByData(strings: _*)
      }

      "multiple random elements" in {
        val strings = (1 to 200).map { _ =>
          val randomString = {
            val len = Random.nextInt(4).abs
            Random.alphanumeric.take(len).mkString("")
          }
          randomString -> Random.nextInt(10)
        }
        testSizeByData(strings: _*)
      }

    }

    "valid add" should {
      def testAddElemsByData(data: (String, Int)*): Unit = {
        val prefixMap = PrefixMap[Int]()
        val validMap = new mutable.HashMap[String, Int]()

        for ((k, v) <- data) {
          prefixMap(k) = v
          validMap(k) = v
          prefixMap.size shouldBe validMap.size

          prefixMap.get(k) shouldBe Some(v)

          prefixMap.contains(k) shouldBe true
        }

        for ((k, v) <- validMap) {
          prefixMap(k) shouldBe v
        }
      }

      "single element" in {
        testAddElemsByData("a" -> 1)
      }

      "single empty element" in {
        testAddElemsByData("" -> 1)
      }

      "multiple elements in first level" in {
        testAddElemsByData("a" -> 1, "b" -> 1, "c" -> 1)

      }

      "multiple elements in first level including empty" in {
        testAddElemsByData("" -> 1, "b" -> 1, "c" -> 1)
      }

      "multiple elements in first level with same elements" in {
        testAddElemsByData("b" -> 1, "b" -> 1, "c" -> 1)
      }

      "multiple elements in second level" in {
        testAddElemsByData("ab" -> 1, "ac" -> 1)
      }

      "multiple elements in second level including empty" in {
        testAddElemsByData("a" -> 1, "ab" -> 1, "ac" -> 1)
      }

      "multiple elements in second level including same" in {
        testAddElemsByData("ab" -> 1, "ac" -> 1, "ab" -> 1)
      }

      "multiple elements first and second levels" in {
        testAddElemsByData("ab" -> 1, "ac" -> 1, "c" -> 1, "d" -> 1)

        testAddElemsByData("ab" -> 1, "ac" -> 1, "a" -> 1)
      }

      "multiple elements first and second levels including empty" in {
        testAddElemsByData("aa" -> 1, "aab" -> 1, "aac" -> 1, "c" -> 1, "d" -> 1)

        testAddElemsByData("aa" -> 1, "aab" -> 1, "aac" -> 1, "c" -> 1, "d" -> 1, "" -> 1)

        testAddElemsByData("aab" -> 1, "aac" -> 1, "c" -> 1, "d" -> 1, "" -> 1)
      }

      "multiple elements first and second levels including same elements" in {
        testAddElemsByData("aab" -> 1, "aac" -> 1, "c" -> 1, "d" -> 1, "d" -> 1)

        testAddElemsByData("ab" -> 1, "ac" -> 1, "ac" -> 1, "a" -> 1)

        testAddElemsByData("aab" -> 1, "aac" -> 1, "aac" -> 1, "c" -> 1, "d" -> 1, "d" -> 1)
      }

      "multiple nesting elements" in {
        testAddElemsByData("a" -> 1, "ab" -> 1, "abc" -> 1, "ac" -> 1)
      }

      "multiple nested elements and first level" in {
        testAddElemsByData("a" -> 1, "ab" -> 1, "abc" -> 1, "ac" -> 1, "f" -> 1)
      }

      "multiple int strings" in {
        val strings = (1 to 100).map(x => x.toString -> x)
        testAddElemsByData(strings: _*)
      }

      "multiple random elements" in {
        val strings = (1 to 200).map { _ =>
          val randomString = {
            val len = Random.nextInt(4).abs
            Random.alphanumeric.take(len).mkString("")
          }
          randomString -> Random.nextInt(10)
        }
        testAddElemsByData(strings: _*)
      }

    }


    "valid iterator" should {
      def testIterator(map: PrefixMap[Int],
                       list: List[(String, Int)]): Unit = {
        val fromMapList = map.toList.sortBy(_._1)
        val sortedList = list.toMap.toList.sortBy(_._1)
        fromMapList shouldBe sortedList
      }

      def testIteratorByData(data: (String, Int)*): Unit = {

        val list = data.toList
        val map = PrefixMap(list: _*)
        testIterator(map, list)
      }

      "empty" in {
        testIterator(PrefixMap[Int](), Nil)
      }

      "single element" in {
        testIteratorByData("a" -> 1)
      }

      "single empty element" in {
        testIteratorByData("" -> 1)
      }

      "multiple elements in first level" in {
        testIteratorByData("a" -> 1, "b" -> 1, "c" -> 1)

      }

      "multiple elements in first level including empty" in {
        testIteratorByData("" -> 1, "b" -> 1, "c" -> 1)
      }

      "multiple elements in first level with same elements" in {
        testIteratorByData("b" -> 1, "b" -> 1, "c" -> 1)
      }

      "multiple elements in second level" in {
        testIteratorByData("ab" -> 1, "ac" -> 1)
      }

      "multiple elements in second level including empty" in {
        testIteratorByData("a" -> 1, "ab" -> 1, "ac" -> 1)
      }

      "multiple elements in second level including same" in {
        testIteratorByData("ab" -> 1, "ac" -> 1, "ab" -> 1)
      }

      "multiple elements first and second levels" in {
        testIteratorByData("ab" -> 1, "ac" -> 1, "c" -> 1, "d" -> 1)

        testIteratorByData("ab" -> 1, "ac" -> 1, "a" -> 1)
      }

      "multiple elements first and second levels including empty" in {

        testIteratorByData("aa" -> 1, "aab" -> 1, "aac" -> 1, "c" -> 1, "d" -> 1)

        testIteratorByData("aa" -> 1, "aab" -> 1, "aac" -> 1, "c" -> 1, "d" -> 1, "" -> 1)

        testIteratorByData("aab" -> 1, "aac" -> 1, "c" -> 1, "d" -> 1, "" -> 1)
      }

      "multiple elements first and second levels including same elements" in {
        testIteratorByData("aab" -> 1, "aac" -> 1, "c" -> 1, "d" -> 1, "d" -> 1)

        testIteratorByData("ab" -> 1, "ac" -> 1, "ac" -> 1, "a" -> 1)

        testIteratorByData("aab" -> 1, "aac" -> 1, "aac" -> 1, "c" -> 1, "d" -> 1, "d" -> 1)
      }

      "multiple nesting elements" in {
        testIteratorByData("a" -> 1, "ab" -> 1, "abc" -> 1, "ac" -> 1)
      }

      "multiple nested elements and first level" in {
        testIteratorByData("a" -> 1, "ab" -> 1, "abc" -> 1, "ac" -> 1, "f" -> 1)
      }

      "multiple ints" in {
        val strings = (1 to 100).map(x => x.toString -> x)
        testIteratorByData(strings: _*)
      }

      "multiple random elements" in {
        val strings = (1 to 200).map { _ =>
          val randomString = {
            val len = Random.nextInt(4).abs
            Random.alphanumeric.take(len).mkString("")
          }
          randomString -> Random.nextInt(10)
        }
        testIteratorByData(strings: _*)
      }


    }

    "valid remove" should {
      def testRemoveElemsByData(data: (String, Int)*): Unit = {
        val prefixMap = PrefixMap[Int]() ++= data
        val validMap = new mutable.HashMap[String, Int]() ++= data

        for ((k, _) <- data) {
          prefixMap -= k
          validMap -= k
          prefixMap.size shouldBe validMap.size

          prefixMap.get(k) shouldBe None
          prefixMap.contains(k) shouldBe false
        }
      }

      "single element" in {
        testRemoveElemsByData("a" -> 1)
      }

      "single empty element" in {
        testRemoveElemsByData("" -> 1)
      }

      "multiple elements in first level" in {
        testRemoveElemsByData("a" -> 1, "b" -> 1, "c" -> 1)

      }

      "multiple elements in first level including empty" in {
        testRemoveElemsByData("" -> 1, "b" -> 1, "c" -> 1)
      }

      "multiple elements in first level with same elements" in {
        testRemoveElemsByData("b" -> 1, "b" -> 1, "c" -> 1)
      }

      "multiple elements in second level" in {
        testRemoveElemsByData("ab" -> 1, "ac" -> 1)
      }

      "multiple elements in second level including empty" in {
        testRemoveElemsByData("a" -> 1, "ab" -> 1, "ac" -> 1)
      }

      "multiple elements in second level including same" in {
        testRemoveElemsByData("ab" -> 1, "ac" -> 1, "ab" -> 1)
      }

      "multiple elements first and second levels" in {
        testRemoveElemsByData("ab" -> 1, "ac" -> 1, "c" -> 1, "d" -> 1)

        testRemoveElemsByData("ab" -> 1, "ac" -> 1, "a" -> 1)
      }

      "multiple elements first and second levels including empty" in {
        testRemoveElemsByData("aa" -> 1, "aab" -> 1, "aac" -> 1, "c" -> 1, "d" -> 1)

        testRemoveElemsByData("aa" -> 1, "aab" -> 1, "aac" -> 1, "c" -> 1, "d" -> 1, "" -> 1)

        testRemoveElemsByData("aab" -> 1, "aac" -> 1, "c" -> 1, "d" -> 1, "" -> 1)
      }

      "multiple elements first and second levels including same elements" in {
        testRemoveElemsByData("aab" -> 1, "aac" -> 1, "c" -> 1, "d" -> 1, "d" -> 1)

        testRemoveElemsByData("ab" -> 1, "ac" -> 1, "ac" -> 1, "a" -> 1)

        testRemoveElemsByData("aab" -> 1, "aac" -> 1, "aac" -> 1, "c" -> 1, "d" -> 1, "d" -> 1)
      }

      "multiple nesting elements" in {
        testRemoveElemsByData("a" -> 1, "ab" -> 1, "abc" -> 1, "ac" -> 1)
      }

      "multiple nested elements and first level" in {
        testRemoveElemsByData("a" -> 1, "ab" -> 1, "abc" -> 1, "ac" -> 1, "f" -> 1)
      }

      "multiple random elements" in {
        val strings = (1 to 100).map(x => x.toString -> x)
        testRemoveElemsByData(strings: _*)
      }

      "multiple random remove and add elements" in {
        val prefixMap = PrefixMap[Int]()
        val map = mutable.HashMap[String, Int]()

        for (_ <- 0 to 200) {
          val randomString = {
            val len = Random.nextInt(4).abs
            Random.alphanumeric.take(len).mkString("")
          }
          val nextRand = Random.nextInt(2)
          nextRand match {
            case 0 =>
              val randomValue = Random.nextInt(10)
              prefixMap += randomString -> randomValue
              map += randomString -> randomValue
            case 1 =>
              prefixMap -= randomString
              map -= randomString
          }
//          val prefixList = prefixMap.keysIterator.toList.sorted
//          val mapList = map.keysIterator.toList.sorted
          prefixMap.size shouldBe map.size
          for ((k, v) <- map) {
            prefixMap.contains(k) shouldBe true
            prefixMap(k) shouldBe v
          }
        }
      }

    }


    "valid find by prefix" should {
      def testFindByPrefixByData(data: (String, Int)*): Unit = {
        val dataList = data.toList
        val prefixMap = PrefixMap[Int]() ++= data

        for ((key, _) <- data) {
          val withPrefix = dataList.collect { case (k, _) if k.startsWith(key) => k }.distinct.sorted
          val found = prefixMap.findForPrefix(key).map { case (k, _) => k }.toList.sorted

          withPrefix shouldBe found

          prefixMap.hasPrefix(key) shouldBe true
        }

        for (_ <- 0 to 10) {
          val randomString = {
            val len = Random.nextInt(3).abs
            Random.alphanumeric.take(len).mkString("")
          }
          if (!dataList.exists(_._1.startsWith(randomString))) {
            prefixMap.findForPrefix(randomString) shouldBe empty
            prefixMap.hasPrefix(randomString) shouldBe false
          }
        }

      }

      "single element" in {
        testFindByPrefixByData("a" -> 1)
      }

      "single empty element" in {
        testFindByPrefixByData("" -> 1)
      }

      "multiple elements in first level" in {
        testFindByPrefixByData("a" -> 1, "b" -> 1, "c" -> 1)

      }

      "multiple elements in first level including empty" in {
        testFindByPrefixByData("" -> 1, "b" -> 1, "c" -> 1)
      }

      "multiple elements in first level with same elements" in {
        testFindByPrefixByData("b" -> 1, "b" -> 1, "c" -> 1)
      }

      "multiple elements in second level" in {
        testFindByPrefixByData("ab" -> 1, "ac" -> 1)
      }

      "multiple elements in second level including empty" in {
        testFindByPrefixByData("a" -> 1, "ab" -> 1, "ac" -> 1)
      }

      "multiple elements in second level including same" in {
        testFindByPrefixByData("ab" -> 1, "ac" -> 1, "ab" -> 1)
      }

      "multiple elements first and second levels" in {
        testFindByPrefixByData("ab" -> 1, "ac" -> 1, "c" -> 1, "d" -> 1)

        testFindByPrefixByData("ab" -> 1, "ac" -> 1, "a" -> 1)
      }

      "multiple elements first and second levels including empty" in {
        testFindByPrefixByData("aa" -> 1, "aab" -> 1, "aac" -> 1, "c" -> 1, "d" -> 1)

        testFindByPrefixByData("aa" -> 1, "aab" -> 1, "aac" -> 1, "c" -> 1, "d" -> 1, "" -> 1)

        testFindByPrefixByData("aab" -> 1, "aac" -> 1, "c" -> 1, "d" -> 1, "" -> 1)
      }

      "multiple elements first and second levels including same elements" in {
        testFindByPrefixByData("aab" -> 1, "aac" -> 1, "c" -> 1, "d" -> 1, "d" -> 1)

        testFindByPrefixByData("ab" -> 1, "ac" -> 1, "ac" -> 1, "a" -> 1)

        testFindByPrefixByData("aab" -> 1, "aac" -> 1, "aac" -> 1, "c" -> 1, "d" -> 1, "d" -> 1)
      }

      "multiple nesting elements" in {
        testFindByPrefixByData("a" -> 1, "ab" -> 1, "abc" -> 1, "ac" -> 1)
      }

      "multiple nested elements and first level" in {
        testFindByPrefixByData("a" -> 1, "ab" -> 1, "abc" -> 1, "ac" -> 1, "f" -> 1)
      }

      "multiple random elements" in {
        val strings = (1 to 100).map(x => x.toString -> x)
        testFindByPrefixByData(strings: _*)
      }

    }
  }

}
