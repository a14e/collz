package a14e.collz.immut

import org.scalatest.{Matchers, WordSpec}

import scala.util.Random

/**
  * Created by User on 23.11.2016.
  */
class StringSetSpec extends WordSpec with Matchers {
  "Tests for StringSet " when {

    "main stracture" should {
      "empty elem should have 0 size" in {
        PrefixTree.empty.size shouldBe 0
      }

      "set elem should have valid size" in {
        PrefixTree("a", "b", "c").size shouldBe 3
      }

      "nested set should have valid size" in {
        val set = PrefixTree("ab", "aa", "b", "c")
        set.size shouldBe 4
        set.nodes.size shouldBe 3
      }

      "nested set with empty fields after should have valid size" in {
        val set = PrefixTree("ab", "a", "b", "c")
        set.size shouldBe 4
        set.nodes.size shouldBe 3
      }

      "nested set with empty fields before should have valid size" in {
        val set = PrefixTree("a", "ab", "b", "c")
        set.size shouldBe 4
        set.nodes.size shouldBe 3
      }

      "multiple nested fields set should have valid size" in {
        val set = PrefixTree("ab", "aa", "b", "bb", "c")
        set.size shouldBe 5
        set.nodes.size shouldBe 3
      }


      "multiple nested lebels set should have valid size" in {
        val set = PrefixTree("ab", "aa", "b", "bb", "cc", "ca", "cab", "caa")
        set.nodes.size shouldBe 3
        set.size shouldBe 8
      }


      "set elem with same elements should have valid size" in {
        val set = PrefixTree("a", "b", "c",
          "a", "b", "c")
        set.nodes.size shouldBe 3
        set.size shouldBe 3
      }

      "nested set with same elements should have valid size" in {
        val set = PrefixTree("ab", "aa", "b", "c",
          "ab", "aa", "b", "c")

        set.nodes.size shouldBe 3
        set.size shouldBe 4
      }

      "multiple nested fields set with same elements should have valid size" in {
        val set = PrefixTree("ab", "aa", "b", "bb", "c",
          "ab", "aa", "b", "bb", "c")
        set.nodes.size shouldBe 3
        set.size shouldBe 5
      }

      "multiple nested lebels set with same elements should have valid size" in {
        val set = PrefixTree("ab", "aa", "b", "bb", "cc", "ca", "cab", "caa",
          "ab", "aa", "b", "bb", "cc", "ca", "cab", "caa")
        set.nodes.size shouldBe 3
        set.size shouldBe 8
      }

      "multiple nested lebels set with same elements and empty should have valid size" in {
        val set = PrefixTree("ab", "aa", "b", "bb", "cc", "ca", "cab", "caa", "",
          "ab", "aa", "b", "bb", "cc", "ca", "cab", "caa", "")
        set.nodes.size shouldBe 4
        set.size shouldBe 9
      }

      "100 numbers in set should have valid size" in {
        val elems = 1 to 100 map (_.toString)
        val set = PrefixTree(elems: _*)
        set.nodes.size shouldBe 9
        set.size shouldBe 100
      }

      "set elem should contain all element" in {
        val set = PrefixTree("a", "b", "c")
        set.contains("a") shouldBe true
        set.contains("b") shouldBe true
        set.contains("c") shouldBe true


        set.contains("aa") shouldBe false
        set.contains("bb") shouldBe false
        set.contains("cc") shouldBe false
        set.contains("g") shouldBe false
      }

      "nested set should contain all element" in {
        val set = PrefixTree("ab", "aa", "b", "c")
        set.contains("ab") shouldBe true
        set.contains("aa") shouldBe true
        set.contains("b") shouldBe true
        set.contains("c") shouldBe true


        set.contains("abb") shouldBe false
        set.contains("aaa") shouldBe false
        set.contains("bb") shouldBe false
        set.contains("cc") shouldBe false
        set.contains("g") shouldBe false
      }

      "multiple nested fields set should contain all element" in {
        val set = PrefixTree("ab", "aa", "b", "bb", "c")
        set.contains("ab") shouldBe true
        set.contains("aa") shouldBe true
        set.contains("b") shouldBe true
        set.contains("bb") shouldBe true
        set.contains("c") shouldBe true


        set.contains("abb") shouldBe false
        set.contains("aaa") shouldBe false
        set.contains("bbb") shouldBe false
        set.contains("cc") shouldBe false
        set.contains("g") shouldBe false
        set.contains("") shouldBe false
      }

      "multiple nested fields and empty set should contain all element" in {
        val set = PrefixTree("ab", "aa", "b", "bb", "c", "")
        set.contains("ab") shouldBe true
        set.contains("aa") shouldBe true
        set.contains("b") shouldBe true
        set.contains("bb") shouldBe true
        set.contains("c") shouldBe true
        set.contains("") shouldBe true


        set.contains("abb") shouldBe false
        set.contains("aaa") shouldBe false
        set.contains("bbb") shouldBe false
        set.contains("cc") shouldBe false
        set.contains("g") shouldBe false
      }

      "100 numbers in set should contains all elements" in {
        val elems = 1 to 100 map (_.toString)
        val set = PrefixTree(elems: _*)

        elems.forall(set.contains) shouldBe true
      }


      "remove set elem should have valid size" in {
        val values = List("a", "b", "c")

        val set = PrefixTree(values: _*)

        val combinations = values.permutations

        for (list <- combinations) {
          val withRemoved = set -- list
          val newSize = set.size - list.length
          withRemoved.size shouldBe newSize
          (set -- list).size shouldBe newSize
        }

      }

      "remove nested set should have valid size" in {
        val values = List("ab", "aa", "b", "c")

        val set = PrefixTree(values: _*)

        val combinations = values.permutations

        for (list <- combinations) {
          val withRemoved = set -- list
          val newSize = set.size - list.length
          withRemoved.size shouldBe newSize
          (set -- list).size shouldBe newSize
        }
      }

      "remove nested set with empty fields after should have valid size" in {
        val values = List("ab", "a", "b", "c")
        val set = PrefixTree(values: _*)

        val combinations = values.permutations

        for (list <- combinations) {
          val withRemoved = set -- list
          val newSize = set.size - list.length
          withRemoved.size shouldBe newSize
          (set -- list).size shouldBe newSize
        }
      }

      "remove nested set with empty fields before should have valid size" in {
        val values = List("a", "ab", "b", "c")
        val set = PrefixTree(values: _*)

        val combinations = values.permutations

        for (list <- combinations) {
          val withRemoved = set -- list
          val newSize = set.size - list.length
          withRemoved.size shouldBe newSize
          (set -- list).size shouldBe newSize
        }
      }

      "remove multiple nested fields set should have valid size" in {
        val values = List("ab", "aa", "b", "bb", "c")
        val set = PrefixTree(values: _*)

        val combinations = values.permutations

        for (list <- combinations) {
          val withRemoved = set -- list
          val newSize = set.size - list.length
          withRemoved.size shouldBe newSize
          (set -- list).size shouldBe newSize
        }
      }


      "remove multiple nested lebels set should have valid size" in {
        val values = List("ab", "aa", "b", "bb", "cc", "ca", "cab", "caa")
        val set = PrefixTree(values: _*)

        val combinations = values.permutations.take(100)

        for (list <- combinations) {
          val withRemoved = set -- list
          val newSize = set.size - list.length
          withRemoved.size shouldBe newSize
          (set -- list).size shouldBe newSize
        }
      }

      "remove multiple nested lebels and empty value set should have valid size" in {
        val values = List("ab", "aa", "b", "bb", "cc", "ca", "cab", "caa", "")
        val set = PrefixTree(values: _*)

        val combinations = values.permutations.take(100)

        for (list <- combinations) {
          val withRemoved = set -- list
          val newSize = set.size - list.length
          withRemoved.size shouldBe newSize
          (set -- list).size shouldBe newSize
        }
      }


      "remove part of nested set" in {
        val values = List("ab", "aa", "b", "bb", "cc", "ca", "cab", "caa", "")
        val set = PrefixTree(values: _*)


        val removedValues = values.take(5)
        val stayedValues = values.drop(5)
        val newSet = set -- removedValues

        for (x <- stayedValues)
          newSet(x) shouldBe true

        for (x <- removedValues) {
          newSet(x) shouldBe false
        }
      }

      "remove part of nested set and stay long leave" in {
        val values = List("ab", "aa", "b", "bb", "cc", "ca", "cab", "caa", "")
        val set = PrefixTree(values: _*)


        val removedValues = List("ca", "cc", "cab", "caa")
        val removedSet = removedValues.toSet
        val stayedValues = values.filterNot(removedSet.contains)
        val newSet = set -- removedValues

        newSet.size shouldBe (values.size - removedValues.size)

        for (x <- stayedValues)
          newSet(x) shouldBe true

        for (x <- removedValues) {
          newSet(x) shouldBe false
        }
      }

      "remove part of random strings" in {
        val values = (1 to 30).map(_ => Random.nextInt().toString)
        val set = PrefixTree(values: _*)

        val toRemove = values.take(15)
        val validValues = values.drop(15)

        val newSet = set -- toRemove

        newSet.size shouldBe 15
        toRemove.forall(x => !newSet.contains(x)) shouldBe true
        validValues.forall(x => newSet.contains(x)) shouldBe true
      }

      "remove part of random and add again strings" in {
        val values = (1 to 30).map(_ => Random.nextInt().toString)
        val set = PrefixTree(values: _*)

        val toRemove = values.take(15)

        val newSet = set -- toRemove
        newSet.size shouldBe 15

        val withAddedValues = newSet ++ toRemove
        withAddedValues.size shouldBe 30

        values.forall(x => withAddedValues.contains(x)) shouldBe true
      }


      "simple iterator test" in {
        val list = PrefixTree("a", "b", "c").iterator.toList
        list.size shouldBe 3
        list should contain theSameElementsAs List("a", "b", "c")
      }


      "nested set should have valid iterator" in {
        val list = PrefixTree("ab", "aa", "b", "c").iterator.toList
        list.size shouldBe 4
        list should contain theSameElementsAs List("ab", "aa", "b", "c")
      }

      "nested set with empty fields after should have valid iterator" in {
        val list = PrefixTree("ab", "a", "b", "c").iterator.toList
        list.size shouldBe 4
        list should contain theSameElementsAs List("ab", "a", "b", "c")
      }

      "nested set with empty fields before should have valid iterator" in {
        val list = PrefixTree("a", "ab", "b", "c").iterator.toList
        list.size shouldBe 4
        list should contain theSameElementsAs List("a", "ab", "b", "c")
      }

      "multiple nested fields set should have valid iterator" in {
        val list = PrefixTree("ab", "aa", "b", "bb", "c").iterator.toList
        list.size shouldBe 5
        list should contain theSameElementsAs List("ab", "aa", "b", "bb", "c")
      }


      "multiple nested lebels set should have valid iterator" in {
        val list = PrefixTree("ab", "aa", "b", "bb", "cc", "ca", "cab", "caa").iterator.toList
        list.size shouldBe 8
        list should contain theSameElementsAs List("ab", "aa", "b", "bb", "cc", "ca", "cab", "caa")
      }


      "big size iterator" in {
        val initialList = (1 to 100).map(_.toString).toList
        val list = PrefixTree(initialList: _*).iterator.toList
        list.size shouldBe 100
        list should contain theSameElementsAs initialList
      }
    }

    "nodes lists " should {
      "build and converts to list" in {
        val stringSetList = PrefixTreeLeaves(PrefixTreeNode("a", 1, 2, null))

        stringSetList.toList shouldBe PrefixTreeNode("a", 1, 2, null) :: Nil

        val stringSetNode2 = PrefixTreeLeaves(PrefixTreeNode("a", 1, 2, null), PrefixTreeNode("ab", 1, 2, null))
        stringSetNode2.toList shouldBe PrefixTreeNode("a", 1, 2, null) :: PrefixTreeNode("ab", 1, 2, null) :: Nil


        val stringSetNode3 =
          PrefixTreeLeaves(
            PrefixTreeNode("a", 1, 2, null),
            PrefixTreeNode("ab", 1, 2, null),
            PrefixTreeNode("abc", 1, 2, null))

        stringSetNode3.toList shouldBe
          PrefixTreeNode("a", 1, 2, null) ::
            PrefixTreeNode("ab", 1, 2, null) ::
            PrefixTreeNode("abc", 1, 2, null) :: Nil


        val stringSetNode4 =
          PrefixTreeLeaves(
            PrefixTreeNode("a", 1, 2, null),
            PrefixTreeNode("ab", 1, 2, null),
            PrefixTreeNode("abc", 1, 2, null),
            PrefixTreeNode("abcd", 1, 2, null))

        stringSetNode4.toList shouldBe
          PrefixTreeNode("a", 1, 2, null) ::
            PrefixTreeNode("ab", 1, 2, null) ::
            PrefixTreeNode("abc", 1, 2, null) ::
            PrefixTreeNode("abcd", 1, 2, null) :: Nil


        val stringSetNode5 =
          PrefixTreeLeaves(
            PrefixTreeNode("a", 1, 2, null),
            PrefixTreeNode("ab", 1, 2, null),
            PrefixTreeNode("abc", 1, 2, null),
            PrefixTreeNode("abcd", 1, 2, null),
            PrefixTreeNode("abcde", 1, 2, null))

        stringSetNode5.toList shouldBe
          PrefixTreeNode("a", 1, 2, null) ::
            PrefixTreeNode("ab", 1, 2, null) ::
            PrefixTreeNode("abc", 1, 2, null) ::
            PrefixTreeNode("abcd", 1, 2, null) ::
            PrefixTreeNode("abcde", 1, 2, null) :: Nil
      }


      "prepend" in {
        val stringSetNode = PrefixTreeLeaves(PrefixTreeNode("a", 1, 2, null))
        val prepended = PrefixTreeNode("c", 1, 2, null) +: stringSetNode

        prepended.toList shouldBe List(PrefixTreeNode("c", 1, 2, null), PrefixTreeNode("a", 1, 2, null))
      }

      "foreach" in {
        val stringSetNode =
          PrefixTreeLeaves(PrefixTreeNode("a", 1, 2, null),
            PrefixTreeNode("b", 1, 2, null),
            PrefixTreeNode("c", 1, 2, null))

        var acc = ""
        stringSetNode.foreach(acc += _.value)
        acc shouldBe "abc"
      }

      "at index" in {
        val node1 = PrefixTreeNode("a", 1, 2, null)
        val node2 = PrefixTreeNode("b", 1, 2, null)
        val node3 = PrefixTreeNode("c", 1, 2, null)
        val stringSetNode = PrefixTreeLeaves(node1, node2, node3)

        stringSetNode.drop(0).elem shouldBe PrefixTreeNode("a", 1, 2, null)
        stringSetNode.drop(1).elem shouldBe PrefixTreeNode("b", 1, 2, null)
        stringSetNode.drop(2).elem shouldBe PrefixTreeNode("c", 1, 2, null)
        stringSetNode.drop(3) shouldBe null
      }


      "without element by ref to head" in {
        val node1 = PrefixTreeNode("a", 1, 2, null)
        val node2 = PrefixTreeNode("b", 1, 2, null)
        val node3 = PrefixTreeNode("c", 1, 2, null)
        val node4 = PrefixTreeNode("d", 1, 2, null)
        val stringSetNode = PrefixTreeLeaves(node1, node2, node3, node4)

        val toRemove1 = stringSetNode.drop(0)
        stringSetNode.withoutElementByRef(toRemove1).toList shouldBe List(node2, node3, node4)


        val toRemove2 = stringSetNode.drop(1)
        stringSetNode.withoutElementByRef(toRemove2).toList shouldBe List(node1, node3, node4)

        val toRemove3 = stringSetNode.drop(2)
        stringSetNode.withoutElementByRef(toRemove3).toList shouldBe List(node1, node2, node4)

        val toRemove4 = stringSetNode.drop(3)
        stringSetNode.withoutElementByRef(toRemove4).toList shouldBe List(node1, node2, node3)
      }

      "without element by ref to el" in {
        val node1 = PrefixTreeNode("a", 1, 2, null)
        val node2 = PrefixTreeNode("b", 1, 2, null)
        val node3 = PrefixTreeNode("c", 1, 2, null)
        val node4 = PrefixTreeNode("d", 1, 2, null)
        val stringSetNode = PrefixTreeLeaves(node1, node2, node3, node4)

        val toRemove1 = stringSetNode.drop(0)
        stringSetNode.withoutElementByRef(toRemove1.elem).toList shouldBe List(node2, node3, node4)


        val toRemove2 = stringSetNode.drop(1)
        stringSetNode.withoutElementByRef(toRemove2.elem).toList shouldBe List(node1, node3, node4)

        val toRemove3 = stringSetNode.drop(2)
        stringSetNode.withoutElementByRef(toRemove3.elem).toList shouldBe List(node1, node2, node4)

        val toRemove4 = stringSetNode.drop(3)
        stringSetNode.withoutElementByRef(toRemove4.elem).toList shouldBe List(node1, node2, node3)
      }


      "find in flat list" in {
        val list = PrefixTreeLeaves(PrefixTreeNode("a"), PrefixTreeNode("b"), PrefixTreeNode("c"), PrefixTreeNode("d"))

        val valids = List("a", "b", "c", "d")

        list.testContains("a") shouldBe true
        list.testContains("b") shouldBe true
        list.testContains("c") shouldBe true
        list.testContains("d") shouldBe true

        val invalids = (1 to 10).map(_ => Random.nextPrintableChar().toString).filterNot(valids.contains).toList

        invalids.forall(list.testContains) shouldBe false
      }

      "find in recursive list" in {
        val abNode = PrefixTreeLeaves(PrefixTreeNode("ab", 1, 2, null))
        val bbNode = PrefixTreeLeaves(PrefixTreeNode("bb", 1, 2, null))
        val cbNode = PrefixTreeLeaves(PrefixTreeNode("cb", 1, 2, null))
        val dbNode = PrefixTreeLeaves(PrefixTreeNode("db", 1, 2, null))

        val list = PrefixTreeLeaves(
          PrefixTreeNode("a", 0, 1, abNode),
          PrefixTreeNode("b", 0, 1, bbNode),
          PrefixTreeNode("c", 0, 1, cbNode),
          PrefixTreeNode("d", 0, 1, dbNode))

        list.testContains("ab") shouldBe true
        list.testContains("bb") shouldBe true
        list.testContains("cb") shouldBe true
        list.testContains("db") shouldBe true

        val valids = List("ab", "bb", "cb", "db")

        valids.forall(list.testContains) shouldBe true

        val invalids = (1 to 10).map(_ => Random.nextPrintableChar().toString + "b").filterNot(valids.contains).toList

        invalids.forall(list.testContains) shouldBe false
      }

    }

    "nodes " should {
      "contain whole value from start" in {
        PrefixTreeNode("a", 0, 1, null).containsStatus("a") shouldBe PrefixTreeNode.CONTAIN
      }

      "contain only start part of value" in {
        PrefixTreeNode("ab", 0, 2, null).containsStatus("a") shouldBe PrefixTreeNode.DEFINITELY_NOT_CONTAIN
      }

      "contain value to end" in {
        PrefixTreeNode("abc", 1, 3, null).containsStatus("abc") shouldBe PrefixTreeNode.CONTAIN
      }

      "contain value to end not empty in nodes" in {
        val internalNodes = PrefixTreeLeaves(PrefixTreeNode("abcde", 3, 4, null))
        PrefixTreeNode("abcde", 1, 3, internalNodes).containsStatus("abc") shouldBe PrefixTreeNode.CAN_CONTAIN
      }

      "not contain value which matches not too end and without child nodes" in {
        PrefixTreeNode("abcde", 1, 3, null).containsStatus("abcd") shouldBe PrefixTreeNode.DEFINITELY_NOT_CONTAIN
      }

      "not contain value which more then node value and without child nodes" in {
        PrefixTreeNode("abcde", 1, 4, null).containsStatus("abcde1") shouldBe PrefixTreeNode.DEFINITELY_NOT_CONTAIN
      }


      "contain value to end and empty in nodes" in {
        val internalNodes = PrefixTreeLeaves(PrefixTreeNode("abcde", 3, 4, null), PrefixTreeNode("abc", 3, 3, null))
        PrefixTreeNode("abcde", 1, 3, internalNodes).containsStatus("abc") shouldBe PrefixTreeNode.CAN_CONTAIN
      }

      "too small start" in {
        PrefixTreeNode("ab", 1, 2, null).containsStatus("a") shouldBe PrefixTreeNode.NOT_CONTAIN
      }

      "empty string from start" in {
        PrefixTreeNode("", 0, 0, null).containsStatus("") shouldBe PrefixTreeNode.CONTAIN
      }

      "empty string in end start" in {
        PrefixTreeNode("a", 1, 1, null).containsStatus("a") shouldBe PrefixTreeNode.CONTAIN
      }

      "substring at start and can contain" in {
        val internalNodes = PrefixTreeLeaves(PrefixTreeNode("abcde", 2, 4, null), PrefixTreeNode("abcde1", 2, 3, null))
        PrefixTreeNode("abcde", 1, 2, internalNodes).containsStatus("abcde") shouldBe PrefixTreeNode.CAN_CONTAIN
      }
    }


  }

}
