package a14e.collz

import org.scalatest.{Matchers, WordSpec}

import scala.language.postfixOps

/**
  * Created by User on 17.03.2016.
  */
class IntervalMapSpec extends WordSpec with Matchers {
  "Tests for IntervalMap " when {

    "test for constructing" in {

      IntervalMap[Double, Int]() shouldBe empty

      val map1 = IntervalMap((1.0, 3.0) -> 0, (5.0, 6.0) -> 1, (9.0, 10.0) -> 2)

      map1.length shouldEqual 3

      map1 should contain theSameElementsAs List(Interval(1.0, 3.0, 0), Interval(5.0, 6.0, 1), Interval(9.0, 10.0, 2))

      map1 should contain theSameElementsAs Array(Interval(1.0, 3.0, 0), Interval(5.0, 6.0, 1), Interval(9.0, 10.0, 2))

      val map2 = IntervalMap(Interval(1.0, 3.0, 0), Interval(5.0, 6.0, 1), Interval(9.0, 10.0, 2))

      map2 should contain theSameElementsAs map1

      map1(0) shouldBe None
      map1(0.5) shouldBe None
      map1(1.5) shouldBe Some(0)
      map1(3.0) shouldBe Some(0)

      map1(4.0) shouldBe None
      map1(5.5) shouldBe Some(1)
      map1(6.0) shouldBe Some(1)

      map1(6.5) shouldBe None
      map1(9.5) shouldBe Some(2)
      map1(10.0) shouldBe Some(2)


      val fromEmptyBuilder = (IntervalMap.newBuilder[Double, Int] ++=
        IntervalMap((1.0, 3.0) -> 0, (3.0, 6.0) -> 1, (6.0, 10.0) -> 2)).result()

      fromEmptyBuilder should contain theSameElementsAs IntervalMap((1.0, 3.0) -> 0, (3.0, 6.0) -> 1, (6.0, 10.0) -> 2)

      (IntervalMap.canBuildFrom[Double, Int].apply() ++=
        IntervalMap((1.0, 3.0) -> 0, (3.0, 6.0) -> 1, (6.0, 10.0) -> 2)).result() should contain theSameElementsAs
        IntervalMap((1.0, 3.0) -> 0, (3.0, 6.0) -> 1, (6.0, 10.0) -> 2)


      val builder = IntervalMap.newBuilder[Double, Int] ++= IntervalMap((1.0, 3.0) -> 0, (3.0, 6.0) -> 1, (6.0, 10.0) -> 2)
      builder.clear()
      builder += Interval(1.0, 3.0, 0)
      builder.result() should contain theSameElementsAs IntervalMap((1.0, 3.0) -> 0)
      builder += Interval(2.0, 3.0, 0)
      builder.result() should contain theSameElementsAs IntervalMap((1.0, 2.0) -> 0, (2.0, 3.0) -> 0)
      builder.clear()
      builder.result() should contain theSameElementsAs IntervalMap[Double, Int]()

      /** wrong type */
      map1 should not equal false

      Interval((1.0, 2.0) -> 1).hasValue(0.0) shouldBe false
      Interval((1.0, 2.0) -> 1).hasValue(1.0) shouldBe true
      Interval((1.0, 2.0) -> 1).hasValue(1.5) shouldBe true
      Interval((1.0, 2.0) -> 1).hasValue(2.0) shouldBe true
      Interval((1.0, 2.0) -> 1).hasValue(3.0) shouldBe false

      /** wrong type */
      Interval((1.0, 2.0) -> 1) should not be false
    }

    "tests for intersect" in {

      val map1 = IntervalMap((1.0, 3.0) -> 0, (3.0, 6.0) -> 1, (6.0, 10.0) -> 2)

      map1.length shouldEqual 3

      map1 should contain theSameElementsAs List(Interval(1.0, 3.0, 0), Interval(3.0, 6.0, 1), Interval(6.0, 10.0, 2))


      val map2 = map1 + Interval(2.0, 4.0, 7)

      map2 should contain theSameElementsAs
        List(Interval(1.0, 2.0, 0), Interval(2.0, 4.0, 7), Interval(4.0, 6.0, 1), Interval(6.0, 10.0, 2))


      val map3 = map1 + Interval(3.0, 7.0, 7)

      map3 should contain theSameElementsAs
        List(Interval(1.0, 3.0, 0), Interval(3.0, 7.0, 7), Interval(7.0, 10.0, 2))


      val map4 = map1 + Interval(0.0, 1.5, 7)

      map4 should contain theSameElementsAs
        List(Interval(0.0, 1.5, 7), Interval(1.5, 3.0, 0), Interval(3.0, 6.0, 1), Interval(6.0, 10.0, 2))



      val map5 = map1 + Interval(0.0, 0.5, 7)

      map5 should contain theSameElementsAs
        List(Interval(0.0, 0.5, 7), Interval(1.0, 3.0, 0), Interval(3.0, 6.0, 1), Interval(6.0, 10.0, 2))


      val map6 = map1 + Interval(11.0, 12.0, 7)

      map6 should contain theSameElementsAs
        List(Interval(1.0, 3.0, 0), Interval(3.0, 6.0, 1), Interval(6.0, 10.0, 2), Interval(11.0, 12.0, 7))


      val map7 = map1 + Interval(1.0, 7.0, 7)

      map7 should contain theSameElementsAs
        List(Interval(1.0, 7.0, 7), Interval(7.0, 10.0, 2))


      val map8 = map1 + Interval(9.0, 11.0, 7)

      map8 should contain theSameElementsAs
        List(Interval(1.0, 3.0, 0), Interval(3.0, 6.0, 1), Interval(6.0, 9.0, 2), Interval(9.0, 11.0, 7))


    }

    "tests for find and update" in {

      val intervalMap = IntervalMap((1.0, 2.0) -> 1, (3.0, 4.0) -> 2, (4.0, 5.0) -> 3)

      intervalMap(0.0) shouldBe None
      intervalMap(1.5) shouldBe Some(1)
      intervalMap(2.0) shouldBe Some(1)

      intervalMap(2.5) shouldBe None
      intervalMap(3.5) shouldBe Some(2)
      intervalMap(4.0) shouldBe Some(2)

      intervalMap(4.5) shouldBe Some(3)
      intervalMap(5.0) shouldBe Some(3)
      intervalMap(5.1) shouldBe None


      intervalMap.contains(0.0) shouldBe false
      intervalMap.contains(1.5) shouldBe true
      intervalMap.contains(2.0) shouldBe true

      intervalMap.contains(2.5) shouldBe false
      intervalMap.contains(3.5) shouldBe true
      intervalMap.contains(4.0) shouldBe true

      intervalMap.contains(4.5) shouldBe true
      intervalMap.contains(5.0) shouldBe true
      intervalMap.contains(5.1) shouldBe false


      intervalMap.applyIfExist(0.0)(_ + 1)
      intervalMap should contain theSameElementsAs IntervalMap((1.0, 2.0) -> 1, (3.0, 4.0) -> 2, (4.0, 5.0) -> 3)

      intervalMap.applyIfExist(1.5)(_ + 1)
      intervalMap should contain theSameElementsAs IntervalMap((1.0, 2.0) -> 2, (3.0, 4.0) -> 2, (4.0, 5.0) -> 3)

      (intervalMap(0) = 10) shouldBe false
      (intervalMap(1.5) = 10) shouldBe true
      intervalMap should contain theSameElementsAs IntervalMap((1.0, 2.0) -> 10, (3.0, 4.0) -> 2, (4.0, 5.0) -> 3)
    }

    "test for iterators " in {
      val intervalMap = IntervalMap((1.0, 2.0) -> 1, (3.0, 4.0) -> 2, (4.0, 5.0) -> 3)
      intervalMap.iterator.toSeq should contain theSameElementsAs
        List(Interval(1.0, 2.0, 1), Interval(3.0, 4.0, 2), Interval(4.0, 5.0, 3))

      intervalMap.map(_.value + 1).toSeq should contain theSameElementsAs List(2, 3, 4)

      intervalMap.map(_.value).sum shouldEqual 6

      intervalMap.map(_.value).product shouldEqual 6

      intervalMap.map(_.value).map(_.toString).reduce(_ + _) shouldEqual "123"

      val intervals = List(Interval(1.0, 2.0, 1), Interval(3.0, 4.0, 2), Interval(4.0, 5.0, 3))

      for (Interval(left, right, value) <- intervalMap)
        intervals should contain(Interval(left, right, value))


      intervalMap.map(_.map(_ + 1)) should contain theSameElementsAs
        IntervalMap((1.0, 2.0) -> 2, (3.0, 4.0) -> 3, (4.0, 5.0) -> 4)

      //      import IntervalMap._
      val maped: IntervalMap[Double, Int] = intervalMap.map(_.map(_ + 1))
      maped.toString shouldEqual
        "IntervalMap(Interval(left: 1.0, right: 2.0, value: 2), " +
          "Interval(left: 3.0, right: 4.0, value: 3), " +
          "Interval(left: 4.0, right: 5.0, value: 4))"

    }


    "testing for filling" in {
      IntervalMap.fill[Int, Double](Nil)(1) should contain theSameElementsAs IntervalMap[Int, Double]()
      IntervalMap.fill[Int, Double](List(1))(1) should contain theSameElementsAs IntervalMap[Int, Double]()
      IntervalMap.fillByData[Int, Double](List(1))(Nil) should contain theSameElementsAs IntervalMap[Int, Double]()

      IntervalMap.fill(List(1, 2, 3, 4))(1) should contain theSameElementsAs
        IntervalMap((1, 2) -> 1, (2, 3) -> 1, (3, 4) -> 1)

      IntervalMap.fillByData(List(1, 2, 3, 4))(List(1, 2, 3)) should contain theSameElementsAs
        IntervalMap((1, 2) -> 1, (2, 3) -> 2, (3, 4) -> 3)

      IntervalMap.fillIntervals(1, 2, 3, 4, 5)(1) should contain theSameElementsAs
        IntervalMap((1, 2) -> 1, (2, 3) -> 1, (3, 4) -> 1, (4, 5) -> 1)

      IntervalMap.fillIntervalsByData(1, 2, 3, 4, 5)(List(1, 2, 3, 4)) should contain theSameElementsAs
        IntervalMap((1, 2) -> 1, (2, 3) -> 2, (3, 4) -> 3, (4, 5) -> 4)

      IntervalMap
        .splitByIntervals((0 to 3).map(_.toDouble))(
          (0 to 12).map(_.toDouble / 4.0)) should contain theSameElementsAs
        IntervalMap(
          (0.0, 1.0) -> List(4.0 / 4.0, 3.0 / 4.0, 2.0 / 4.0, 1.0 / 4.0, 0.0 / 4.0),
          (1.0, 2.0) -> List(8.0 / 4.0, 7.0 / 4.0, 6.0 / 4.0, 5.0 / 4.0),
          (2.0, 3.0) -> List(12.0 / 4.0, 11.0 / 4.0, 10.0 / 4.0, 9.0 / 4.0))

      IntervalMap
        .countByIntervals((0 to 3).map(_.toDouble))(
          (0 to 12).map(_.toDouble / 4.0)
        ) should contain theSameElementsAs
        IntervalMap(
          (0.0, 1.0) -> 5,
          (1.0, 2.0) -> 4,
          (2.0, 3.0) -> 4)
    }

    "testing for errors" in {

      an[NoSuchElementException] should be thrownBy IntervalMap[Double, Double]().iterator.next()


      an[IllegalArgumentException] should be thrownBy IntervalMap.fillByData(List(1, 2, 3, 4))(List(1, 2))

      an[IllegalArgumentException] should be thrownBy IntervalMap.fillIntervalsByData(1, 2, 3, 4, 5)(List(1, 2, 3))


      an[IllegalArgumentException] should be thrownBy IntervalMap.fillIntervalsByData(1, 1, 3, 4, 5)(List(1, 2, 3, 4))

      an[IllegalArgumentException] should be thrownBy IntervalMap.fillIntervalsByData(1, 2, -3, 4, 5)(List(1, 2, 3, 4))


    }

  }
}