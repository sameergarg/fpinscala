package fpinscala.datastructures.laziness

import fpinscala.laziness.{Empty, Stream}
import org.scalatest.{Matchers, WordSpec}

class StreamSpec extends WordSpec with Matchers {

  "Stream" should {
    "produce fibonacci numbers" in new Stream[Int] {
      Stream.fibs.take(7).toList shouldBe List(0, 1, 1, 2, 3, 5, 8)
      Stream.fibsUsingUnfold.take(7).toList shouldBe List(0, 1, 1, 2, 3, 5, 8)
    }

    "produce constants" in {
      Stream.constant(5).take(4).toList shouldBe List(5, 5, 5, 5)
      Stream.constantUsingUnfold(5).take(4).toList shouldBe List(5, 5, 5, 5)
    }

    "produce ones" in {
      Stream.ones.take(4).toList shouldBe List(1, 1, 1, 1)
      Stream.onesUsingUnfold.take(4).toList shouldBe List(1, 1, 1, 1)
    }

    "produce from" in {
      Stream.from(1).take(5).toList shouldBe List(1, 2, 3, 4, 5)
      Stream.fromUsingUnfold(1).take(5).toList shouldBe List(1, 2, 3, 4, 5)
    }

    "take n" in new Stream[Int]{
      Stream.ones.takeViaUnfold(5).toList shouldBe List(1, 1, 1, 1, 1)
    }

    "take while" in {
      List(
        Stream.from(1).takeWhile(_ < 6),
        Stream.from(1).takeWhileUsingUnfold(_ < 6)
      ).foreach(_.toList shouldBe List(1, 2, 3, 4, 5))
    }

    "zip with" in {
      Stream.constant(1).zipWith(Stream.constant(2))(_ + _).take(3).toList shouldBe List(3, 3, 3)
    }

    "zip all" in {
      Stream.constant(1).zipAll(Stream.constant(2)).take(3).toList shouldBe List((Some(1), Some(2)), (Some(1), Some(2)), (Some(1), Some(2)))
      Stream.constant(1).zipAll(Stream.constant(2).take(2)).take(3).toList shouldBe List((Some(1), Some(2)), (Some(1), Some(2)), (Some(1), None))
      Stream.constant(1).zipAll(Empty).take(3).toList shouldBe List((Some(1), None), (Some(1), None), (Some(1), None))
    }

    "starts with" in {
      Stream(1,2,3) startsWith Stream(1,2) shouldBe true
      Stream(1,2,3) startsWith2 Stream(1,2) shouldBe true
    }

    "tails" in {
      //Stream(1,2,3).tails.headOption shouldBe Stream(Stream(1,2,3), Stream(2,3), Stream(3), Stream())
      Stream(1,2,3).tails.map(_.toList).takeWhile(!_.isEmpty).toList shouldBe List(List(1,2,3), List(2, 3), List(3))
    }

    "has subsequence" in {
      Stream(1,2,3) hasSubsequence Stream(2, 3) shouldBe true
      Stream(1,2,3) hasSubsequence Stream(3, 2) shouldBe false
    }

    "scan right" in {
      Stream(1,2,3).scanRight(0)(_ + _).toList shouldBe List(6, 5, 3, 0)
    }
  }
}
