package fpinscala.datastructures

import org.scalatest.{Matchers, WordSpec}

class ListSpec extends WordSpec with Matchers {
  "List" should {
    "foldRightUsingFoldLeft" in {
      List.foldRightUsingFoldLeft(List(1,2,3), 0)(_+_) shouldBe 6
    }

    "flatmap" in {
      List.flatMap(List(1,2,3))(i => List(i,i)) shouldBe List(1,1,2,2,3,3)
    }

    "hasSubsequence" in {
      List.hasSubsequence(List(1,2,3,4), List(1,2)) shouldBe true
      List.hasSubsequence(List(1,2,3,4), List(3,2)) shouldBe false
      List.hasSubsequence(List(1,2,3,4), Nil) shouldBe true
      List.hasSubsequence(List(1,2,3,4), List(4)) shouldBe true
      List.hasSubsequence(List(1,2,3,4), List(1,2,3,4)) shouldBe true
    }
  }
}
