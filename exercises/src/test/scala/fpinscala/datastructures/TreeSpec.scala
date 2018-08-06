package fpinscala.datastructures

import org.scalatest.{Matchers, WordSpec}

class TreeSpec extends WordSpec with Matchers {

  "Tree" should {

    "find size" in {
      Tree.size(Branch(Leaf(4), Branch(Leaf(5), Leaf(9)))) shouldBe 3
      Tree.sizeUsingFold(Branch(Leaf(4), Branch(Leaf(5), Leaf(9)))) shouldBe 3
    }

    "find max" in {
          Tree.max(Branch(Leaf(4), Branch(Leaf(5), Leaf(9)))) shouldBe 9
          Tree.maxUsingFold(Branch(Leaf(4), Branch(Leaf(5), Leaf(9)))) shouldBe 9
        }

    "find depth"  in {
      Tree.depth(Branch(Leaf(4), Branch(Branch(Leaf(5), Leaf(1)), Leaf(9)))) shouldBe 3
    }

    "map" in {
      Tree.map(Branch(Leaf(4), Branch(Branch(Leaf(5), Leaf(1)), Leaf(9))))(_*2) shouldBe Branch(Leaf(8), Branch(Branch(Leaf(10), Leaf(2)), Leaf(18)))
    }
  }

}
