package chapter3

import org.scalatest.{FlatSpec, Matchers}

class TreeTest extends FlatSpec with Matchers {

  "Tree size" should "return 1 for a tree which has only a leaf" in {
    Tree.size(Leaf(1)) shouldBe 1
  }

  it should "return the size of a single branch tree" in {
    Tree.size(Branch(Leaf('a'), Leaf('b'))) shouldBe 3
  }

  it should "return the size of a multi branch tree" in {
    val t = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
    Tree.size(t) shouldBe 5
  }

  "Tree maximum" should "return the value of a leaf" in {
    Tree.maximum(Leaf(9)) shouldBe 9
  }

  it should "return the maximum of a single branch tree" in {
    Tree.maximum(Branch(Leaf(3), Leaf(8))) shouldBe 8
  }

  it should "return the maximum of a multi branch tree with leaf on the right" in {
    val t = Branch(Branch(Leaf(9), Leaf(2)), Leaf(3))
    Tree.maximum(t) shouldBe 9
  }

  it should "return the maximum of a multi branch tree with leaf on the left" in {
    val t = Branch(Leaf(30), Branch(Leaf(9), Leaf(12)))
    Tree.maximum(t) shouldBe 30
  }

  it should "return the maximum of a multi branch tree" in {
    val t = Branch(Branch(Leaf(30), Leaf(48)), Branch(Leaf(9), Leaf(12)))
    Tree.maximum(t) shouldBe 48
  }
}
