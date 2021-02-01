package datastructures

import datastructures._
import org.scalatest._

class TreeSpec extends FlatSpec with Matchers {

  "size" should "compute tree size" in {
    Tree.size(Leaf(0)) shouldEqual 1
    Tree.size(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))) shouldEqual 5
  }

  "maximum" should "return tree's maximum element." in {
    Tree.maximum(Leaf(0)) shouldEqual 0
    Tree.maximum(Branch(Branch(Leaf(3), Leaf(2)), Branch(Leaf(4), Leaf(3)))) shouldEqual 4
  }

  "depth" should "compute tree's depth." in {
    Tree.depth(Leaf(0)) shouldEqual 1
    Tree.depth(Branch(Branch(Leaf(3), Leaf(2)), Branch(Branch(Leaf(4), Leaf(5)), Leaf(3)))) shouldEqual 4
  }

  "map" should "map tree's element." in {
    Tree.map(Leaf(0))(i => "value: " + i) shouldEqual Leaf("value: 0")
    Tree.map(Branch(Branch(Leaf(3), Leaf(2)), Branch(Branch(Leaf(4), Leaf(5)), Leaf(3))))(i => "value: " + i) shouldEqual
      Branch(Branch(Leaf("value: " + 3), Leaf("value: " + 2)), Branch(Branch(Leaf("value: " + 4), Leaf("value: " + 5)), Leaf("value: " + 3)))
  }

  "fold" should "compute collect answer." in {
    Tree.size2(Leaf(0)) shouldEqual 1
    Tree.size2(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))) shouldEqual 5
    Tree.maximum2(Leaf(0)) shouldEqual 0
    Tree.maximum2(Branch(Branch(Leaf(3), Leaf(2)), Branch(Leaf(4), Leaf(3)))) shouldEqual 4
    Tree.depth2(Leaf(0)) shouldEqual 1
    Tree.depth2(Branch(Branch(Leaf(3), Leaf(2)), Branch(Branch(Leaf(4), Leaf(5)), Leaf(3)))) shouldEqual 4
    Tree.map2(Leaf(0))(i => "value: " + i) shouldEqual Leaf("value: 0")
    Tree.map2(Branch(Branch(Leaf(3), Leaf(2)), Branch(Branch(Leaf(4), Leaf(5)), Leaf(3))))(i => "value: " + i) shouldEqual
      Branch(Branch(Leaf("value: " + 3), Leaf("value: " + 2)), Branch(Branch(Leaf("value: " + 4), Leaf("value: " + 5)), Leaf("value: " + 3)))

  }

  "foldViaCps" should "compute collect answer." in {
    Tree.size3(Leaf(0)) shouldEqual 1
    Tree.size3(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))) shouldEqual 5
    Tree.maximum3(Leaf(0)) shouldEqual 0
    Tree.maximum3(Branch(Branch(Leaf(3), Leaf(2)), Branch(Leaf(4), Leaf(3)))) shouldEqual 4
    Tree.depth3(Leaf(0)) shouldEqual 1
    Tree.depth3(Branch(Branch(Leaf(3), Leaf(2)), Branch(Branch(Leaf(4), Leaf(5)), Leaf(3)))) shouldEqual 4
    Tree.map3(Leaf(0))(i => "value: " + i) shouldEqual Leaf("value: 0")
    Tree.map3(Branch(Branch(Leaf(3), Leaf(2)), Branch(Branch(Leaf(4), Leaf(5)), Leaf(3))))(i => "value: " + i) shouldEqual
      Branch(Branch(Leaf("value: " + 3), Leaf("value: " + 2)), Branch(Branch(Leaf("value: " + 4), Leaf("value: " + 5)), Leaf("value: " + 3)))

  }

  "foldEval" should "compute collect answer." in {
    Tree.size4(Leaf(0)) shouldEqual 1
    Tree.size4(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))) shouldEqual 5
    Tree.maximum4(Leaf(0)) shouldEqual 0
    Tree.maximum4(Branch(Branch(Leaf(3), Leaf(2)), Branch(Leaf(4), Leaf(3)))) shouldEqual 4
    Tree.depth4(Leaf(0)) shouldEqual 1
    Tree.depth4(Branch(Branch(Leaf(3), Leaf(2)), Branch(Branch(Leaf(4), Leaf(5)), Leaf(3)))) shouldEqual 4
    Tree.map4(Leaf(0))(i => "value: " + i) shouldEqual Leaf("value: 0")
    Tree.map4(Branch(Branch(Leaf(3), Leaf(2)), Branch(Branch(Leaf(4), Leaf(5)), Leaf(3))))(i => "value: " + i) shouldEqual
      Branch(Branch(Leaf("value: " + 3), Leaf("value: " + 2)), Branch(Branch(Leaf("value: " + 4), Leaf("value: " + 5)), Leaf("value: " + 3)))

  }
}