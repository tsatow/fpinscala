package datastructures

import org.scalatest._
import datastructures._

class ListSpec extends FlatSpec with Matchers {
  "tail" should "Listの先頭の要素を削除する" in {
    List.tail(List(1, 2)) shouldEqual List(2)
    List.tail(List(1)) shouldEqual List()
    List.tail(List()) shouldEqual List()
  }

  "setHead" should "Listの先頭の要素を置き換える" in {
    List.setHead(List(1, 2), 3) shouldEqual List(3, 2)
    List.setHead(List(1), 9) shouldEqual List(9)
    List.setHead(List(), 9) shouldEqual List()
  }

  "drop" should "Listの先頭n個を削除する" in {
    List.drop(List(1, 2, 3), 1) shouldEqual List(2, 3)
    List.drop(List(1, 2, 3), 2) shouldEqual List(3)
    List.drop(List(1, 2, 3), 3) shouldEqual List()
    List.drop(List(1, 2, 3), 4) shouldEqual List()
    List.drop(List(), 9) shouldEqual List()
  }

  "dropWhile" should "Listの先頭の述語を満たす要素を削除する" in {
    List.dropWhile(List(1, 2, 3))(_ < 2) shouldEqual List(2, 3)
    List.dropWhile(List(1, 2, 3))(_ < 3) shouldEqual List(3)
    List.dropWhile(List(1, 2, 3))(_ < 4) shouldEqual List()
    List.dropWhile[Int](List())(_ < 1) shouldEqual List()
  }
}
