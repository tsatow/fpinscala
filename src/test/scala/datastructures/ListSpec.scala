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

  "reverse" should "Listの並び順を反転させる" in {
    List.reverse(List(1, 2, 3)) shouldEqual List(3, 2, 1)
    List.reverse(List(1)) shouldEqual List(1)
    List.reverse(Nil) shouldEqual Nil
  }

  "flatten" should "複数のリストからなるリストを1つにリストとして連結する" in {
    List.flatten(List(List(1, 2, 3), List(4, 5, 6))) shouldEqual List(1, 2, 3, 4, 5, 6)
    List.flatten(List(List(1))) shouldEqual List(1)
    List.flatten(Nil) shouldEqual Nil
  }

  "flatMap" should "flatMapする" in {
    List.flatMap(List(1, 2, 3))(i => List(i, i)) shouldEqual List(1, 1, 2, 2, 3, 3)
  }

  "hasSubsequence" should "subsequenceかどうかを判定する" in {
    List.hasSubsequence(List(1, 2, 3, 4), List(1, 2)) shouldEqual true
    List.hasSubsequence(List(1, 2, 3, 4), List(2, 3)) shouldEqual true
    List.hasSubsequence(List(1, 2, 3, 4), List(4)) shouldEqual true
    List.hasSubsequence(List(1, 2, 3, 4), Nil) shouldEqual true
    List.hasSubsequence(List(1, 2, 3, 4), List(3, 5)) shouldEqual false
    List.hasSubsequence(List(1, 2, 3), List(1, 2, 3, 4)) shouldEqual false
  }
}
