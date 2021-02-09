package laziness

import laziness._
import org.scalatest._

class StreamSpec extends FlatSpec with Matchers {
  "toList" should "return list." in {
    Stream(1, 2, 3, 4).toList shouldEqual List(1, 2, 3, 4)
    Stream.empty[Int].toList shouldEqual List()
  }

  "take" should "先頭からn個の要素を取り出す" in {
    Stream(1, 2, 3, 4).take(-1).toList shouldEqual Stream().toList
    Stream(1, 2, 3, 4).take(0).toList shouldEqual Stream().toList
    Stream(1, 2, 3, 4).take(3).toList shouldEqual Stream(1, 2, 3).toList
    Stream(1, 2, 3, 4).take(4).toList shouldEqual Stream(1, 2, 3, 4).toList
    Stream(1, 2, 3, 4).take(5).toList shouldEqual Stream(1, 2, 3, 4).toList
    Stream.empty.take(1) shouldEqual Stream.empty
  }

  "drop" should "先頭からn個の要素をスキップする" in {
    Stream(1, 2, 3, 4).drop(3).toList shouldEqual Stream(4).toList
    Stream(1, 2, 3, 4).drop(4).toList shouldEqual Stream().toList
    Stream(1, 2, 3, 4).drop(5).toList shouldEqual Stream().toList
    Stream.empty.drop(1) shouldEqual Stream.empty
  }

  "takeWhile" should "先頭から指定された述語とマッチする要素をすべて取り出す" in {
    Stream(1, 2, 3, 4).takeWhile(_ < 1).toList shouldEqual Stream().toList
    Stream(1, 2, 3, 4).takeWhile(_ <= 1).toList shouldEqual Stream(1).toList
    Stream(1, 2, 3, 4).takeWhile(_ <= 3).toList shouldEqual Stream(1, 2, 3).toList
    Stream(1, 2, 3, 4).takeWhile(_ <= 4).toList shouldEqual Stream(1, 2, 3, 4).toList
    Stream.empty[Int].takeWhile(_ => true) shouldEqual Stream.empty
  }

  "takeWhileViaFoldRight" should "先頭から指定された述語とマッチする要素をすべて取り出す" in {
    Stream(1, 2, 3, 4).takeWhileViaFoldRight(_ < 1).toList shouldEqual Stream().toList
    Stream(1, 2, 3, 4).takeWhileViaFoldRight(_ <= 1).toList shouldEqual Stream(1).toList
    Stream(1, 2, 3, 4).takeWhileViaFoldRight({ n =>
      println(n)
      n <= 2
    }).toList shouldEqual Stream(1, 2).toList
    Stream(1, 2, 3, 4).takeWhileViaFoldRight(_ <= 3).toList shouldEqual Stream(1, 2, 3).toList
    Stream(1, 2, 3, 4).takeWhileViaFoldRight(_ <= 4).toList shouldEqual Stream(1, 2, 3, 4).toList
    Stream.empty[Int].takeWhileViaFoldRight(_ => true) shouldEqual Stream.empty
  }
}
