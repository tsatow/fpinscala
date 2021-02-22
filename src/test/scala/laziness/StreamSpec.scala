package laziness

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
    Stream(1, 2, 3, 4).takeWhileViaFoldRight(_ <= 2).toList shouldEqual Stream(1, 2).toList
    Stream(1, 2, 3, 4).takeWhileViaFoldRight(_ <= 3).toList shouldEqual Stream(1, 2, 3).toList
    Stream(1, 2, 3, 4).takeWhileViaFoldRight(_ <= 4).toList shouldEqual Stream(1, 2, 3, 4).toList
    Stream.empty[Int].takeWhileViaFoldRight(_ => true) shouldEqual Stream.empty
  }

  "fib" should "正しいフィボナッチ数列のストリームになっていること" in {
    Stream.fibs.take(7).toList shouldEqual List(0, 1, 1, 2, 3, 5, 8)
  }

  "unfold" should "正しく余再帰できているか" in {
    Stream.fibsViaUnfold.take(7).toList shouldEqual List(0, 1, 1, 2, 3, 5, 8)
    Stream.fromViaUnfold(11).take(5).toList shouldEqual List(11, 12, 13, 14, 15)
    Stream.constantViaUnfold(11).take(100).forAll(_ == 11) shouldEqual true
    Stream.onesViaUnfold.take(100).forAll(_ == 1) shouldEqual true
  }

  "takeViaUnfold" should "先頭からn個の要素を取り出す" in {
    Stream(1, 2, 3, 4).takeViaUnfold(-1).toList shouldEqual Stream().toList
    Stream(1, 2, 3, 4).takeViaUnfold(0).toList shouldEqual Stream().toList
    Stream(1, 2, 3, 4).takeViaUnfold(3).toList shouldEqual Stream(1, 2, 3).toList
    Stream(1, 2, 3, 4).takeViaUnfold(4).toList shouldEqual Stream(1, 2, 3, 4).toList
    Stream(1, 2, 3, 4).takeViaUnfold(5).toList shouldEqual Stream(1, 2, 3, 4).toList
    Stream.empty.takeViaUnfold(1) shouldEqual Stream.empty
  }

  "takeWhileViaUnfold" should "先頭から指定された述語とマッチする要素をすべて取り出す" in {
    Stream(1, 2, 3, 4).takeWhileViaUnfold(_ < 1).toList shouldEqual Stream().toList
    Stream(1, 2, 3, 4).takeWhileViaUnfold(_ <= 1).toList shouldEqual Stream(1).toList
    Stream(1, 2, 3, 4).takeWhileViaUnfold(_ <= 2).toList shouldEqual Stream(1, 2).toList
    Stream(1, 2, 3, 4).takeWhileViaUnfold(_ <= 3).toList shouldEqual Stream(1, 2, 3).toList
    Stream(1, 2, 3, 4).takeWhileViaUnfold(_ <= 4).toList shouldEqual Stream(1, 2, 3, 4).toList
    Stream.empty[Int].takeWhileViaUnfold(_ => true) shouldEqual Stream.empty
  }

  "zipWith" should "zipWithすること" in {
    Stream(1, 2, 3, 4).zipWith(Stream(2, 3, 4))(_ + _).toList shouldEqual Stream(3, 5, 7).toList
    Stream(1, 2, 3, 4).zipWith(Stream())(_ + _).toList shouldEqual Stream().toList
    Stream[Int]().zipWith(Stream(2, 3, 4))(_ + _).toList shouldEqual Stream().toList
  }

  "zipAll" should "zipAllすること" in {
    Stream(1, 2, 3, 4).zipAll(Stream(2, 3, 4)).toList shouldEqual Stream((Some(1), Some(2)), (Some(2), Some(3)), (Some(3), Some(4)), (Some(4), None)).toList
    Stream(1, 2, 3, 4).zipAll(Stream()).toList shouldEqual Stream((Some(1), None), (Some(2), None), (Some(3), None), (Some(4), None)).toList
    Stream[Int]().zipAll(Stream(2, 3, 4)).toList shouldEqual Stream((None, Some(2)), (None, Some(3)), (None, Some(4))).toList
  }

  "tails" should "suffix全部並べる" in {
    Stream(1, 2, 3, 4).tails.map(_.toList).toList shouldEqual Stream(Stream(1, 2, 3, 4), Stream(2, 3, 4), Stream(3, 4), Stream(4)).map(_.toList).toList
    Stream[Int]().tails.map(_.toList).toList shouldEqual Stream().toList
  }

  "scanRight" should "scanRight" in {
    Stream(1, 2, 3).scanRight(0)(_ + _).toList shouldEqual Stream(6, 5, 3, 0).toList

  }
}
