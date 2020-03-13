package gettingstarted

import org.scalatest._

class GettingStartedSpec extends FlatSpec with Matchers {
  "fib" should "フィボナッチ数列を計算する。" in {
    import GettingStarted.fib

    fib(0) shouldEqual 0
    fib(1) shouldEqual 1
    fib(2) shouldEqual 1
    fib(3) shouldEqual 2
    fib(4) shouldEqual 3
    fib(5) shouldEqual 5
  }

  "isSorted" should "与えられた配列がソート済であるか判定する。" in {
    import GettingStarted.isSorted

    isSorted[Int](Array(), _ <= _) shouldEqual true
    isSorted[Int](Array(1), _ <= _) shouldEqual true
    isSorted[Int](Array(1, 2), _ <= _) shouldEqual true
    isSorted[Int](Array(1, 2, 3, 4, 5), _ <= _) shouldEqual true
    isSorted[Int](Array(1, 2, 5, 4, 5), _ <= _) shouldEqual false
    isSorted[Int](Array(1, 2, 3, 3, 5), _ <= _) shouldEqual true
  }
}
