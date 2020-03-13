package gettingstarted

object GettingStarted {

  /**
    * ex.2.1
    */
  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, prev: Int, curr: Int): Int = n match {
      case 0 => prev
      case _ => go(n - 1, curr, prev + curr)
    }
    go(n, 0, 1)
  }

  /**
    * ex.2.2
    */
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    def go(prev: A, nth: Int, isSorted: Boolean): Boolean = {
      if (!isSorted) return isSorted

      if (nth >= as.length) {
        isSorted
      } else {
        val curr = as(nth)
        go(curr, nth + 1, isSorted && ordered(prev, curr))
      }
    }

    as.length < 2 || go(as(0), 1, true)
  }

  /**
    * ex.2.3
    */
  def curry[A, B, C](f: (A, B) => C): A => B => C = a => b => f(a, b)

}
