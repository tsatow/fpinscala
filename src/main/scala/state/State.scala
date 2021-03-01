package state

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {

  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  /**
   * Exercise.6.1
   */
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt

    if (i > 0) {
      (i, r)
    } else {
      (- i - 1, r)
    }
  }

  /**
   * Exercise.6.2
   */
  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)

    (i / Int.MaxValue.toDouble, r)
  }

  /**
   * Exercise.6.3
   */
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)

    ((i, d), r2)
  }

  /**
   * Exercise.6.3
   */
  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, r1) = double(rng)
    val (i, r2) = r1.nextInt

    ((d, i), r2)
  }

  /**
   * Exercise.6.3
   */
  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)

    ((d1, d2, d3), r3)
  }

  /**
   * Exercise.6.4
   */
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count < 0) {
      (Nil, rng)
    } else {
      val (i, r1) = rng.nextInt
      val (is, r2) = ints(count - 1)(r1)
      (i :: is, r2)
    }
  }

  type Rand[A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt
  def unit[A](a: A): Rand[A] = rng => (a, rng)

  /**
   * Exercise.6.5
   */
  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = { rng =>
    val (a, r) = s(rng)
    (f(a), r)
  }

  /**
   * Exercise.6.5
   */
  def doubleViaMap: Rand[Double] = {
    map(_.nextInt)(i => i / Int.MaxValue.toDouble)
  }

  /**
   * Exercise.6.6
   */
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = { rng =>
    val (a, r1) = ra(rng)
    val (b, r2) = rb(r1)

    (f(a, b), r2)
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra,rb)((_,_))

  /**
   * Exercise.6.7
   */
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = { rng =>
    fs.foldRight((List[A](), rng)) {
      case (ra, (as, r0)) =>
        val (a, r) = ra(r0)
        (a :: as, r)
    }
  }
  // 問題文の意図に従ったRandの合成を利用した回答
  def sequenceCollectAnswer[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((ra, acc) => map2(ra, acc)(_ :: _))

  /**
   * Exercise.6.7
   */
  def intsViaSequence(count: Int): Rand[List[Int]] = sequence(List.fill(count)(_.nextInt))

  // 単純に(nonNegative(rng) % n)だと、(Int.MaxValue % n)以下の数が出現する期待値が大きくなってしまう。
  // なので、歪みのもととなっている、「nonNegativeIntが生成した数が、32ビットの整数に収まり、かつnの最大の倍数よりも大きい数」が生成された場合にはリトライを行うようにする。
  def nonNegativeLessThan(n: Int): Rand[Int] = { rng =>
    val (i, rng2) = nonNegativeInt(rng)
    val mod = i % n
    if (i + (n - 1) - mod >= 0) {
      (mod, rng2)
    } else {
      nonNegativeLessThan(n)(rng)
    }
  }

  /**
   * Exercise.6.8
   */
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = { rng =>
    val (a, r) = f(rng)
    g(a)(r)
  }

  /**
   * Exercise.6.8
   */
  def nonNegativeLessThanViaFlatMap(n: Int): Rand[Int] = flatMap(nonNegativeInt) { i =>
    val mod = i % n
    if (i + (n - 1) - mod >= 0) {
      rng => (mod, rng)
    } else {
      nonNegativeLessThanViaFlatMap(n)
    }
  }

}

