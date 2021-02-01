package datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  /**
   * exercise.3.25
   */
  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(left, right) => size(left) + 1 + size(right)
  }

  /**
   * exercise.3.26
   */
  def maximum(tree: Tree[Int]): Int = {
    def go(max: Int, tree: Tree[Int]): Int = tree match {
      case Leaf(v)             => Math.max(v, max)
      case Branch(left, right) => Math.max(max, Math.max(maximum(left), maximum(right)))
    }
    go(Int.MinValue, tree)
  }

  /**
   * exercise.3.27
   */
  def depth[A](tree: Tree[A]): Int = {
    def go[A](currDepth: Int, tree: Tree[A]): Int = tree match {
      case Leaf(v)             => currDepth + 1
      case Branch(left, right) => Math.max(go(currDepth + 1, left), go(currDepth + 1, right))
    }
    go(0, tree)
  }

  /**
   * exercise.3.28
   */
  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(v)      => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  /**
   * exercise.3.29
   * 頑張って末尾再帰最適化が掛かるようにしたが、あんまり使い途はなさそう。
   */
  def fold[A, B](tree: Tree[A])(f: A => B)(g: (B,B) => B): B = {
    // 畳み込み関数(foldSimple↓↓を見て)の各ステップでの引数・ローカル変数の状態を表現する
    sealed trait FunctionLocalState[A, B]
    case class Unprocessed(tree: Tree[A]) extends FunctionLocalState[A, B]
    case class TreeDisassembled(tree: Tree[A], lt: Tree[A], rt: Tree[A]) extends FunctionLocalState[A, B]
    case class LeftProcessed(tree: Tree[A], lr: B, rt: Tree[A]) extends FunctionLocalState[A, B]
    case class RightProcessed(tree: Tree[A], lr: B, rr: B) extends FunctionLocalState[A, B]

    @annotation.tailrec // 末尾再帰最適化されない場合エラーになるおまじない
    def process(localState: FunctionLocalState[A, B], callstack: List[FunctionLocalState[A, B]]): B = localState match {
      case Unprocessed(tree) => tree match {
        case Leaf(v) => callstack match {
          case Nil        => f(v) // LeafだけのTreeなので即終了
          case Cons(_, _) => returnProcess(f(v), callstack) match {
            case Nil          => f(v)
            case Cons(ls, cs) => process(ls, cs)
          }
        }
        case Branch(l, r) => process(TreeDisassembled(tree, l, r), callstack)
      }
      case TreeDisassembled(_, lt, _) => process(Unprocessed(lt), Cons(localState, callstack))
      case LeftProcessed(_, _, rt)    => process(Unprocessed(rt), Cons(localState, callstack))
      case RightProcessed(_, lv, rv)  => returnProcess(g(lv, rv), callstack) match {
        case Nil             => g(lv, rv)
        case Cons(curr, rem) => process(curr, rem)
      }
    }

    // 戻るときの処理
    // 戻り値を上位のstackにセットする
    def returnProcess(retVal: B, callstack: List[FunctionLocalState[A, B]]): List[FunctionLocalState[A, B]] = callstack match {
      case Cons(TreeDisassembled(arg, _, rt), t) => Cons(LeftProcessed(arg, retVal, rt), t) // lrへの変数束縛
      case Cons(LeftProcessed(arg, l, _), t)     => Cons(RightProcessed(arg, l, retVal), t) // rrへの変数束縛
      case _                                     => callstack // ちゃんと戻り処理を呼び出せてればここにはこない
    }

    process(Unprocessed(tree), Nil)
  }
  // exercise.3.29の普通の実装
  def foldSimple[A, B](tree: Tree[A])(f: A => B)(g: (B,B) => B): B = tree match {
    case Leaf(v)      => f(v)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }
  def size2[A](tree: Tree[A]): Int = {
    fold(tree)(_ => 1)(_ + _ + 1)
  }
  def maximum2(tree: Tree[Int]): Int = {
    fold(tree)(identity)(_ max _)
  }
  def depth2[A](tree: Tree[A]): Int = {
    fold(tree)(_ => 1)((l, r) => (l max r) + 1)
  }
  def map2[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
    fold(tree)(v => Leaf(f(v)): Tree[B])(Branch(_, _))
  }

  def foldViaCps[A, B](tree: Tree[A])(f: A => B)(g: (B,B) => B): B = {
    def foldCps(tree: Tree[A])(cb: B => B): B = tree match {
      case Leaf(v)        => cb(f(v))
      case Branch(lt, rt) =>
        foldCps(lt) { lr =>
          foldCps(rt) { rr =>
            cb(g(lr, rr))
          }
        }
    }
    foldCps(tree)(identity)
  }
  def size3[A](tree: Tree[A]): Int = {
    foldViaCps(tree)(_ => 1)(_ + _ + 1)
  }
  def maximum3(tree: Tree[Int]): Int = {
    foldViaCps(tree)(identity)(_ max _)
  }
  def depth3[A](tree: Tree[A]): Int = {
    foldViaCps(tree)(_ => 1)((l, r) => (l max r) + 1)
  }
  def map3[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
    foldViaCps(tree)(v => Leaf(f(v)): Tree[B])(Branch(_, _))
  }

  def foldViaEval[A, B](tree: Tree[A])(f: A => B)(g: (B,B) => B): B = {
    @annotation.tailrec
    def eval(exp: Exp[A, B]): B = {
      exp.reduce() match {
        case Value(v) => v
        case _        => eval(exp.reduce())
      }
    }
    eval(Fold(tree, f, g))
  }
  sealed trait Exp[A, B] {
    def reduce(): Exp[A, B]
  }
  case class Fold[A, B](tree: Tree[A], f: A => B, g: (B, B) => B) extends Exp[A, B] {
    override def reduce(): Exp[A, B] = tree match {
      case Leaf(v)      => Value(f(v))
      case Branch(l, r) => G(Fold(l, f, g), Fold(r, f, g), g)
    }
  }
  case class G[A, B](l: Exp[A, B], r: Exp[A, B], g: (B, B) => B) extends Exp[A, B] {
    override def reduce(): Exp[A, B] = (l, r) match {
      case (Value(l),   Value(r)) => Value(g(l, r))
      case (l@Value(_), r)        => G(l, r.reduce(), g)
      case (l,          r)        => G(l.reduce(), r, g)
    }
  }
  case class Value[A, B](v: B) extends Exp[A, B] {
    override def reduce(): Exp[A, B] = this
  }
  def size4[A](tree: Tree[A]): Int = {
    foldViaEval(tree)(_ => 1)(_ + _ + 1)
  }
  def maximum4(tree: Tree[Int]): Int = {
    foldViaEval(tree)(identity)(_ max _)
  }
  def depth4[A](tree: Tree[A]): Int = {
    foldViaEval(tree)(_ => 1)((l, r) => (l max r) + 1)
  }
  def map4[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
    foldViaEval(tree)(v => Leaf(f(v)): Tree[B])(Branch(_, _))
  }
}
