package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)

  }
  def take(n: Int): Stream[A] =
    if (n > 0) this match {
      case Cons(h,t) if n == 1 => cons(h(), empty)
      case Cons(h,t) => cons(h(), t().take(n - 1))
      case _ => Empty

    }
    else Stream.empty

  def drop(n: Int): Stream[A] = sys.error("todo")

  def takeWhile(p: A => Boolean): Stream[A] =
    foldRight(Empty: Stream[A])((a, acc) => if (p(a)) cons(a, acc) else empty)

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, acc) => p(a) && acc)

  def startsWith[B](s: Stream[B]): Boolean = sys.error("todo")

  def toList: List[A] = foldRight(Nil: List[A])((a,acc) => a :: acc)

  def headOption: Option[A] =
    foldRight(None: Option[A])((a,_) => Some(a))

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((a, b) => cons(f(a), b))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) =>
      if (f(h)) cons(h, t)
      else t)

  def append[B>:A](other: => Stream[B]): Stream[B] =
    foldRight(other)((h, t) => cons(h, t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h,t) => f(h) append t)
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)
  def from(n: Int): Stream[Int] = sys.error("todo")

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = sys.error("todo")
}

object StreamTest {
  import fpinscala.Test.test

  def main(args: Array[String]): Unit = {
    val stream123: Stream[Int] = Stream(1,2,3)
    test("toList")(stream123.toList)(List(1,2,3))

    test("take")(Stream.empty.take(2).toList)(Nil)
    test("take")(stream123.take(4).toList)(List(1,2,3))
    test("take")(stream123.take(2).toList)(List(1,2))

    def isNot(n: Int)(i: Int): Boolean = i != n
    test("takeWhile")(Stream.empty.takeWhile(isNot(1)).toList)(Nil)
    test("takeWhile")(stream123.takeWhile(isNot(1)).toList)(Nil)
    test("takeWhile")(stream123.takeWhile(isNot(3)).toList)(List(1,2))

    test("forAll")(Stream.empty.forAll(isNot(1)))(true)
    test("forAll")(stream123.forAll(isNot(4)))(true)
    test("forAll")(stream123.forAll(isNot(2)))(false)

    test("headOption")(Stream.empty.headOption)(None)
    test("headOption")(stream123.headOption)(Some(1))

    test("map")(stream123.map(_.toString).toList)(List("1","2","3"))
    test("filter")(stream123.filter(_ < 3).toList)(List(1,2))
    test("append")(stream123.append(Stream(4,5,6)).toList)(List(1,2,3,4,5,6))
    test("flatMap")(stream123.flatMap(a => Stream(a, a)).toList)(List(1,1,2,2,3,3))
  }
}