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

  def drop(n: Int): Stream[A] = this match {
    case Cons(h,t) => t()
    case _ => Empty
  }

  def takeWhile(p: A => Boolean): Stream[A] =
    foldRight(Empty: Stream[A])((a, acc) => if (p(a)) cons(a, acc) else empty)

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, acc) => p(a) && acc)

  def startsWith[B](s: Stream[B]): Boolean =
    zipAll(s).takeWhile {
      case (_, None) => false
      case _ => true
    } forAll { case (a,b) => a == b }

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

  def mapWithUnfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Empty => None
      case Cons(h,t) => Some((f(h()), t()))
    }

  def takeWithUnfold(n: Int): Stream[A] =
    unfold((n, this)) {
      case (0, _) => None
      case (_, Empty) => None
      case (n, Cons(h,t)) => Some(h(), (n - 1, t()))
    }

  def takeWhileWithUnfold(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h,t) if (p(h())) => Some(h(), t())
      case _ => None
    }

  def zipWith[B, C](s: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this,s)) {
      case (Cons(h1,t1),Cons(h2,t2)) => Some(f(h1(),h2()), (t1(), t2()))
      case _ => None
    }

  def zipAll[B](s: Stream[B]): Stream[(Option[A],Option[B])] =
    unfold((this,s)) {
      case (Empty,Empty) => None
      case (Empty,Cons(h,t)) => Some((None,Some(h())),(Empty,t()))
      case (Cons(h,t),Empty) => Some((Some(h()),None),(t(),Empty))
      case (Cons(h1,t1),Cons(h2,t2)) => Some((Some(h1()),Some(h2())),(t1(),t2()))
    }

  def tails: Stream[Stream[A]] =
    unfold(this) {
      case Empty => None
      case s => Some((s, s drop 1))
    } append Stream(empty)
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

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def fibs: Stream[Int] = {
    def go(a: Int, b: Int): Stream[Int] =
      cons(a, go(b, a + b))
    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case None => Empty
      case Some((a, s)) => cons(a, unfold(s)(f))
    }

  def onesWithUnfold: Stream[Int] = unfold(None)(_ => Some(1, None))
  def constantWithUnfold[A](a: A): Stream[A] = unfold(None)(_ => Some(a, None))
  def fromWithUnfold(n: Int): Stream[Int] = unfold(n)(s => Some(s, s + 1))
  def fibsWithUnfold: Stream[Int] = unfold((0,1)) { case (a, b) => Some(a, (b, b + a)) }
}

object StreamTest extends App with fpinscala.Test {
  val stream123: Stream[Int] = Stream(1,2,3)
  val stream456: Stream[Int] = Stream(4,5,6)
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
  test("append")(stream123.append(stream456).toList)(List(1,2,3,4,5,6))
  test("flatMap")(stream123.flatMap(a => Stream(a, a)).toList)(List(1,1,2,2,3,3))

  test("constant")(Stream.constant("A").take(5).toList)(List("A","A","A","A","A"))

  test("from")(Stream.from(4).take(3).toList)(List(4,5,6))

  test("fibs")(Stream.fibs.take(8).toList)(List(0,1,1,2,3,5,8,13))

  test("onesWithUnfold")(Stream.onesWithUnfold.take(5).toList)(List(1,1,1,1,1))
  test("constantWithUnfold")(Stream.constantWithUnfold("A").take(5).toList)(List("A","A","A","A","A"))
  test("fromWithUnfold")(Stream.fromWithUnfold(4).take(3).toList)(List(4,5,6))
  test("fibsWithUnfold")(Stream.fibsWithUnfold.take(8).toList)(List(0,1,1,2,3,5,8,13))

  val emptyIntStream: Stream[Int] = empty;
  test("mapWithUnfold")(emptyIntStream.mapWithUnfold(_.toString).toList)(Nil)
  test("mapWithUnfold")(stream123.mapWithUnfold(_.toString).toList)(List("1","2","3"))
  test("takeWithUnfold")(stream123.takeWithUnfold(2).toList)(List(1,2))
  test("takeWithUnfold")(stream123.takeWithUnfold(4).toList)(List(1,2,3))
  test("takeWithUnfold")(emptyIntStream.takeWithUnfold(4).toList)(Nil)
  test("takeWhileWithUnfold")(emptyIntStream.takeWhileWithUnfold(isNot(3)).toList)(Nil)
  test("takeWhileWithUnfold")(stream123.takeWhileWithUnfold(isNot(1)).toList)(Nil)
  test("takeWhileWithUnfold")(stream123.takeWhileWithUnfold(isNot(3)).toList)(List(1,2))
  test("zipWith")(stream123.zipWith(emptyIntStream)((a,b) => s"$a$b").toList)(Nil)
  test("zipWith")(stream123.zipWith(stream456)((a,b) => s"$a$b").toList)(List("14","25","36"))
  test("zipAll")(Stream("A").zipAll(stream123).toList)(List((Some("A"),Some(1)),(None,Some(2)),(None,Some(3))))

  test("startsWith")(stream123 startsWith Stream(1,2))(true)
  test("startsWith")(stream123 startsWith stream123)(true)
  test("startsWith")(Stream(1,2) startsWith stream123)(false)
  test("startsWith")(emptyIntStream startsWith Stream(1,2))(false)

  test("tails")(stream123.tails.toList.map(_.toList))(Stream(Stream(1,2,3), Stream(2,3), Stream(3), Stream()).toList.map(_.toList))
}