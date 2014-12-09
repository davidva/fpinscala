package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
case class Cons[+A](head: A, tail: List[A]) extends List[A] // Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`, which may be `Nil` or another `Cons`.

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("empty list")
    case Cons(_, xs) => xs
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => sys.error("empty list")
    case Cons(_, xs) => Cons(h, xs)
  }

  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(x, xs) => drop(xs, n - 1)
    }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(x, xs) if f(x) => dropWhile(xs, f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("empty list")
    case Cons(_, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((x, y) => y + 1)

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def sumLeft(l: List[Int]): Int = foldLeft(l, 0)(_ + _)

  def productLeft(l: List[Double]): Double = foldLeft(l, 1.0)(_ * _)

  def lengthLeft[A](l: List[A]): Int = foldLeft(l, 0)((acc, y) => acc + 1)

  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])((acc, y) => Cons(y, acc))

  def appendWithFoldRight[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)(Cons(_, _))

  def concat[A](xs: List[List[A]]): List[A] =
    foldLeft(xs, Nil: List[A])(append)

  def plus1(l: List[Int]): List[Int] =
    foldRight(l, List[Int]())((x, acc) => Cons(x + 1, acc))

  def mapToStr(l: List[Int]): List[String] =
    foldRight(l, List[String]())((x, acc) => Cons(x.toString, acc))

  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil: List[B])((x, acc) => Cons(f(x), acc))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil: List[A])((x, acc) => if (f(x)) Cons(x, acc) else acc)

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    concat(map(as)(f))

  def filterWithFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(a => if (f(a)) List(a) else Nil)

  def addPairWise(as: List[Int], bs: List[Int]): List[Int] =
    zipWith(as, bs)(_ + _)

  def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] = (as, bs) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(a, as), Cons(b, bs)) => Cons(f(a, b), zipWith(as, bs)(f))
  }
}

object ListTest {
  def test(name: String)(actual: Any)(expected: Any) {
    if (actual == expected)
      println(s"$name works!")
    else
      println(s"$name fails - $actual is not $expected")
  }

  def main(args: Array[String]): Unit = {
    test("tail")(List.tail(List(1, 2, 3)))(List(2, 3))
    test("tail")(List.tail(List(1)))(Nil)
    test("setHead")(List.setHead(List(1), 0))(List(0))
    test("setHead")(List.setHead(List(1, 2), 0))(List(0, 2))
    test("drop")(List.drop(List(1, 2), 0))(List(1, 2))
    test("drop")(List.drop(List(1, 2), 2))(Nil)
    test("drop")(List.drop(List(1, 2), 3))(Nil)
    test("dropWhile")(List.dropWhile(List(1, 2, 1), (x: Int) => x > 0))(Nil)
    test("dropWhile")(List.dropWhile(List(1, 2, 1), (x: Int) => x < 0))(List(1, 2, 1))
    test("dropWhile")(List.dropWhile(List(1, 2, 1), (x: Int) => x < 2))(List(2, 1))
    test("init")(List.init(List(1)))(Nil)
    test("init")(List.init(List(1, 2, 3, 4)))(List(1, 2, 3))
    test("length")(List.length(Nil))(0)
    test("length")(List.length(List(1, 2, 3, 4)))(4)
    test("foldLeft")(List.foldLeft(List(1, 2, 3, 4), 10)(_ + _))(20)

    val ints = List(1, 2, 3, 4)
    test("sumLeft")(List.sum(ints))(List.sumLeft(ints))

    val doubles = List[Double](1, 2, 3, 4)
    test("productLeft")(List.product(doubles))(List.productLeft(doubles))
    test("lengthLeft")(List.length(doubles))(List.lengthLeft(doubles))

    test("reverse")(List.reverse(List(1, 2, 3, 4)))(List(4, 3, 2, 1))

    test("appendWithFoldRight")(List.appendWithFoldRight(List(1, 2), List(3, 4)))(List(1, 2, 3, 4))

    test("plus1")(List.plus1(List(1, 2, 3)))(List(2, 3, 4))
    test("mapToStr")(List.mapToStr(List(1, 2, 3)))(List("1", "2", "3"))
    test("map")(List.map(List(1, 2, 3, 4))(_ * 2))(List(2, 4, 6, 8))

    test("filter")(List.filter(List(1, 2, 3, 4))(_ % 2 == 0))(List(2, 4))

    test("flatMap")(List.flatMap(List(1, 2, 3))(i => List(i, i)))(List(1, 1, 2, 2, 3, 3))

    test("filterWithFlatMap")(List.filterWithFlatMap(List(1, 2, 3, 4))(_ % 2 == 0))(List(2, 4))
    test("addPairWise")(List.addPairWise(List(1, 2, 3), List(4, 5, 6)))(List(5, 7, 9))
  }
}
