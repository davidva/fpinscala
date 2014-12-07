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

  def map[A,B](l: List[A])(f: A => B): List[B] = sys.error("todo")
}

object ListTest {
  def main(args: Array[String]): Unit = {
    println("tail ? %b".format(List.tail(List(1, 2, 3)) == List(2, 3)))
    println("tail ? %b".format(List.tail(List(1)) == Nil))
    println("setHead ? %b".format(List.setHead(List(1), 0) == List(0)))
    println("setHead ? %b".format(List.setHead(List(1, 2), 0) == List(0, 2)))
    println("drop ? %b".format(List.drop(List(1, 2), 0) == List(1, 2)))
    println("drop ? %b".format(List.drop(List(1, 2), 2) == Nil))
    println("drop ? %b".format(List.drop(List(1, 2), 3) == Nil))
    println("dropWhile ? %b".format(List.dropWhile(List(1, 2, 1), (x: Int) => x > 0) == Nil))
    println("dropWhile ? %b".format(List.dropWhile(List(1, 2, 1), (x: Int) => x < 0) == List(1, 2, 1)))
    println("dropWhile ? %b".format(List.dropWhile(List(1, 2, 1), (x: Int) => x < 2) == List(2, 1)))
    println("init ? %b".format(List.init(List(1)) == Nil))
    println("init ? %b".format(List.init(List(1, 2, 3, 4)) == List(1, 2, 3)))
    println("length ? %b".format(List.length(Nil) == 0))
    println("length ? %b".format(List.length(List(1, 2, 3, 4)) == 4))
    println("foldLeft ? %b".format(List.foldLeft(List(1, 2, 3, 4), 10)(_ + _) == 20))

    val ints = List(1, 2, 3, 4)
    println("sumLeft ? %b".format(List.sum(ints) == List.sumLeft(ints)))

    val doubles = List[Double](1, 2, 3, 4)
    println("productLeft ? %b".format(List.product(doubles) == List.productLeft(doubles)))

    println("lengthLeft ? %b".format(List.length(doubles) == List.lengthLeft(doubles)))
  }
}
