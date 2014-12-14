package fpinscala.errorhandling


import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def getOrElse[B>:A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse None

  def orElse[B>:A](ob: => Option[B]): Option[B] = map(Some(_)) getOrElse ob

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if (f(a)) => Some(a)
    case _ => None
  }
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = sys.error("todo")

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
    case (None,_) | (_,None) => None
    case (Some(x), Some(y)) => Some(f(x,y))
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case a :: as => a flatMap (aa => sequence(as) map (aa :: _))
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = sys.error("todo")
}

object OptionTest {
  def test(name: String)(actual: Any)(expected: Any) {
    if (actual == expected)
      println(s"$name works!")
    else
      println(s"$name fails - $actual is not $expected")
  }

  def main(args: Array[String]): Unit = {
    test("map")(None.map(x => Some(x.toString)))(None)
    test("map")(Some(1).map(x => Some(x.toString)))(Some(Some("1")))

    test("getOrElse")(None.getOrElse("a"))("a")
    test("getOrElse")(Some("1").getOrElse(Some("a")))("1")

    test("flatMap")(None.flatMap(x => Some(x.toString)))(None)
    test("flatMap")(Some(1).flatMap(x => Some(x.toString)))(Some("1"))

    test("orElse")(None.orElse(Some("a")))(Some("a"))
    test("orElse")(Some("1").orElse(Some("a")))(Some("1"))

    test("filter")(None.filter(a => true))(None)
    test("filter")(Some(1).filter(a => true))(Some(1))

    test("map2")(Option.map2(None, Some(5))((x: Int, y: Int) => x * y))(None)
    test("map2")(Option.map2(Some(4), Some(5))((x: Int, y: Int) => x * y))(Some(20))

    test("sequence")(Option.sequence(List(Some(1),None,Some(3))))(None)
    test("sequence")(Option.sequence(List(Some(1),Some(2),Some(3))))(Some(List(1,2,3)))
  }
}
