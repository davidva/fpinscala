package fpinscala.errorhandling


import scala.{Option => _, Either => _, Left => _, Right => _, _} // hide std library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Either[+E,+A] {
 def map[B](f: A => B): Either[E, B] = this match {
   case Left(e) => Left(e)
   case Right(a) => Right(f(a))
 }

 def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
   case Left(e) => Left(e)
   case Right(a) => f(a)
 }

 def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
  case Left(_) => b
  case _ => this
 }

 def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = (this, b) match {
  case (Left(e), _) => Left(e)
  case (_, Left(e)) => Left(e)
  case (Right(a), Right(b)) => Right(f(a, b))
 }
}
case class Left[+E](get: E) extends Either[E,Nothing]
case class Right[+A](get: A) extends Either[Nothing,A]

object Either {
  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] =
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

}

object EitherTest {
  import fpinscala.Test.test

  def main(args: Array[String]): Unit = {
    val left: Either[String,Int] = Left("I'm left")
    val right0: Either[String,Int] = Right(0)
    val right2: Either[String,Int] = Right(2)
    val right4: Either[String,Int] = Right(4)

    def dup(x: Int): Int = x * 2
    test("map")(left map dup)(left)
    test("map")(right2 map dup)(right4)

    def invert(x: Int): Either[String, Double] = x match {
      case 0 => Left("cannot divide by 0")
      case x => Right(1d / x)
    }

    test("flatMap")(left flatMap invert)(left)
    test("flatMap")(right0 flatMap invert)(Left("cannot divide by 0"))
    test("flatMap")(right2 flatMap invert)(Right(0.5))

    test("orElse")(left orElse right2)(right2)
    test("orElse")(right0 orElse right2)(right0)

    def append(x: Int, y: Int): String = s"$x$y"
    test("map2")(left.map2(right2)(append))(left)
    test("map2")(right0.map2(left)(append))(left)
    test("map2")(right0.map2(right2)(append))(Right("02"))
  }
}
