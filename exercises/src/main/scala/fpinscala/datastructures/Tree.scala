package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
  def size[A](t: Tree[A]): Int =
    fold(t)(_ => 1)(_ + _ + 1)

  def maximum(t: Tree[Int]): Int =
    fold(t)(a => a)(_ max _)

  def depth[A](t: Tree[A]): Int =
    fold(t)(_ => 1)((x,y) => (x max y) + 1)

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_,_))

  def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = t match {
    case Leaf(a) => f(a)
    case Branch(l,r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }
}

object TreeTest {
  def test(name: String)(actual: Any)(expected: Any) {
    if (actual == expected)
      println(s"$name works!")
    else
      println(s"$name fails - $actual is not $expected")
  }

  def main(args: Array[String]): Unit = {
    val tree: Tree[Int] = Branch(Branch(Leaf(1), Leaf(7)), Leaf(3))

    test("size")(Tree.size(tree))(5)
    test("maximum")(Tree.maximum(tree))(7)
    test("depth")(Tree.depth(tree))(3)
    test("map")(Tree.map(tree)(x => x.toString))(Branch(Branch(Leaf("1"), Leaf("7")), Leaf("3")))
  }
}
