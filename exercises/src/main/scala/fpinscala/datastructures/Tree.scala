package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(a) => a
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + depth(l) max depth(r)
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
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
