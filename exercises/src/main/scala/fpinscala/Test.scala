package fpinscala

trait Test {
  def test(name: String)(actual: Any)(expected: Any) {
    if (actual == expected)
      println(s"$name works!")
    else
      println(s"$name fails - $actual is not $expected")
  }
}
