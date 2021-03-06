package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }

  def double(rng: RNG): (Double, RNG) =
    map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))(rng)

  def intDouble(rng: RNG): ((Int,Double), RNG) = both(int, double)(rng)

  def doubleInt(rng: RNG): ((Double,Int), RNG) = both(double, int)(rng)

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    sequence(List.fill(count)(int))(rng)

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] = map2(ra, rb)((_, _))

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, r1) = f(rng)
      g(a)(r1)
    }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt)(i => {
      val mod = i % n
      if (i + (n-1) - mod >= 0) unit(mod)
      else nonNegativeLessThan(n)
    })
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    flatMap(a => State.unit(f(a)))

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb map (b => f(a, b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s => {
      val (a, s1) = run(s)
      f(a) run s1
    })
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  def unit[S,A](a: A): State[S,A] = State(s => (a, s))

  def sequence[S,A](fs: List[State[S,A]]): State[S,List[A]] =
    fs.foldRight(unit[S, List[A]](List[A]())) ((f, acc) => f.map2(acc)(_ :: _))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- getState // Gets the current state and assigns it to `s`.
    _ <- setState(f(s)) // Sets the new state to `f` applied to `s`.
  } yield ()

  def getState[S]: State[S, S] = State(s => (s, s))

  def setState[S](s: S): State[S, Unit] = State(_ => ((), s))

  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    for {
      _ <- sequence(inputs.map(i => modify((s: Machine) => (i, s) match {
        case (_, Machine(_, 0, _)) => s
        case (Coin, Machine(false, _, _)) => s
        case (Coin, Machine(true, candy, coin)) => Machine(false, candy, coin + 1)
        case (Turn, Machine(true, _, _)) => s
        case (Turn, Machine(false, candy, coin)) => Machine(true, candy - 1, coin)
      })))
      s <- getState
    } yield (s.candies, s.coins)
}

object StateTest extends App with fpinscala.Test {
  val rng0 = RNG.Simple(0)
  val rng1 = RNG.Simple(1)

  test("nonNegativeInt")(RNG.nonNegativeInt(rng1)._1)(384748)
  test("double")(RNG.double(rng1)._1)(1.7916224896907806E-4)

  test("intDouble")(RNG.intDouble(rng0)._1)((0, 0.0019707889296114445))
  test("doubleInt")(RNG.doubleInt(rng0)._1)((0.0, 4232237))

  test("ints")(RNG.ints(0)(rng1)._1)(Nil)
  test("ints")(RNG.ints(5)(rng1)._1)(List(384748, -1151252339, -549383847, 1612966641, -883454042))

  test("sequence")(RNG.sequence(List(RNG.unit(1), RNG.unit(2), RNG.unit(3)))(rng1)._1)(List(1, 2, 3))

  test("nonNegativeLessThan")(RNG.nonNegativeLessThan(2)(rng1)._1)(0)

  val result = State.simulateMachine(List(Coin, Turn, Turn, Coin, Coin, Turn)) run Machine(true, 5, 0)
  test("simulateMachine")(result._1)(Machine(true, 3, 2))
}
