package edu.luc.cs.cs372.matryoshka

import scalaz.{ Equal, Functor }
import scalaz.std.anyVal._ // declares basic types as instances of the basic typeclasses

import scalaz.scalacheck.ScalaCheckBinding._
import scalaz.scalacheck.ScalazProperties._
import org.scalacheck.{ Arbitrary, Gen, Prop, Properties }
import Arbitrary._
import Prop.BooleanOperators

import matryoshka._
import matryoshka.implicits._
import matryoshka.data.Fix
import matryoshka.scalacheck.arbitrary._

/**
 * In this example, we represent natural numbers
 * essentially as lists without item values:
 *
 * 0 = zero
 * 3 = succ(succ(succ(zero)))
 *
 * We can then define operations such as addition on these.
 */
object NatF extends Properties("NatF") {

  /**
   * Endofunctor for (non-generic) F-algebra in the category Scala types:
   * {{{
   * data NatF[+T] = Zero | Succ(n: T)
   * }}}
   *
   * @tparam T argument (type parameter) of the endofunctor
   */
  sealed trait NatF[+T]

  case object Zero extends NatF[Nothing]

  case class Succ[+T](n: T) extends NatF[T]

  /**
   * Implicit value for declaring `NatF` as an instance of
   * typeclass `Functor` in scalaz.
   */
  implicit val natFFunctor = new Functor[NatF] {
    override def map[T, U](fa: NatF[T])(f: T => U): NatF[U] = fa match {
      case Zero    => Zero: NatF[U]
      case Succ(n) => Succ(f(n))
    }
  }

  /**
   * Required for equality on `NatF` to extend to `Fix[NatF]`
   * and related recursive types.
   */
  implicit object natFEqualD extends Delay[Equal, NatF] {
    override def apply[T](eq: Equal[T]) = Equal.equalA[NatF[T]]
  }

  implicit object natFArbitraryD extends Delay[Arbitrary, NatF] {
    override def apply[T](a: Arbitrary[T]) = Arbitrary {
      import Gen._
      oneOf(const(Zero), a.arbitrary.map(Succ(_)))
    }
  }

  // tests of equality and functor laws for `NatF`
  include(equal.laws[NatF[Unit]], "equalNatF.")
  include(equal.laws[NatF[NatF[Unit]]], "equalNatF2.")
  include(functor.laws[NatF], "functorNatF.")

  /**
   * Least fixpoint of `NatF` (recursive type based on `NatF`)
   * as carrier object for initial algebra.
   */
  type Nat = Fix[NatF]

  include(equal.laws[Nat], "equalNat.")

  // Factory methods for convenience.
  val zero = Fix[NatF](Zero)
  val succ = (n: Nat) => Fix[NatF](Succ(n))

  // some instances
  val one = succ(zero)
  val two = succ(one)
  val three = succ(two)

  /**
   * Conversion to `Int` as an `NatF`-algebra
   * for carrier object `Int` in the category Scala types.
   */
  val toInt: Algebra[NatF, Int] = {
    case Zero    => 0
    case Succ(n) => n + 1
  }

  // Using the catamorphism, we now can fold the `toInt` algebra into instances.
  // (This is an example of recursion.)
  property("cata0") = Prop { (zero cata toInt) == 0 }
  property("cata3") = Prop { (three cata toInt) == 3 }

  /**
   * Conversion from `Int` as an `NatF`-coalgebra
   * for carrier object `Int` in category Scala types
   * (generator for corecursion).
   */
  val fromInt: Coalgebra[NatF, Int] = (n: Int) => {
    require (n >= 0)
    if (n == 0) Zero
    else Succ(n - 1)
  }

  // Using the anamorphism on a coalgebra such as `fromInt`,
  // we can now unfold a `Nat` from an `Int`.
  // (This is an example of corecursion.)
  property("ana0") = Prop { (0 ana[Nat] fromInt cata toInt) == 0 }
  property("ana7") = Prop { (7 ana[Nat] fromInt cata toInt) == 7 }
  property("anaForall") = Prop.forAll { i: Int => (i >= 0 && i < 100) ==> ((i ana[Nat] fromInt cata toInt) == i) }

  /**
   * Addition to a number `m` as an `NatF`-algebra for carrier object
   * `Nat` in the category Scala types.
   *
   * @param m the number to which we are adding the argument of the algebra
   */
  val plus: Nat => Algebra[NatF, Nat] = m => {
    case Zero    => m
    case Succ(n) => succ(n)
  }

  property("cata00") = Prop { (zero cata plus(zero) cata toInt) == 0 }
  property("cata03") = Prop { (zero cata plus(three) cata toInt) == 3 }
  property("cata30") = Prop { (three cata plus(zero) cata toInt) == 3 }
  property("cata23") = Prop { (two cata plus(three) cata toInt) == 5 }

  /**
   * Multiplication by a number `m` as an `NatF`-algebra for carrier object
   * `Nat` in the category Scala types.
   *
   * @param m the number to which we are adding the argument of the algebra
   */
  val times: Nat => Algebra[NatF, Nat] = m => {
    case Zero    => zero
    case Succ(n) => n.cata(plus(m))
  }

  property("cataOnTimes00") = Prop { (zero cata times(zero) cata toInt) == 0 }
  property("cataOnTimes03") = Prop { (zero cata times(three) cata toInt) == 0 }
  property("cataOnTimes30") = Prop { (three cata times(zero) cata toInt) == 0 }
  property("cataOnTimes23") = Prop { (two cata times(three) cata toInt) == 6 }

  /**
   * Argument function for `para`. Returns `one` when there is no accumulated
   * result yet. Otherwise it multiplies the accumulated result by the current
   * receiver value during traversal, whose tail (out) is passed as `curr` by
   * `para`.
   * By contrast, F-algebras do not have access to the current receiver value
   * during traversal!
   * Exercise: This has a similar type signature as `plus` and `times`. What
   * are the key differences?
   *
   * @param curr the tail of the current receiver value
   * @return the current receiver times the accumulated result
   */
  def oneOrTimes(curr: NatF[Nat]): Algebra[NatF, Nat] = {
    case Zero      => one
    case Succ(acc) => Fix[NatF](curr) cata times(acc)
  }

  property("oneOrTimes20") = Prop { (oneOrTimes(Succ(two))(Zero) cata toInt) == 1 }
  property("oneOrTimes03") = Prop { (oneOrTimes(Zero) (Succ(three)) cata toInt) == 0 }
  property("oneOrTimes12") = Prop { (oneOrTimes(Succ(one))(Succ(two)) cata toInt) == 4 }
  property("oneOrTimes23") = Prop { (oneOrTimes(Succ(two))(Succ(three)) cata toInt) == 9 }

  // TODO table-driven property test
  //  (0 to 5) zip Seq(1, 1, 2, 6, 24, 120) foreach { case (arg, result) =>
  //    µ.unfold(arg)(fromInt) para oneOrTimes cata toInt assert_=== result
  //  }
}
