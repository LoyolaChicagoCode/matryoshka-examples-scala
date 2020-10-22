package edu.luc.cs.cs371.matryoshka

import scalaz.Equal
import scalaz.std.option._ // declares option as an instance of the basic typeclasses
import scalaz.std.anyVal._ // declares basic types as instances of the basic typeclasses

import scalaz.scalacheck.ScalazProperties._
import org.scalacheck.{ Arbitrary, Prop, Properties }
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
object NatOption extends Properties("NatOption") {

  /*
   * A (nongeneric) F-algebra in the category Scala types:
   * we use the predefined Option[_] endofunctor
   * of the category Scala types (type constructor of arity 1 with a `map`
   * function that obeys certain laws). This is already defined
   * as an instance of typeclass Functor in scalaz.
   */

  /**
   * Least fixpoint of `Option` as carrier object for initial algebra
   * (recursive type based on `Option`).
   */
  type Nat = Fix[Option]

  /**
   * Required for equality on `Option` to extend to `Fix[Option]`
   * and related recursive types.
   */
  implicit object optionEqualD extends Delay[Equal, Option] {
    override def apply[T](eq: Equal[T]) = Equal.equalA[Option[T]]
  }

  // tests of equality and functor laws for `Option` and `Nat`
  include(equal.laws[Option[Unit]], "equalOption.")
  include(equal.laws[Option[Option[Unit]]], "equalOption2.")
  include(equal.laws[Nat], "equalNat.")
  include(functor.laws[Option], "functorOption")

  // factory methods for convenience.
  val zero = Fix[Option](None)
  val succ = (n: Nat) => Fix[Option](Some(n))

  // some instances
  val one = succ(zero)
  val two = succ(succ(zero))
  val three = succ(two)

  /**
   * Conversion to `Int` as an `Option`-algebra
   * for carrier object `Int` in the category Scala types.
   */
  val toInt: Algebra[Option, Int] = {
    case None    => 0
    case Some(n) => n + 1
  }

  // Using the catamorphism, we now can fold the `toInt` algebra into instances.
  // (This is an example of recursion.)
  property("cata0") = Prop { (zero cata toInt) == 0 }
  property("cata3") = Prop { (three cata toInt) == 3 }

  /**
   * Conversion from `Int` as an `Option`-coalgebra
   * for carrier object `Int` in category Scala types
   * (generator for corecursion).
   */
  val fromInt: Coalgebra[Option, Int] = n => {
    require { n >= 0 }
    if (n == 0) None
    else Some(n - 1)
  }

  // Using the anamorphism on a coalgebra such as `fromInt`,
  // we can now unfold a `Nat` from an `Int`.
  // (This is an example of corecursion.)
  property("ana0") = Prop { (0 ana[Nat] fromInt cata toInt) == 0 }
  property("ana7") = Prop { (7 ana[Nat] fromInt cata toInt) == 7 }
  property("anaForall") = Prop.forAll { i: Int => (i >= 0 && i < 100) ==> ((i ana[Nat] fromInt cata toInt) == i) }

  /**
   * Addition to a number `m` as an `Option`-algebra for carrier object
   * `Nat` in the category Scala types.
   *
   * @param m the number to which we are adding the argument of the algebra
   */
  val plus: Nat => Algebra[Option, Nat] = m => {
    case None    => m
    case Some(n) => succ(n)
  }

  property("cata00") = Prop { (zero cata plus(zero) cata toInt) == 0 }
  property("cata03") = Prop { (zero cata plus(three) cata toInt) == 3 }
  property("cata30") = Prop { (three cata plus(zero) cata toInt) == 3 }
  property("cata23") = Prop { (two cata plus(three) cata toInt) == 5 }

  /**
   * Multiplication by a number `m` as an `Option`-algebra for carrier object
   * `Nat` in the category Scala types.
   *
   * @param m the number to which we are adding the argument of the algebra
   */
  val times: Nat => Algebra[Option, Nat] = m => {
    case None    => zero
    case Some(n) => n cata plus(m)
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
   * @return the current receiver times the accumulated result
   */
  val oneOrTimes: GAlgebra[(Nat, ?), Option, Nat] = {
    case None              => one
    case Some((curr, acc)) => succ(curr) cata times(acc)
  }

  property("oneOrTimes20")    = Prop { (oneOrTimes(None) cata toInt) == 1 }
  property("oneOrTimes12")    = Prop { (oneOrTimes(Some(one, two)) cata toInt) == 4 }
  property("oneOrTimes23")    = Prop { (oneOrTimes(Some(two, three)) cata toInt) == 9 }
  property("paraOneOrTImes3") = Prop { (three para oneOrTimes cata toInt) == 6 }

  // TODO table-driven property test
  //  (0 to 5) zip Seq(1, 1, 2, 6, 24, 120) foreach { case (arg, result) =>
  //    µ.unfold(arg)(fromInt) para oneOrTimes cata toInt assert_=== result
  //  }
}
