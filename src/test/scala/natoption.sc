import scalaz.std.anyVal._     // for assert_=== to work on basic values
import scalaz.std.option._     // for Option as Functor instance
import scalaz.syntax.equal._   // for assert_===

import scalamu._                // algebra types and injected cata method

/*
 * In this example, we represent natural numbers
 * essentially as lists without item values:
 *
 * 0 = zero
 * 3 = succ(succ(succ(zero)))
 *
 * We can then define operations such as addition on these.
 */

/*
 * A (nongeneric) F-algebra in the category Scala types:
 * we use the predefined Option[_] endofunctor
 * of the category Scala types (type constructor of arity 1 with a `map`
 * function that obeys certain laws). This is already defined
 * as an instance of typeclass Functor in scalaz.
 */

/**
 * Least fixpoint of `Option` (recursive type based on `Option`)
 * as carrier object for initial algebra.
 */
type Nat = µ[Option]

// factory methods for convenience.
val zero = In[Option](None)
val succ = (n: Nat) => In[Option](Some(n))

// some instances
val one   = succ(zero)
val two   = succ(one)
val three = succ(two)

/**
 * Conversion to `Int` as an `Option`-algebra
 * for carrier object `Int` in the category Scala types.
 */
val toInt: Algebra[Option, Int] = {
  case None    => 0
  case Some(n) => n + 1
}

// now we can fold the `toInt` algebra into instances
zero  cata toInt assert_=== 0
three cata toInt assert_=== 3

/**
 * Conversion from `Int` as an `Option`-coalgebra
 * for carrier object `Int` in category Scala types
 * (generator for corecursion).
 */
val fromInt: Coalgebra[Option, Int] = n => {
  require { n >= 0 }
  if   (n == 0) None
  else          Some(n - 1)
}

/*
 * Unfold is an anamorphism for unfolding a Nat from a coalgebra
 * such as `fromInt`. This is an example of corecursion.
 */
µ.unfold(0)(fromInt) cata toInt assert_=== 0
µ.unfold(7)(fromInt) cata toInt assert_=== 7

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

zero  cata plus(zero)  cata toInt assert_=== 0
zero  cata plus(three) cata toInt assert_=== 3
three cata plus(zero)  cata toInt assert_=== 3
two   cata plus(three) cata toInt assert_=== 5

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

zero  cata times(zero)  cata toInt assert_=== 0
zero  cata times(three) cata toInt assert_=== 0
three cata times(zero)  cata toInt assert_=== 0
two   cata times(three) cata toInt assert_=== 6

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
def oneOrTimes(curr: Option[Nat]): Algebra[Option, Nat] = {
  case None      => one
  case Some(acc) => In(curr) cata times(acc)
}

oneOrTimes(Some(two))(None)        cata toInt assert_=== 1
oneOrTimes(None)     (Some(three)) cata toInt assert_=== 0
oneOrTimes(Some(one))(Some(two))   cata toInt assert_=== 4
oneOrTimes(Some(two))(Some(three)) cata toInt assert_=== 9

(0 to 5) zip Seq(1, 1, 2, 6, 24, 120) foreach { case (arg, result) =>
  µ.unfold(arg)(fromInt) para oneOrTimes cata toInt assert_=== result
}

println("■")
