package edu.luc.cs.cs372.matryoshka

import scalaz.{ Bifunctor, Functor } // basic typeclasses
import scalaz.std.anyVal._ // declares basic types as instances of the basic typeclasses
import scalaz.std.string._ // declares strings as instances of the basic typeclasses
import scalaz.std.tuple._ // declares tuples as instances of the basic typeclasses
import scalaz.std.option._ // declares option as an instance of the basic typeclasses
import scalaz.syntax.functor._
import scalaz.syntax.bifunctor._

import scalaz.scalacheck.ScalazProperties._
import org.scalacheck.{ Arbitrary, Prop, Properties }
import Arbitrary._
import Prop.BooleanOperators

import matryoshka._
import matryoshka.implicits._
import matryoshka.data.Fix
import matryoshka.scalacheck.arbitrary._

/**
 * Generic `NEL` (non-empty list) as initial algebra of the `Option` endofunctor
 * in the category Scala types.
 */
object NonEmptyList extends Properties("NonEmptyList") {

  /** Required to resolve the ambiguity between RecursiveT and BirecursiveT. */
  implicit def fixBirecursive(implicit r: BirecursiveT[Fix]) = matryoshka.birecursiveTBirecursive(r)

  /**
   * A node in a non-empty list.
   *
   * @tparam A the item type
   * @tparam B the type of the optional rest of the list
   */
  type NelF[A, B] = (A, Option[B])

  implicit object bifunctorNelF extends Bifunctor[NelF] {
    override def bimap[A, B, C, D](fab: (A, Option[B]))(f: (A) => C, g: (B) => D) =
      fab.bimap(f, _.map(g))
  }

  property("bifunctorNelF") = bifunctor.laws[NelF]

  implicit def functorNelF[C]: Functor[NelF[C, ?]] = bifunctorNelF.rightFunctor

  property("functorNelF") = functor.laws[NelF[Unit, ?]]

  /**
   * Least fixpoint of `NelF` in its second parameter `B`
   * as generic carrier object for initial algebra.
   * (recursive type based on `NelF`).
   *
   * @tparam A item type of the resulting carrier object
   */
  type Nel[A] = Fix[NelF[A, ?]]

  // factory methods for convenience
  def point[A](head: A): Nel[A] = Fix[NelF[A, ?]]((head, None: Option[Nel[A]]))
  def cons[A](head: A, tail: Nel[A]): Nel[A] = Fix[NelF[A, ?]]((head, Some(tail)))

  // some instances

  val list1 = point("hello")
  val list2 = cons("world", list1)
  val list3 = cons("good morning", list2)

  /**
   * Generic `NelF[A, ?]`-algebra for carrier object `Int` in category Scala types.
   * Note that this is nonrecursive.
   *
   * @tparam A generic item type of the algebra
   */
  def length[A]: Algebra[NelF[A, ?], Int] = {
    case (_, None)    => 1 // end of list:  0
    case (_, Some(n)) => 1 + n // regular node: add 1 to sum accumulated so far
  }

  property("cataT1") = Prop { RecursiveT[Fix].cataT[NelF[String, ?], Int](list1)(length) == 1 }
  property("cataT3") = Prop { RecursiveT[Fix].cataT[NelF[String, ?], Int](list3)(length) == 3 }

  // now we can fold the length algebra into instances.

  // FIXME get these to work
  // property("cataT1") = Prop { list1.cata(length) == 1 }
  // property("cataT3") = Prop { list3.cata(length) == 3 }

  /**
   * Nongeneric `NelF[Int, ?]`-algebra for carrier object `Int`
   * but specific item type, also `Int`.
   */
  val sum: Algebra[NelF[Int, ?], Int] = {
    case (v, None)    => v // end of list:  0
    case (v, Some(n)) => v + n // regular node: add value to sum accumulated so far
  }

  val list4 = cons(4, cons(3, cons(2, point(1))))

  property("cataT4") = Prop { RecursiveT[Fix].cataT[NelF[Int, ?], Int](list4)(sum) == 10 }

  // FIXME get this to work
  // property("cataT4a") = Prop { list4.cata(sum) == 10 }

  /**
   * Generic `Option`-coalgebra for carrier object `Int` in category Scala types.
   */
  val downFrom: Coalgebra[Option, Int] = (n: Int) => {
    require { n >= 0 }
    if (n == 0) None /* 0: end of list */
    else Some(n / 2) /* > 0: create node for n and continue with n / 2 */
  }

  // Now we can create instances by unfolding the coalgebra from a starting value.

  // FIXME get these to work
  // property("ana1") = Prop { 1.ana[Nel[Int]](downFrom).cata(length) == 1 }
  // property("ana8") = Prop { 8.ana[Nel[Int]](downFrom).cata(length) == 4 }
}