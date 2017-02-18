package edu.luc.cs.cs372.matryoshka

import scalaz.{ Bifunctor, Equal, Functor } // basic typeclasses
import scalaz.std.anyVal._ // declares basic types as instances of the basic typeclasses
import scalaz.std.string._ // declares strings as instances of the basic typeclasses
import scalaz.std.option._ // declares option as an instance of the basic typeclasses
import scalaz.syntax.functor._
import scalaz.syntax.apply._

import scalaz.scalacheck.ScalaCheckBinding._
import scalaz.scalacheck.ScalazProperties._
import org.scalacheck.{ Arbitrary, Gen, Prop, Properties }
import Arbitrary._

import matryoshka._
import matryoshka.implicits._
import matryoshka.data.Fix
import matryoshka.scalacheck.arbitrary._

/**
 * Generic `NEL` (non-empty list) as initial algebra of an `Option`-based bifunctor
 * in the category Scala types.
 */
object NonEmptyList extends Properties("NonEmptyList") {

  /**
   * A node in a non-empty list.
   *
   * @tparam H the head (item) type
   * @tparam T the type of the optional child (tail) of the list
   */
  case class NelF[H, T](head: H, tail: Option[T])

  implicit def nelFArbitraryD[H](implicit c: Arbitrary[H]) = new Delay[Arbitrary, NelF[H, ?]] {
    override def apply[T](a: Arbitrary[T]) = Arbitrary {
      import Gen._
      (c.arbitrary ⊛ option(a.arbitrary))(NelF[H, T](_, _))
    }
  }

  implicit def nelFEqualD[H] = new Delay[Equal, NelF[H, ?]] {
    override def apply[T](eq: Equal[T]) = Equal.equalA[NelF[H, T]]
  }

  include(equal.laws[NelF[Unit, Unit]], "equalNelF.")
  include(equal.laws[NelF[Unit, NelF[Unit, Unit]]], "equalNelF2.")

  implicit object nelFBifunctor extends Bifunctor[NelF] {
    override def bimap[H, T, J, U](fab: NelF[H, T])(f: H => J, g: T => U) = fab match {
      case NelF(h, t) => NelF(f(h), t map g)
    }
  }

  include(bifunctor.laws[NelF], "bifunctorNelF.")

  implicit def nelFFunctor[H]: Functor[NelF[H, ?]] = nelFBifunctor.rightFunctor

  include(functor.laws[NelF[Unit, ?]], "functorNelF.")

  /**
   * Least fixpoint of `NelF` in its second argument `T`
   * as generic carrier object for initial algebra.
   * (recursive type based on `NelF`).
   *
   * @tparam H generic item type of the resulting carrier object
   */
  type Nel[H] = Fix[NelF[H, ?]]

  include(equal.laws[Nel[Unit]], "equalNel.")
  include(functor.laws[Nel], "functorNel.")

  // factory methods for convenience

  def point[H](head: H): Nel[H] = Fix[NelF[H, ?]](NelF(head, None))
  def cons[H](head: H, tail: Nel[H]): Nel[H] = Fix[NelF[H, ?]](NelF(head, Some(tail)))

  // some instances

  val list1 = point("hello")
  val list2 = cons("world", list1)
  val list3 = cons("good morning", list2)
  val list3n = cons(12, cons(5, point(5)))

  import scalaz.syntax.functor._
  property("map0") = Prop { (list3 map identity) == list3 }
  property("map1") = Prop { (list3 map (_.length)) == list3n }

  /**
   * Generic `NelF[H, ?]`-algebra for carrier object `Int` in category Scala types.
   * Note that this is nonrecursive.
   *
   * @tparam H generic item type of the algebra
   */
  def length[H]: Algebra[NelF[H, ?], Int] = {
    case NelF(_, None)    => 1 // end of list:  0
    case NelF(_, Some(n)) => 1 + n // regular node: add 1 to sum accumulated so far
  }

  // now we can fold the length algebra into instances.

  property("cataT1") = Prop { (list1 cata length) == 1 }
  property("cataT3") = Prop { (list3 cata length) == 3 }

  /**
   * Nongeneric `NelF[Int, ?]`-algebra for carrier object `Int`
   * but specific item type, also `Int`.
   */
  val sum: Algebra[NelF[Int, ?], Int] = {
    case NelF(v, None)    => v // end of list: 0
    case NelF(v, Some(n)) => v + n // regular node: add value to sum accumulated so far
  }

  val list4 = cons(4, cons(3, cons(2, point(1))))

  property("cataT4a") = Prop { (list4 cata sum) == 10 }

  /**
   * Generic `Option`-coalgebra for carrier object `Int` in category Scala types.
   */
  val downFrom: Coalgebra[NelF[Int, ?], Int] = (n: Int) => {
    require { n >= 0 }
    if (n == 0) NelF(0, None) /* 0: end of list */
    else NelF(n, Some(n / 2)) /* > 0: create node for n and continue with n / 2 */
  }

  // Now we can create instances by unfolding the coalgebra from a starting value.

  property("ana1") = Prop { (1 ana[Nel[Int]] downFrom cata length) == 2 }
  property("ana8") = Prop { (8 ana[Nel[Int]] downFrom cata length) == 5 }
}
