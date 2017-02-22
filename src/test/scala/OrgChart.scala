package edu.luc.cs.cs372.matryoshka

import scalaz.{ Bifunctor, Equal, Functor } // basic typeclasses
import scalaz.std.anyVal._ // declares basic types as instances of the basic typeclasses
import scalaz.std.string._ // declares strings as instances of the basic typeclasses
import scalaz.std.list._ // declares lists as instances of the basic typeclasses
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
 * Generic org chart as initial algebra of a `List`-based bifunctor
 * in the category Scala types.
 * {{{
 * data NodeF[A, B] = P(A) | OU(A, B*)
 * }}}
 * (This is largely equivalent to rose trees.)
 */
object OrgChart extends Properties("OrgChart") {

  /**
   * A node in an org chart.
   *
   * @tparam H the head (item) type
   * @tparam T the type of the zero or more children (tails) of the list
   */
  case class NodeF[H, T](item: H, children: List[T])

  implicit def nodeFArbitraryD[H](implicit c: Arbitrary[H]) = new Delay[Arbitrary, NodeF[H, ?]] {
    override def apply[T](a: Arbitrary[T]) = Arbitrary {
      import Gen._
      (c.arbitrary ⊛ listOf[T](a.arbitrary))(NodeF[H, T](_, _))
    }
  }

  implicit def nodeFEqualD[H] = new Delay[Equal, NodeF[H, ?]] {
    override def apply[T](eq: Equal[T]) = Equal.equalA[NodeF[H, T]]
  }

  include(equal.laws[NodeF[Unit, Unit]], "equalNodeF.")
  include(equal.laws[NodeF[Unit, NodeF[Unit, Unit]]], "equalNodeF2.")

  implicit object nodeFBifunctor extends Bifunctor[NodeF] {
    override def bimap[H, T, J, U](fab: NodeF[H, T])(f: H => J, g: T => U) = fab match {
      case NodeF(h, t) => NodeF(f(h), t map g)
    }
  }

  include(bifunctor.laws[NodeF], "bifunctorNodeF.")

  implicit def nodeFFunctor[H]: Functor[NodeF[H, ?]] = nodeFBifunctor.rightFunctor

  include(functor.laws[NodeF[Unit, ?]], "functorNodeF.")

  /**
   * Least fixpoint of `NodeF` in its second argument `T`
   * as generic carrier object for initial algebra.
   * (recursive type based on `NodeF`).
   *
   * @tparam H generic item type of the resulting carrier object
   */
  type Node[H] = Fix[NodeF[H, ?]]

  // FIXME figure out why this gets stuck
  //  include(equal.laws[Node[Unit]], "equalNode.")

  // factory methods for convenience

  def p[H](head: H): Node[H] = Fix[NodeF[H, ?]](NodeF(head, List.empty))
  def ou[H](head: H, tail: Node[H]*): Node[H] = Fix[NodeF[H, ?]](NodeF(head, tail.toList))

  // TODO Traverse

  val org: Node[(String, Int)] =
    ou(
      ("The Outfit", 50),
      p(("CEO", 140)),
      p(("Assistant to CEO", 60)),
      ou(
        ("Retail Dept", 70),
        p(("Dir of Retail", 120)),
        p(("Asst Dir of Retail", 90)),
        p(("Retail Clerk", 50))
      ),
      ou(
        ("IT Dept", 130),
        p(("Dir of IT", 110)),
        p(("IT Specialist", 85))
      )
    )

  import scalaz.syntax.functor._
  property("map0") = Prop { (org map identity) == org }
  property("map1") = Prop { (org map (_._1.length)).unFix.item == 10 }

  def size[H]: Algebra[NodeF[H, ?], Int] = {
    case NodeF(_, Nil) => 1
    case NodeF(_, cs)  => cs.sum
  }

  property("cataSize") = Prop { (org cata size) == 7 }

  def height[H]: Algebra[NodeF[H, ?], Int] = {
    case NodeF(_, Nil) => 1
    case NodeF(_, cs)  => 1 + cs.max
  }

  property("cataHeight") = Prop { (org cata height) == 3 }

  def incBy(perc: Float)(num: Int): Int = scala.math.round(num.toFloat * (100 + perc) / 100)

  import scalaz.std.function._
  import scalaz.syntax.arrow._
  val orgAfterRaise = org map (incBy(2.5f) _).second
  property("orgAfterRaise") = Prop { orgAfterRaise.unFix.children(0).unFix.item._2 == 144 }

  val orgSanitized = orgAfterRaise map (_._1)
  property("orgSanitized") = Prop { orgSanitized.unFix.item == "The Outfit" }

  // TODO explore the use of lenses to give raise to single person
}