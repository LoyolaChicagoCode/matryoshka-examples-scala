package edu.luc.cs.cs372.matryoshka

import scalaz.{ Equal, Functor }
import scalaz.std.option._
import scalaz.std.anyVal._

import scalaz.scalacheck.ScalazArbitrary._
import scalaz.scalacheck.ScalaCheckBinding._
import scalaz.scalacheck.ScalazProperties._
import org.scalacheck.{ Arbitrary, Gen, Prop, Properties }
import Gen._
import Arbitrary._

import matryoshka._
import matryoshka.implicits._
import matryoshka.data.Fix
import matryoshka.scalacheck.arbitrary._

object NatOption extends Properties("NatOption") {

  // ambiguity between RecursiveT and BirecursiveT without this definition
  implicit def fixBirecursive(implicit r: BirecursiveT[Fix]) = matryoshka.birecursiveTBirecursive(r)

  type Nat = Fix[Option]

  val zero = Fix[Option](None)
  val succ = (n: Nat) => Fix[Option](Some(n))

  val two = succ(succ(zero))
  val three = succ(two)

  val toInt: Algebra[Option, Int] = {
    case None    => 0
    case Some(n) => n + 1
  }

  property("cataWorks") = Prop { three.cata(toInt) == 3 }

  val plus: Nat => Algebra[Option, Nat] = m => {
    case None    => m
    case Some(n) => succ(n)
  }

  property("cataWorks2") = Prop { two.cata(plus(three)).cata(toInt) == 5 }

  val fromInt: Coalgebra[Option, Int] = n => {
    require { n >= 0 }
    if (n == 0) None
    else Some(n - 1)
  }

  property("anaWorks") = Prop { 7.ana[Nat](fromInt).cata(toInt) == 7 }

  implicit object optionEqual extends Delay[Equal, Option] {
    def apply[A](eq: Equal[A]) = Equal.equal {
      case (None, None)       => true
      case (Some(a), Some(b)) => eq.equal(a, b)
      case _                  => false
    }
  }

  property("equalsOptionInt") = equal.laws[Option[Int]]
  property("equalsOptionOptionInt") = equal.laws[Option[Option[Int]]]
  property("equalsNat") = equal.laws[Nat]
  property("functorOption") = functor.laws[Option]

}