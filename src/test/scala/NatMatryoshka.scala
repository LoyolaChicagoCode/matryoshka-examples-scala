// simple example illustrating how to replace 
// https://github.com/LoyolaChicagoCode/scalamu
// with https://github.com/slamdata/matryoshka
//
// libraryDependencies ++= Seq(
//   "org.scalaz"     %% "scalaz-core"           % "7.2.8",
//   "com.slamdata"   %% "matryoshka-core"       % "0.16.4",
//   "com.slamdata"   %% "matryoshka-scalacheck" % "0.16.4" % Test
// )

import scalaz.Functor
import scalaz.std.option._

import matryoshka._
import matryoshka.implicits._
import matryoshka.data.Fix

// ambiguity between RecursiveT and BirecursiveT without this definition
implicit def fixBirecursive(implicit r: BirecursiveT[Fix]) = matryoshka.birecursiveTBirecursive(r)

type Nat = Fix[Option]

val zero = Fix[Option](None)
val succ = (n: Nat) => Fix[Option](Some(n))

val two   = succ(succ(zero))
val three = succ(two)

val toInt: Algebra[Option, Int] = {
  case None    => 0
  case Some(n) => n + 1
}

assert { three.cata(toInt) == 3 }

val plus: Nat => Algebra[Option, Nat] = m => {
  case None    => m
  case Some(n) => succ(n)
}

assert { two.cata(plus(three)).cata(toInt) == 5 }

val fromInt: Coalgebra[Option, Int] = n => {
  require { n >= 0 }
  if   (n == 0) None
  else          Some(n - 1)
}

assert { 7.ana[Nat](fromInt).cata(toInt) == 7 }

import scalaz.Equal
import scalaz.std.anyVal._
import scalaz.scalacheck.ScalazProperties.{ equal, functor }
import matryoshka.scalacheck.arbitrary._

implicit object optionEqual extends Delay[Equal, Option] {
  def apply[A](eq: Equal[A]) = Equal.equal {
    case (None, None)       => true
    case (Some(a), Some(b)) => eq.equal(a, b)
    case _                  => false
  }
}

equal.laws[Option[Int]].check
equal.laws[Nat].check
functor.laws[Option].check
