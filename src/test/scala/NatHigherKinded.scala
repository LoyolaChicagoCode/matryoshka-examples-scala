package edu.luc.cs.cs372.matryoshka

import scalaz.Functor
import scalaz.std.option._

import scalaz.scalacheck.ScalazProperties._
import org.scalacheck.{ Prop, Properties }

import matryoshka._
import matryoshka.implicits._
import matryoshka.data.Fix

/*
 * In this example, we demonstrate that we can unify the natf and natoption
 * examples by defining an abstraction to describe the initial algebra
 * of a given endofunctor.
 *
 * As a result, we can avoid the duplication of essentially equivalent code
 * between NatF and NatOption!
 */

/**
 * A common abstraction for the initial algebra of any endofunctor
 * isomorphic to `Option`.
 *
 * @tparam F the endofunctor
 */
trait FInitial[F[+_]] {

  /** The zero constructor. */
  def z: F[Nothing]

  /** The successor constructor. */
  def s[A]: A => F[A]

  /** The extractor. */
  def out[A]: F[A] => Option[A]

  /** The `Functor` typeclass instance. */
  implicit val fFunctor = new Functor[F] {
    def map[A, B](fa: F[A])(f: A => B): F[B] = out(fa) match {
      case None    => z: F[B]
      case Some(n) => s(f(n))
    }
  }
}

/**
  * A common abstraction over the code in the `natf` and `natoption`
  * example worksheets made possible by the `FInitial` abstraction.
  *
  * @tparam F the endofunctor we are going to examine
  */
abstract class FInitialTest[F[+_]](fi: FInitial[F]) extends Properties("NatHigherKinded") {

  import fi._

  val zero: Fix[F]           = Fix[F](z)
  val succ: Fix[F] => Fix[F] = n => Fix[F](s(n))

  val one   = succ(zero)
  val two   = succ(one)
  val three = succ(two)

  val toInt: Algebra[F, Int] = out(_) match {
    case None    => 0
    case Some(n) => n + 1
  }

  property("cata0") = Prop { (zero cata toInt) == 0 }
  property("cata3") = Prop { (three cata toInt) == 3 }

  val fromInt: Coalgebra[F, Int] = (n: Int) => {
    require { n >= 0 }
    if   (n == 0) z
    else          s(n - 1)
  }

  property("ana0") = Prop { (0 ana[Fix[F]] fromInt cata toInt) == 0 }
  property("ana7") = Prop { (7 ana[Fix[F]] fromInt cata toInt) == 7 }

  val plus: Fix[F] => Algebra[F, Fix[F]] = m => out(_) match {
    case None    => m
    case Some(n) => succ(n)
  }

  property("plus00") = Prop { (zero  cata plus(zero)  cata toInt) == 0 }
  property("plus03") = Prop { (zero  cata plus(three) cata toInt) == 3 }
  property("plus30") = Prop { (three cata plus(zero)  cata toInt) == 3 }
  property("plus23") = Prop { (two   cata plus(three) cata toInt) == 5 }
}


/**
 * Initial algebra for the `Option` endofunctor.
 * This is conceptually the identity descriptor.
 */
object OptionC extends FInitial[Option] {
  override val z = None
  override def s[A] = Some(_)
  override def out[A] = identity _
}

/**
 * Initial algebra for the `NatF` endofunctor.
 * This benefits from an outer wrapper for bundling everything together.
 * It also exposes the isomorphism between `NatF` and `Option`.
 */
object NatFWrapper {
  sealed trait NatF[+A]
  case object Zero extends NatF[Nothing]
  case class Succ[+A](n: A) extends NatF[A]
  object NatFC extends FInitial[NatF] {
    override val z = Zero
    override def s[A] = Succ(_)
    override def out[A] = {
      case Zero    => None
      case Succ(n) => Some(n)
    }
  }
}

// now testing both functor choices using the same code :D
object OptionCTest extends FInitialTest(OptionC)
object NatFCTest extends FInitialTest(NatFWrapper.NatFC)
