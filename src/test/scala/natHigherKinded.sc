import scalaz.Functor
import scalaz.std.anyVal._     // for assert_=== to work on basic values
import scalaz.std.option._
import scalaz.syntax.equal._   // for assert_===
import scalamu._               // algebra types and injected cata method

/*
 * In this example, we demonstrate that we can unify the natf and natoption
 * examples by defining an abstraction to describe the initial algebra
 * of a given endofunctor.
 *
 * As a result, we can avoid the duplication of essentially equivalent code
 * between natf and natoption!
 */

/**
 * A common abstraction for the initial algebra of any endofunctor
 * isomorphic to `Option`.
 *
 * @tparam F the endofunctor
 */
trait FInitial[F[+_]] {

  /** The zero constructor. */
  val z: F[Nothing]

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
 * @param fi the initial algebra of the endofunctor
 */
def test[F[+_]](fi: FInitial[F]): Unit = {
  import fi._

  val zero: µ[F] = In[F](z)
  val succ: µ[F] => µ[F] = n => In[F](s(n))

  val one   = succ(zero)
  val two   = succ(one)
  val three = succ(two)

  val toInt: Algebra[F, Int] = out(_) match {
    case None    => 0
    case Some(n) => n + 1
  }

  // unsure why this works only when applied to `F`
  implicit val enableCata = ToMuOps[F] _

  zero cata toInt assert_=== 0
  three cata toInt assert_=== 3

  val fromInt: Coalgebra[F, Int] = (n: Int) => {
    require { n >= 0 }
    if   (n == 0) z
    else          s(n - 1)
  }

  µ.unfold(0)(fromInt) cata toInt assert_=== 0
  µ.unfold(7)(fromInt) cata toInt assert_=== 7

  val plus: µ[F] => Algebra[F, µ[F]] = m => out(_) match {
    case None    => m
    case Some(n) => succ(n)
  }
  zero  cata plus(zero)  cata toInt assert_=== 0
  zero  cata plus(three) cata toInt assert_=== 3
  three cata plus(zero)  cata toInt assert_=== 3
  two   cata plus(three) cata toInt assert_=== 5
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
  object C extends FInitial[NatF] {
    override val z = Zero
    override def s[A] = Succ(_)
    override def out[A] = {
      case Zero    => None
      case Succ(n) => Some(n)
    }
  }
}

// now testing both functor choices using the same code :D
test(NatFWrapper.C)
test(OptionC)

println("■")
