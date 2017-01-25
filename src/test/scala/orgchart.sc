import scalaz.{ Cofree, Functor }
import scalaz.std.anyVal._     // for assert_=== to work on basic values
import scalaz.std.string._     // for assert_=== to work on strings
import scalaz.std.function._   // provides standard instances of common function types
import scalaz.syntax.arrow._   // provides arrow operators used below
import scalaz.syntax.equal._   // for assert_===

import scalamu._               // algebra types and injected cata method

/**
 * Endofunctor for (generic) algebra in the category Scala types.
 * {{{
 * data NodeF[+A] = P | OU(A*)
 * }}}
 * (This is largely equivalent to rose trees.)
 *
 * @tparam A argument of the endofunctor
 */
sealed trait NodeF[+A] { def children: Seq[A] }
case object P extends NodeF[Nothing] { val children = Seq.empty }
case class OU[+A](children: A*) extends NodeF[A]

/**
 * Implicit value for declaring `NodeF` as a `Functor` in scalaz.
 */
implicit val NodeFFunctor = new Functor[NodeF] {
  def map[A, B](fa: NodeF[A])(f: A => B): NodeF[B] = fa match {
    case P => P
    case OU(cs @ _*) => OU(cs map f: _*)
  }
}

// TODO NodeMonad/flatMap

/**
 * Least fixpoint of `NodeF` (recursive type based on `NodeF`)
 * as generic carrier object for initial algebra.
 * By contrast with the nongeneric examples, this requires
 * the explicit use of `Cofree` as the container of item values.
 *
 * @tparam A generic item type of the resulting carrier object
 */
type Node[T] = Cofree[NodeF, T]

/**
 * Factory methods for convenience.
 */
def p[T](value: T)                      = Cofree[NodeF, T](value, P)
def ou[T](value: T, children: Node[T]*) = Cofree[NodeF, T](value, OU(children: _*))

val org =
  ou(("The Outfit", 50),
    p(("CEO", 140)),
    p(("Assistant to CEO", 60)),
    ou(("Retail Dept", 70),
      p(("Dir of Retail", 120)),
      p(("Asst Dir of Retail", 90)),
      p(("Retail Clerk", 50))
    ),
    ou(("IT Dept", 130),
      p(("Dir of IT", 110)),
      p(("IT Specialist", 85))
    )
  )

org.map(_._1.length).head assert_=== 10

def size[A]: GenericAlgebra[A, NodeF, Int] = _ => {
  case P => 1
  case OU(cs @ _*) => cs.sum
}

org cata size assert_=== 7

def depth[A]: GenericAlgebra[A, NodeF, Int] = _ => {
  case P => 1
  case OU(cs @ _*) => 1 + cs.max
}

org cata depth assert_=== 3

def incBy(perc: Float)(num: Int): Int = scala.math.round(num.toFloat * (100 + perc) / 100)

val orgAfterRaise = org map (incBy(2.5f) _).second
orgAfterRaise.tail.children(0).head._2 assert_=== 144

val orgSanitized = orgAfterRaise map { _._1 }
orgSanitized.head assert_=== "The Outfit"

// TODO scalamu lenses to give raise to single person

println("■")
