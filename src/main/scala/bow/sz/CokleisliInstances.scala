package bow.sz

import bow.{ArrowApply, ArrowChoice}
import bow.functions.LazyFunc

import scala.language.higherKinds
import scalaz._
import scalaz.Comonad._
import scalaz.syntax.functor._
import scalaz.syntax.either._

/**
  * User: Oleg
  * Date: 16-Feb-16
  * Time: 23:04
  */
trait CokleisliInstances {
  implicit def cokleisliArrowChoice[W[_]](implicit W: Comonad[W]) = new ArrowChoice[Cokleisli[W, ?, ?]] with ArrowApply[Cokleisli[W, ?, ?]] {

    import W.comonadSyntax._

    /** Feed marked inputs through the argument arrow, passing the rest through unchanged to the output. */
    type >>~[a, b] = Cokleisli[W, a, b]

    @inline def >>~[a, b](x: W[a] => b): a >>~ b = Cokleisli[W, a, b](x)


    def arr[A, B](f: A => B): A >>~ B = >>~(x => f(x.copoint))

    override def id[A]: A >>~ A = >>~(W.copoint)

    def first[A, B, C](fa: A >>~ B): (A, C) >>~ (B, C) =
      >>~ { fx => (fa.run(fx.map {_._1}), fx.copoint._2) }

    override def second[A, B, C](f: A >>~ B): (C, A) >>~ (C, B) =
      >>~ { fx => (fx.copoint._1, f.run(fx.map {_._2})) }

    def app[A, B]: (A >>~ B, A) >>~ B = >>~ { fa => fa.copoint._1.run(fa.map(_._2)) }

    override def join[A]: (Unit >>~ A) >>~ A = >>~ { fa => fa.copoint.run(fa >|()) }

    def compose[A, B, C](f: B >>~ C, g: A >>~ B): A >>~ C = >>~ { x => f.run(x cobind g.run) }

  }
}
