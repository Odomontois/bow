package bow.sz

import bow.{>=:, ArrowApply}

import scala.language.higherKinds
import scalaz.{Monad, Comonad}
import scalaz.syntax.functor._

/**
  * User: Oleg
  * Date: 17-Feb-16
  * Time: 01:08
  */
final case class DiKleisli[W[_], M[_], A, B](run: W[A] => M[B])

trait DiKleisliInstances {
  implicit def diKleisliInstances[W[_], M[_]](implicit W: Comonad[W], M: Monad[M], D: W >=: M) = new ArrowApply[DiKleisli[W, M, ?, ?]] {

    import W.comonadSyntax._
    import M.monadSyntax._
    import D.distributiveSyntax._

    type >~>[a, b] = DiKleisli[W, M, a, b]

    @inline def >~>[a, b](f: W[a] => M[b]) = DiKleisli[W, M, a, b](f)

    def app[A, B]: (A >~> B, A) >~> B = ???

    def arr[A, B](f: (A) => B): A >~> B = >~> { wx => f(wx.copoint).point }

    def first[A, B, C](fa: A >~> B): (A, C) >~> (B, C) = >~> { wx => fa.run(wx.map(_._1)).map((_, wx.copoint._2)) }

    override def id[A]: DiKleisli[W, M, A, A] = >~> {_.copoint.point}

    def compose[A, B, C](f: B >~> C, g: A >~> B): A >~> C = >~> {_.cobind(g.run).distr >>= f.run}

  }
}

