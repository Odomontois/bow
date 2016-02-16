package bow

import scala.language.higherKinds

/**
  * User: Oleg
  * Date: 17-Feb-16
  * Time: 01:34
  */
trait Distribute[W[_], M[_]] {
  def distr[A](x: W[M[A]]): M[W[A]]

  val distributiveSyntax = new DistributeSyntax[W, M] {def _D = Distribute.this}

}

trait DistributeSyntax[W[_], M[_]] {
  def _D: Distribute[W, M]

  implicit class DistributeOps[X](wx: W[M[X]]) {
    def distr: M[W[X]] = _D.distr(wx)
  }
}

