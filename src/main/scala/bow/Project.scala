package bow

import scala.language.higherKinds
import scalaz.Functor
import bow.std._
/** A functor that could map inside an arrow*/

trait Project[F[_]] extends Functor[F]{
  def project[=>:[_,_], A, B](f: A =>: B)(implicit ar: ArrowChoice[=>:]): F[A] =>: F[B]

  def map[A, B](fa: F[A])(f: A => B): F[B] = project[Function1, A, B](f).apply(fa)
}
