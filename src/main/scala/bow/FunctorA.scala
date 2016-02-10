package bow

import scala.language.higherKinds
import scalaz.{Monad, Applicative, Functor}
import bow.std._
import scalaz.syntax.arrow._

/** A functor that could map inside an arrow */

trait FunctorA[F[_]] extends Functor[F] {
  def mapA[=>:[_, _], A, B](f: A =>: B)(implicit A: ArrowChoice[=>:]): F[A] =>: F[B]

  def map[A, B](fa: F[A])(f: A => B): F[B] = mapA[Function1, A, B](f).apply(fa)
}

trait PointA[F[_]] {
  def pointA[=>:[_, _], A](implicit A: ArrowChoice[=>:]): A =>: F[A]
}

trait ApplicativeA[F[_]] extends FunctorA[F] with Applicative[F] with PointA[F] {

  def apA[=>:[_, _], A, B](f: F[A =>: B])(implicit A: ArrowChoice[=>:]): F[A] =>: F[B]

  def point[A](a: => A): F[A] = pointA[Function1, A].apply(a)

  def ap[A, B](fa: => F[A])(f: => F[A => B]): F[B] = apA[Function1, A, B](f).apply(fa)

  def mapA[=>:[_, _], A, B](f: A =>: B)(implicit A: ArrowChoice[=>:]): F[A] =>: F[B] = apA(point(f))
}

trait MonadA[F[_]] extends FunctorA[F] with PointA[F] with Monad[F] {
  def bindA[=>:[_, _], A, B](f: A =>: F[B])(implicit A: ArrowChoice[=>:]): F[A] =>: F[B]

  def bind[A, B](fa: F[A])(f: A => F[B]): F[B] = bindA[Function1, A, B](f).apply(fa)

  def mapA[=>:[_, _], A, B](f: A =>: B)(implicit A: ArrowChoice[=>:]): F[A] =>: F[B] = bindA(f >>> pointA[=>:, B])
}
