package bow.std

import bow.{ArrowChoice, FunctorA}

import scala.language.higherKinds
import scalaz.{-\/, \/-, \/, StreamTMonadPlus}
import scalaz.syntax.arrow._

/**
  * User: Oleg
  * Date: 09-Feb-16
  * Time: 18:23
  */
trait StreamInstances {
  implicit object streamFunctorA$ extends FunctorA[Stream] {
    def decompose[A]: Stream[A] => (Stream[Nothing] \/ (A, Stream[A])) = {
      case x #:: rest => \/-(x, rest)
      case _ => -\/(Stream.empty)
    }

    def compose[A]: (Stream[Nothing] \/ (A, Stream[A])) => Stream[A] = _.fold(identity, { case (x, rest) => x #:: rest })

    def mapA[=>:[_, _], A, B](f: A =>: B)(implicit ar: ArrowChoice[=>:]): Stream[A] =>: Stream[B] = {
      val recur = ar.chooseLz(ar.id[Stream[Nothing]])(f *** mapA(f))
      recur.dimap(decompose, compose)
    }
  }
}
