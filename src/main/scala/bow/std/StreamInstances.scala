package bow.std

import bow.{ArrowChoice, Project}

import scala.language.higherKinds
import scalaz.{-\/, \/-, \/, StreamTMonadPlus}
import bow.syntax._
import scalaz.syntax.arrow._

/**
  * User: Oleg
  * Date: 09-Feb-16
  * Time: 18:23
  */
trait StreamInstances {
  implicit object streamProject extends Project[Stream] {
    def decompose[A]: Stream[A] => (Stream[Nothing] \/ (A, Stream[A])) = {
      case x #:: rest => \/-(x, rest)
      case _ => -\/(Stream.empty)
    }

    def compose[A]: (Stream[Nothing] \/ (A, Stream[A])) => Stream[A] = _.fold(identity, { case (x, rest) => x #:: rest })

    def project[=>:[_, _], A, B](f: A =>: B)(implicit ar: ArrowChoice[=>:]): Stream[A] =>: Stream[B] = {
      val recur = ar.id[Stream[Nothing]] +++ (f *** project(f))
      recur.dimap(decompose, compose)
    }
  }
}
