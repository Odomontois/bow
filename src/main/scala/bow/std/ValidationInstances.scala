package bow.std

import bow.{ArrowChoice, FunctorA}

import scala.language.higherKinds
import scalaz.Validation
import bow.syntax._
import scalaz.syntax.arrow._

/**
  * User: Oleg
  * Date: 09-Feb-16
  * Time: 19:25
  */
trait ValidationInstances {
  implicit def validationProject[E] = new FunctorA[Validation[E, ?]] {
    def mapA[=>:[_, _], A, B](f: A =>: B)(implicit ar: ArrowChoice[=>:]): Validation[E, A] =>: Validation[E, B] =
      (ar.id[E] +++ f).dimap(_.disjunction, _.validation)
  }
}
