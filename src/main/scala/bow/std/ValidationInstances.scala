package bow.std

import bow.{ArrowChoice, Project}

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
  implicit def validationProject[E] = new Project[Validation[E, ?]] {
    def project[=>:[_, _], A, B](f: A =>: B)(implicit ar: ArrowChoice[=>:]): Validation[E, A] =>: Validation[E, B] =
      (ar.id[E] +++ f).dimap(_.disjunction, _.validation)
  }
}
