package bow

import scala.language.higherKinds
import scalaz.\/

/**
  * User: Oleg
  * Date: 06-Feb-16
  * Time: 23:37
  */
package object syntax {
  implicit class ChoiceOps[=>:[_, _], A, B](val fa: A =>: B)(implicit ar: ArrowChoice[=>:]) {
    def +++[A1, B1](fb: A1 =>: B1): (A \/ A1) =>: (B \/ B1) = ar.choose(fa)(fb)

    def |||[A1](fb: A1 =>: B): (A \/ A1) =>: B = ar.fanin(fa)(fb)

    def project[F[_]](implicit P: Project[F]): F[A] =>: F[B] = P.project(fa)
  }


}
