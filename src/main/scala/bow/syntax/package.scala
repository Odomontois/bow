package bow

import scala.language.higherKinds
import scalaz.\/

/**
  * User: Oleg
  * Date: 06-Feb-16
  * Time: 23:37
  */
package object syntax {
  implicit class ChoiceOps[=>:[_, _], A, B](fa: A =>: B)(implicit ar: ArrowChoice[=>:]) {
    def left[C] = ar.left[A, B, C](fa)

    def right[C] = ar.right[A, B, C](fa)

    def +++[A1, B1](fb: A1 =>: B1): (A \/ A1) =>: (B \/ B1) = ar.choose(fa)(fb)

    def |||[A1](fb: A1 =>: B): (A \/ A1) =>: B = ar.fanin(fa)(fb)

    def project[F[_]](implicit P: FunctorA[F]): F[A] =>: F[B] = P.mapA(fa)
  }

  implicit class ChoiceCondOps[=>:[_, _], A](val cond: A =>: Boolean)(implicit ar: ArrowChoice[=>:]) {
    def ifTrue(fa: A =>: A): A =>: A = ar.ifThenElse(cond)(fa, ar.id[A])

    def ifFalse(fb: A =>: A): A =>: A = ar.ifThenElse(cond)(ar.id[A], fb)

    def thenElse[B](fa: A =>: B, fb: A =>: B): A =>: B = ar.ifThenElse(cond)(fa, fb)
  }

  implicit class PlusOps[=>:[_, _], A, B](fa: A =>: B)(implicit ar: ArrowPlus[=>:]) {
    def <+>(fb: A =>: B): A =>: B = ar.plus(fa, fb)
  }


}
