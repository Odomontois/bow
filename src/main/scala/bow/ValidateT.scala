package bow

import bow.ValidateT.AppValid

import scala.language.higherKinds
import scalaz._
import scalaz.syntax.arrow._
import scalaz.syntax.applicative._
import bow.syntax._
import bow.functions._
import bow.std._


/**
  * User: Oleg
  * Date: 06-Feb-16
  * Time: 22:37
  * Validate Arrow transformer
  */
sealed abstract class ValidateT[=>:[_, _] : ArrowChoice, A, E, B] {
  def run: A =>: Validation[E, B]

  def andThen[C](v: ValidateT[=>:, B, E, C]): ValidateT[=>:, A, E, C] = new ComposeValidateT[=>:, A, E, B, C](this, v)
}

final case class SuccessValidateT[=>:[_, _] : ArrowChoice, A, E, B](f: A =>: B) extends ValidateT[=>:, A, E, B] {
  def run = f >>^ Validation.success

  override def andThen[C](v: ValidateT[=>:, B, E, C]): ValidateT[=>:, A, E, C] = v match {
    case SuccessValidateT(g) => SuccessValidateT[=>:, A, E, C](f >>> g)
    case SimpleValidateT(g) => SimpleValidateT(f >>> g)
    case _ => super.andThen(v)
  }
}

final case class SimpleValidateT[=>:[_, _] : ArrowChoice, A, E, B](run: A =>: Validation[E, B]) extends ValidateT[=>:, A, E, B]

final case class SplitValidateT[=>:[_, _] : ArrowChoice, A1, A2, E, B1, B2]
(first: ValidateT[=>:, A1, E, B1], second: ValidateT[=>:, A2, E, B2])
(implicit ap: Applicative[Validation[E, ?]], ar: Arrow[=>:]) extends ValidateT[=>:, (A1, A2), E, (B1, B2)] {
  def run: (A1, A2) =>: Validation[E, (B1, B2)] = first.run *** second.run >>^ { case (x, y) => x tuple y }

}

final case class ComposeValidateT[=>:[_, _], A, E, B, C]
(start: ValidateT[=>:, A, E, B], continue: ValidateT[=>:, B, E, C])(implicit A: ArrowChoice[=>:]) extends ValidateT[=>:, A, E, C] {
  def run: A =>: Validation[E, C] = start.run >>^ (_.disjunction) >>> (A.arr(Validation.failure[E, C]) ||| continue.run)

  override def andThen[D](v: ValidateT[=>:, C, E, D]): ValidateT[=>:, A, E, D] = ComposeValidateT(start, continue andThen v)
}

final case class ChooseValidateT[=>:[_, _] : ArrowChoice, A1, A2, E, B1, B2]
(left: ValidateT[=>:, A1, E, B1], right: ValidateT[=>:, A2, E, B2]) extends ValidateT[=>:, A1 \/ A2, E, B1 \/ B2] {
  def run: (A1 \/ A2) =>: Validation[E, B1 \/ B2] = (left.run +++ right.run) >>^ (_.bitraverse[Validation[E, ?], B1, B2](identity, identity))
}

object ValidateT {
  type AppValid[E] = Applicative[Validation[E, ?]]

  implicit def instance[=>:[_, _] : ArrowChoice, E: AppValid]: Arrow[ValidateT[=>:, ?, E, ?]] = new ValidateTInstance[=>:, E]

  def fail[=>:[_, _], E, A](implicit A: ArrowChoice[=>:]): ValidateT[=>:, E, E, A] = SimpleValidateT(A.arr(Validation.failure[E, A]))

  def success[=>:[_, _] : ArrowChoice, A, E, B](f: A =>: B): ValidateT[=>:, A, E, B] = SuccessValidateT[=>:, A, E, B](f)

  def apply[=>:[_, _]] = new ApplyBuilder[=>:]

  def wrap[=>:[_, _], E, A](implicit ar: ArrowChoice[=>:]) = ValidateT.apply(ar.id[Validation[E, A]])


  class ApplyBuilder[=>:[_, _]] {
    def apply[E, A, B](f: A =>: Validation[E, B])(implicit ar: ArrowChoice[=>:]): ValidateT[=>:, A, E, B] = SimpleValidateT(f)
  }
}

final class ValidateTInstance[=>:[_, _], E: AppValid](implicit A: ArrowChoice[=>:]) extends ArrowChoice[ValidateT[=>:, ?, E, ?]] {
  type =>![A, B] = ValidateT[=>:, A, E, B]

  def success[A, B](x: A =>: B) = SuccessValidateT[=>:, A, E, B](x)

  override def split[A1, B1, A2, B2](fa: A1 =>! B1, fb: A2 =>! B2) = SplitValidateT(fa, fb)

  def arr[A, B](f: A => B): A =>! B = success(A.arr(f))

  def id[A]: A =>! A = success[A, A](A.id)

  def compose[A, B, C](f: B =>! C, g: A =>! B): A =>! C = g andThen f

  def first[A, B, C](fa: A =>! B): (A, C) =>! (B, C) = split(fa, id[C])

  /** Feed marked inputs through the argument arrow, passing the rest through unchanged to the output. */
  def left[A, B, C](fa: A =>! B): (A \/ C) =>! (B \/ C) = ChooseValidateT(fa, id[C])

  override def second[A, B, C](f: A =>! B): (C, A) =>! (C, B) = split(id[C], f)

  override def mapfst[A, B, C](fab: A =>! B)(f: C => A): C =>! B = fab match {
    case SuccessValidateT(g) => success(g ^>> f)
    case SimpleValidateT(g) => SimpleValidateT(g ^>> f)
    case ComposeValidateT(g, h) => ComposeValidateT(mapfst(g)(f), h)
    case _ => super.mapfst(fab)(f)
  }

  override def mapsnd[A, B, C](fab: A =>! B)(f: (B) => C): A =>! C = fab match {
    case SuccessValidateT(g) => success(g >>^ f)
    case ComposeValidateT(g, h) => ComposeValidateT(g, mapsnd(h)(f))
    case _ => super.mapsnd(fab)(f)
  }
}

class ValidateAspect[E](implicit app: AppValid[E]) extends Aspect[ValidateT[?[_, _], ?, E, ?]] {
  def liftClass[A[_, _]](ar: ArrowChoice[A]): ArrowChoice[ValidateT[A, ?, E, ?]] = new ValidateTInstance[A, E]()(app, ar)

  def tmap[A1[_, _], A2[_, _]](f: A1 ~~> A2)(implicit ar1: ArrowChoice[A1], ar2: ArrowChoice[A2]) =
    new (ValidateT[A1, ?, E, ?] ~~> ValidateT[A2, ?, E, ?]) {
      def apply[A, B](v: ValidateT[A1, A, E, B]): ValidateT[A2, A, E, B] = SimpleValidateT(f(v.run))
    }

  def lift[A[_, _], b, c](x: A[b, c])(implicit ar: ArrowChoice[A]): ValidateT[A, b, E, c] = SuccessValidateT[A, b, E, c](x)
}





