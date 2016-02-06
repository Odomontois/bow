package bow

import bow.ValidateT._

import scala.language.higherKinds
import scalaz._
import scalaz.syntax.arrow._
import scalaz.syntax.applicative._
import bow.syntax._
import bow.functions._


/**
  * User: Oleg
  * Date: 06-Feb-16
  * Time: 22:37
  * Validate Arrow transformer
  */
sealed trait ValidateT[=>:[_, _], A, E, B] {
  implicit val ar: ArrowChoice[=>:] = implicitly
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

final case class SimpleValidateT[=>:[_, _], A, E, B](run: A =>: Validation[E, B]) extends ValidateT[=>:, A, E, B]

final case class SplitValidateT[=>:[_, _] : ArrowChoice, A1, A2, E, B1, B2]
(left: ValidateT[=>:, A1, E, B1], right: ValidateT[=>:, A2, E, B2])
(implicit ap: Applicative[Validation[E, ?]], ar: Arrow[=>:]) extends ValidateT[=>:, (A1, A2), E, (B1, B2)] {
  def run: (A1, A2) =>: Validation[E, (B1, B2)] = left.run *** right.run >>^ { case (x, y) => x tuple y }

}

final case class ComposeValidateT[=>:[_, _] : ArrowChoice, A, E, B, C]
(start: ValidateT[=>:, A, E, B], continue: ValidateT[=>:, B, E, C]) extends ValidateT[=>:, A, E, C] {
  def run: A =>: Validation[E, C] = start.run >>^ (_.disjunction) >>> (arr(Validation.failure[E, C]) ||| continue.run)

  override def andThen[D](v: ValidateT[=>:, C, E, D]): ValidateT[=>:, A, E, D] = ComposeValidateT(start, continue andThen v)
}

object ValidateT {
  type AppValid[E] = Applicative[Validation[E, ?]]

  implicit def instance[=>:[_, _] : ArrowChoice, E: AppValid]: Arrow[ValidateT[=>:, ?, E, ?]] = new ValidateTInstance[=>:, E]

  def fail[=>:[_, _] : ArrowChoice, E, A]: ValidateT[=>:, E, E, A] = SimpleValidateT(arr(Validation.failure[E, A]))
}

final class ValidateTInstance[=>:[_, _], E: AppValid](implicit A: ArrowChoice[=>:]) extends Arrow[ValidateT[=>:, ?, E, ?]] {
  type =>![A, B] = ValidateT[=>:, A, E, B]

  def success[A, B](x: A =>: B) = SuccessValidateT[=>:, A, E, B](x)

  override def split[A1, B1, A2, B2](fa: A1 =>! B1, fb: A2 =>! B2) = SplitValidateT(fa, fb)

  def arr[A, B](f: A => B): A =>! B = success(A.arr(f))

  def id[A]: A =>! A = success[A, A](A.id)

  def compose[A, B, C](f: B =>! C, g: A =>! B): A =>! C = g andThen f

  def first[A, B, C](fa: A =>! B): (A, C) =>! (B, C) = split(fa, id[C])

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

