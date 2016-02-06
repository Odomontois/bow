package bow

import scalaz._

/**
  * User: Oleg
  * Date: 06-Feb-16
  * Time: 00:58
  */
sealed trait Validate[-A, +E, +B] {
  def run(x: A): Validation[E, B]
  def andThen[C, EE >: E](v: Validate[B, EE, C]): Validate[A, EE, C] = new ComposeValidate(this, v)
}

final case class SuccessValidate[-A, +B](f: A => B) extends Validate[A, Nothing, B] {
  def run(x: A) = Validation success f(x)

  override def andThen[C, EE >: Nothing](v: Validate[B, EE, C]): Validate[A, EE, C] = v match {
    case SuccessValidate(g) => SuccessValidate(f andThen g)
    case SimpleValidate(g) => SimpleValidate(f andThen g)
    case _ => super.andThen(v)
  }
}

final case class SimpleValidate[-A, +E, +B](f: A => Validation[E, B]) extends Validate[A, E, B] {
  def run(x: A) = f(x)
}

final case class SplitValidate[-A1, -A2, +E, +B1, +B2](left: Validate[A1, E, B1], right: Validate[A2, E, B2])
                                                      (implicit ap: Applicative[Validation[E, ?]]) extends Validate[(A1, A2), E, (B1, B2)] {
  def run(x: (A1, A2)) = ap.tuple2(left.run(x._1), right.run(x._2))
}

final case class ComposeValidate[-A, +E, B, +C](start: Validate[A, E, B], continue: Validate[B, E, C]) extends Validate[A, E, C] {
  def run(x: A) = start.run(x) match {
    case Success(y) => continue.run(y)
    case fail@Failure(_) => fail
  }

  override def andThen[D, EE >: E](v: Validate[C, EE, D]): Validate[A, EE, D] = ComposeValidate(start, continue andThen v)
}

object Validate {
  def instance[E](implicit ap: Applicative[Validation[E, ?]]): Arrow[Validate[?, E, ?]] = new ValidateInstance[E]
}

final class ValidateInstance[E](implicit ap: Applicative[Validation[E, ?]]) extends Arrow[Validate[?, E, ?]] {
  def arr[A, B](f: (A) => B): Validate[A, E, B] = SuccessValidate(f)

  def id[A]: Validate[A, E, A] = SuccessValidate(identity)

  def compose[A, B, C](f: Validate[B, E, C], g: Validate[A, E, B]): Validate[A, E, C] = g andThen f

  def first[A, B, C](fa: Validate[A, E, B]): Validate[(A, C), E, (B, C)] = SplitValidate(fa, id[C])

  override def second[A, B, C](f: Validate[A, E, B]): Validate[(C, A), E, (C, B)] = SplitValidate(id, f)

  override def split[A, B, C, D](f: Validate[A, E, B], g: Validate[C, E, D]): Validate[(A, C), E, (B, D)] = SplitValidate(f, g)

  override def mapfst[A, B, C](fab: Validate[A, E, B])(f: (C) => A): Validate[C, E, B] = fab match {
    case SuccessValidate(g) => SuccessValidate(f andThen g)
    case SimpleValidate(g) => SimpleValidate(f andThen g)
    case ComposeValidate(g, h) => ComposeValidate(mapfst(g)(f), h)
    case _ => super.mapfst(fab)(f)
  }

  override def mapsnd[A, B, C](fab: Validate[A, E, B])(f: (B) => C): Validate[A, E, C] = fab match {
    case SuccessValidate(g) => SuccessValidate(g andThen f)
    case ComposeValidate(g, h) => ComposeValidate(g, mapsnd(h)(f))
    case _ => super.mapsnd(fab)(f)
  }
}
