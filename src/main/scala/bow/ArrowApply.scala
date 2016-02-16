package bow

import scala.language.higherKinds
import scalaz._
import scalaz.syntax.arrow._
import scalaz.syntax.either._
import functions._

/**
  * User: Oleg
  * Date: 06-Feb-16
  * Time: 15:16
  */
/** Some arrows allow application of arrow inputs to other inputs
  * Such arrows are equivalent to monads
  */

trait ArrowApply[=>:[_, _]] extends Arrow[=>:] with ArrowChoice[=>:] {
  def app[A, B]: (A =>: B, A) =>: B

  def join[A]: (Unit =>: A) =>: A = mapfst(app[Unit, A]) {(_, ())}

  def left[A, B, C](f: A =>: B): (A \/ C) =>: (B \/ C) =
    mapfst(join[B \/ C]) {
      case -\/(a) => mapsnd(mapfst(f)({ (_: Unit) => a }))(_.left[C])
      case \/-(c) => arr { (_: Unit) => c.right[B] }
    }
  /** A mirror image of left. */
  override def right[A, B, C](f: A =>: B): (C \/ A) =>: (C \/ B) =
    mapfst(join[C \/ B]) {
      case -\/(c) => arr { (_: Unit) => c.left[B] }
      case \/-(a) => mapsnd(mapfst(f)({ (_: Unit) => a }))(_.right[C])
    }

  /** Split the input between the two argument arrows, retagging and merging their outputs. */
  override def choose[A1, A2, B1, B2](fa: => A1 =>: B1)(fb: => A2 =>: B2): (A1 \/ A2) =>: (B1 \/ B2) =
    mapfst(join[B1 \/ B2]) {
      case -\/(a1) => mapsnd(mapfst(fa)({ (_: Unit) => a1 }))(_.left[B2])
      case \/-(a2) => mapsnd(mapfst(fb)({ (_: Unit) => a2 }))(_.right[B1])
    }
}


object ArrowApply {
  def monadInstance[A, =>:[_, _]](implicit aap: ArrowApply[=>:]) = new ArrowApplyMonad
}

final class ArrowApplyMonad[X, =>:[_, _]](implicit aap: ArrowApply[=>:]) extends MonadReader[X =>: ?, X] {

  import aap.{id, arr, app}

  def point[B](a: => B): X =>: B = arr(_ => a)

  def bind[B, C](fa: X =>: B)(f: B => (X =>: C)): X =>: C = (fa >>^ f &&& id) >>> app

  override def map[A, B](fa: X =>: A)(f: A => B): X =>: B = fa >>^ f

  override def apply2[A, B, C](fa: => X =>: A, fb: => X =>: B)(f: (A, B) => C): X =>: C = (fa &&& fb) >>^ f.tupled

  override def ap[A, B](fa: => X =>: A)(f: => X =>: (A => B)): X =>: B = apply2(f, fa)(_ (_))

  def ask: X =>: X = id

  def local[A](f: X => X)(fa: X =>: A): X =>: A = fa ^>> f
}
