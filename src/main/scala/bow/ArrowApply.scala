package bow

import scala.language.higherKinds
import scalaz.{MonadReader, Monad, Arrow}
import scalaz.syntax.arrow._
import functions._

/**
  * User: Oleg
  * Date: 06-Feb-16
  * Time: 15:16
  */
/** Some arrows allow application of arrow inputs to other inputs
  * Such arrows are equivalent to monads
  */

trait ArrowApply[=>:[_, _]] extends Arrow[=>:] {
  def app[A, B]: (A =>: B, A) =>: B
}


object ArrowApply {
  def monadInstance[A, =>:[_, _]](implicit aap: ArrowApply[=>:]) = new ArrowApplyMonad
}

final class ArrowApplyMonad[X, =>:[_, _]](implicit aap: ArrowApply[=>:]) extends MonadReader[X =>: ?, X] {

  import aap.{id, arr, app}

  def point[B](a: => B): X =>: B = arr(_ => a)

  def bind[B, C](fa: X =>: B)(f: B => (X =>: C)): X =>: C = (fa >>> arr(f) &&& id) >>> app

  def ask: X =>: X = id

  def local[A](f: X => X)(fa: X =>: A): X =>: A = arr(f) >>> fa
}
