package bow

import scala.language.higherKinds
import scalaz.{Monad, Arrow}
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

final class ArrowApplyMonad[A, =>:[_, _]](implicit aap: ArrowApply[=>:]) extends Monad[A =>: ?] {

  import aap.{id, arr, app}

  def point[B](a: => B): A =>: B = arr(_ => a)

  def bind[B, C](fa: A =>: B)(f: B => (A =>: C)): A =>: C = (fa >>> arr(f) &&& id) >>> app
}
