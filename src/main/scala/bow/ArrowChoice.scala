package bow

import scala.language.higherKinds
import scalaz._
import scalaz.syntax.arrow._
import bow.syntax._

/**
  * User: Oleg
  * Date: 06-Feb-16
  * Time: 14:30
  */
trait ArrowChoice[=>:[_, _]] extends Arrow[=>:] with ProChoice[=>:] {
  private implicit def _arr = this

  /** Feed marked inputs through the argument arrow, passing the rest through unchanged to the output. */
  def left[A, B, C](fa: A =>: B): (A \/ C) =>: (B \/ C)

  def mkLazy[A, B](fa: => A =>: B): A =>: B = fa

  /** A mirror image of left. */
  def right[A, B, C](fa: A =>: B): (C \/ A) =>: (C \/ B) =
    dimap[A \/ C, B \/ C, C \/ A, C \/ B](left(fa))(_.swap)(_.swap)

  /** Split the input between the two argument arrows, retagging and merging their outputs. */
  def choose[A1, A2, B1, B2](fa: => A1 =>: B1)(fb: => A2 =>: B2): (A1 \/ A2) =>: (B1 \/ B2) = compose(left(fa), right(fb))

  /** Split the input between the two argument arrows and merge their outputs. */
  def fanin[A, B, C](fa: => A =>: C)(fb: => B =>: C): (A \/ B) =>: C = mapsnd(choose(fa)(fb))(_.fold(identity, identity))

  private def toEither[A]: ((Boolean, A)) => A \/ A = { case (cond, x) => if (cond) \/-(x) else -\/(x) }

  def ifThenElse[A, B](fCond: A =>: Boolean)(fThen: => A =>: B, fElse: => A =>: B): A =>: B =
    (fCond &&& id) >>^ toEither >>> (fElse ||| fThen)

  def constA[A, B](x: => B): A =>: B = arr(_ => x)

  def id[A] = arr(identity[A])
}
