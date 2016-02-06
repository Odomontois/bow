package bow

import scala.language.higherKinds
import scalaz.{\/, Arrow}

/**
  * User: Oleg
  * Date: 06-Feb-16
  * Time: 14:30
  */
trait ArrowChoice[=>:[_, _]] extends Arrow[=>:] {
  /** Feed marked inputs through the argument arrow, passing the rest through unchanged to the output. */
  def left[A, B, C](fa: A =>: B): (A \/ C) =>: (B \/ C)

  /** A mirror image of left. */
  def right[A, B, C](fa: A =>: B): (C \/ A) =>: (C \/ B) =
    dimap[A \/ C, B \/ C, C \/ A, C \/ B](left(fa))(_.swap)(_.swap)

  /** Split the input between the two argument arrows, retagging and merging their outputs. */
  def choose[A1, A2, B1, B2](fa: A1 =>: B1)(fb: A2 =>: B2): (A1 \/ A2) =>: (B1 \/ B2) = compose(left(fa), right(fb))

  /** Split the input between the two argument arrows and merge their outputs. */
  def fanin[A, B, C](fa: A =>: C)(fb: B =>: C): (A \/ B) =>: C = mapsnd(choose(fa)(fb))(_.fold(identity, identity))
}