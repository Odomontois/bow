package bow

import scala.language.higherKinds
import scalaz.Arrow

/**
  * User: Oleg
  * Date: 09-Feb-16
  * Time: 11:44
  */
trait ArrowZero[=>:[_, _]] extends Arrow[=>:] {
  def zero[A, B]: A =>: B
}

trait ArrowPlus[=>:[_, _]] extends ArrowZero[=>:] {
  def plus[A, B](fa: A =>: B, fb: A =>: B): A =>: B
}


