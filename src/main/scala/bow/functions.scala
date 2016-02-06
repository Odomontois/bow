package bow

import scala.language.higherKinds
import scalaz.Arrow
import scalaz.syntax.arrow._

/**
  * User: Oleg
  * Date: 06-Feb-16
  * Time: 15:50
  */
object functions {
  def arr[A, B, =>:[_, _]](f: A => B)(implicit ar: Arrow[=>:]): A =>: B = ar.arr(f)

  def constA[A, B, =>:[_, _]](x: => B)(implicit ar: Arrow[=>:]): A =>: B = ar.arr(_ => x)
}
