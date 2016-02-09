package bow

import scala.language.higherKinds
import scalaz.{~~>, BiNaturalTransformation, NaturalTransformation, Arrow}

/** An ArrowChoice transformer */
trait Aspect[T[=>:[_, _], _, _]] {
  def liftClass[A[_, _]](ar: ArrowChoice[A]): ArrowChoice[T[A, ?, ?]]
  def lift[A[_, _], b, c](x: A[b, c])(implicit ar: ArrowChoice[A]): T[A, b, c]
  def tmap[A1[_, _], A2[_, _]](f: A1 ~~> A2)(implicit ar1: ArrowChoice[A1], ar2: ArrowChoice[A2]): T[A1, ?, ?] ~~> T[A2, ?, ?]
}
