package bow

import scalaz.Arrow

/**
  * User: Oleg
  * Date: 07-Feb-16
  * Time: 20:13
  */
package object functions {
  def arr[=>:[_, _]]= new ArrBuilder[=>:]

  def constA[A, =>:[_, _]] = new ConstABuilder[=>:, A]
}

class ArrBuilder[=>:[_, _]]{
  def apply[A, B](f: A => B)(implicit A: Arrow[=>:])  = A.arr(f)
}

class ConstABuilder[=>:[_, _], A]{
  def apply[B](x: => B)(implicit A: ArrowChoice[=>:]): A =>: B = A.constA(x)
}
