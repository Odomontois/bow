package bow.functions

/**
  * User: Oleg
  * Date: 14-Feb-16
  * Time: 01:00
  */
class LazyFunc[A, B](f: => A => B) extends (A => B) {
  lazy val _run = f

  def apply(x: A) = _run(x)
}
