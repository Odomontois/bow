package bow

import bow.ValidateT.instance
import bow.functions._
import bow.std._
import bow.syntax._
import scalaz.Free.Trampoline
import scalaz.{Validation, Kleisli, NonEmptyList}
import scalaz.syntax.validation._
import scalaz.syntax.arrow._
import scalaz.syntax.monad._

/**
  * User: Oleg
  * Date: 09-Feb-16
  * Time: 23:53
  */
object ValidateStreamTrampSuite extends App {
  type TrampArr[a, b] = Kleisli[Trampoline, a, b]

  type Err = NonEmptyList[(Int, String)]

  type V[A] = Validation[Err, A]

  type =!>[a, b] = ValidateT[TrampArr, a, Err, b]

  implicitly[ArrowChoice[=!>]]
  val A = implicitly[ArrowChoice[TrampArr]]

  def divisor(n: Int): Int =!> Boolean = arr[=!>](_ % n == 0)

  def err[A](s: String): Int =!> A = ValidateT.make[TrampArr](Kleisli[Trampoline, Int, V[A]]((x: Int) => (x, s).failureNel[A].point[Trampoline]))

  def divErr(n: Int, mess: String): Int =!> Int = divisor(n) ifTrue err(mess)

  val div3 = divErr(3, "division by 3 is not good")

  val div5 = divErr(5, "division by 5 is bad")

  val div15 = divErr(15, "division by 15 is pure evil")

  val divAll: Int =!> Int = div15 >>> div5 >>> div3

  //1 to 50 foreach (divAll.run andThen println)
  val divStream = Aspect[ZipStreamT].lift[=!>, Int, Int](divAll)

  divStream.run.run(Stream.range(1, 100000)).run bimap(_ foreach println, _ foreach println)
}
