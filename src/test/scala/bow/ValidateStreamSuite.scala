package bow

import bow.ValidateT.instance
import bow.functions._
import bow.std._
import bow.syntax._
import scala.language.higherKinds
import scalaz.Free.Trampoline
import scalaz.{Monad, Validation, Kleisli, NonEmptyList}
import scalaz.syntax.validation._
import scalaz.syntax.arrow._
import scalaz.syntax.monad._

/**
  * User: Oleg
  * Date: 09-Feb-16
  * Time: 23:53
  */

class ValidateStreamSuite[Arr[_, _]](implicit A: ArrowChoice[Arr]) {
  def go: Arr[Stream[Int], Stream[String]] = {
    type Err = NonEmptyList[String]

    type V[A] = Validation[Err, A]

    type =!>[a, b] = ValidateT[Arr, a, Err, b]

    implicitly[ArrowChoice[=!>]]

    def divisor(n: Int): Int =!> Boolean = arr[=!>](_ % n == 0)

    def err[A, B](s: A => String): A =!> B = ValidateT.make[Arr](A.arr(x => s(x).failureNel))

    def divErr(n: Int, mess: String): Int =!> Int = divisor(n) ifTrue err(num => s"number $num : $mess")

    val div3 = divErr(3, "division by 3 is not good")

    val div5 = divErr(5, "division by 5 is bad")

    val div15 = divErr(15, "division by 15 is pure evil")

    val divAll: Int =!> Int = div15 >>> div5 >>> div3

    //1 to 50 foreach (divAll.run andThen println)
    val divStream = Aspect[ZipStreamT].lift[=!>, Int, Int](divAll)

    divStream.run.run >>^ (_.disjunction.bimap(_.stream, _ map (_.toString)).merge)
  }
}

object ValidateStreamSuite /*extends ValidateStreamTrampSuite[Kleisli[Trampoline, ?, ?]]*/ {
  type Arr[a, b] = Kleisli[Trampoline, a, b]
  implicitly[Monad[Trampoline]]
  implicitly[ArrowChoice[Arr]]

  def main(args: Array[String]) {
    new ValidateStreamSuite[Function1] go Stream.range(1, 1000) foreach println
    new ValidateStreamSuite[Arr].go.run(Stream.range(1,10000)).run foreach println
  }
}
