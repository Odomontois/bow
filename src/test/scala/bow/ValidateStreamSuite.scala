package bow

import bow.ValidateT.instance
import bow.functions._
import bow.std._
import bow.sz._
import bow.syntax._
import scala.io.{StdIn, Source}
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

    type =!>[a, b] = ValidateT[Arr, a, Err, b]

    def divisor(n: Int): Int =!> Boolean = arr[=!>](_ % n == 0)

    def err[A, B](s: A => String): A =!> B = ValidateT.make(arr[Arr](x => s(x).failureNel))

    def divErr(n: Int, mess: String): Int =!> Int = divisor(n) ifTrue err(num => s"number $num : $mess")

    val divCheck = divErr(3, "FIZZ division by 3 is not good") >>>
      divErr(5, "BUZZ division by 5 is bad") >>>
      divErr(15, "FIZZBUZZ division by 15 is pure evil")

    //1 to 50 foreach (divAll.run andThen println)
    val divStream = Aspect[ZipStreamT].lift[=!>, Int, Int](divCheck)

    divStream.run.run >>^ (_.disjunction.bimap(_.stream, _ map (_.toString)).merge)
  }

  //
  //  def remainders(r: Int): ZipStreamT[Arr, Int, Int]  = {
  //
  //  }
}

object ValidateStreamSuite /*extends ValidateStreamTrampSuite[Kleisli[Trampoline, ?, ?]]*/ {
  type Arr[a, b] = Kleisli[Trampoline, a, b]
  implicitly[Monad[Trampoline]]
  implicitly[ArrowChoice[Arr]]

  def main(args: Array[String]) {
//    StdIn.readLine()
    val begin = System.currentTimeMillis()
    new ValidateStreamSuite[Arr].go.run(Stream.range(1, 100000)).run foreach println
    println(s"${System.currentTimeMillis() - begin} ms")
  }
}
