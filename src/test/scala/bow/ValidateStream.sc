import bow.{Aspect, ZipStreamT, ArrowChoice, ValidateT}
import bow.std._
import bow.functions._
import scalaz.syntax.validation._
import bow.syntax._
import scalaz.NonEmptyList
import bow.ValidateT.instance
import scalaz.syntax.arrow._

type =!>[a, b] = ValidateT[Function1, a, NonEmptyList[(Int, String)], b]
implicitly [ArrowChoice[=!>]]
def divisor(n: Int): Int =!> Boolean = arr[=!>](_ % n == 0)
def err[A](s: String): Int =!> A = ValidateT.make(x => (x,s).failureNel[A])

val div3 = divisor(3) ifTrue err("division by 3 is not good")

val div5 = divisor(5) ifTrue err("division by 5 is bad")

val div15 = divisor(15) ifTrue err("division by 15 is pure evil")

val divAll = div15 >>> div5 >>> div3

//1 to 50 foreach (divAll.run andThen println)
val divStream = Aspect[ZipStreamT].lift(divAll)

divStream.run.run(Stream(1,2,3))

