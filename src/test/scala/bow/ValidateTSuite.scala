package bow

import scalaz.{NonEmptyList, ValidationNel, Validation}
import scalaz.syntax.arrow._
import scalaz.syntax.validation._
import bow.std._
import bow.functions._
/**
  * User: Oleg
  * Date: 07-Feb-16
  * Time: 19:59
  */
object ValidateTSuite extends App{
  type VT[a,b] = ValidateT[Function1,a,NonEmptyList[String],b]


  val wrap: VT[ValidationNel[String, Int], Int] = ValidateT.wrap

  val ae =  "bad a".failureNel[Int]
  val be =  "bad b".failureNel[Int]

  val as = 1.successNel[String]
  val bs = 3.successNel[String]

  val process = (wrap *** wrap) >>> arr[VT]{case (x, y) => x + y}

  for{a <- List(as, ae); b <- List(bs, be)} println(process.run(a,b))


}
