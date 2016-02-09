package bow

import scala.language.higherKinds
import scalaz.{Arrow, -\/, \/, \/-}
import scalaz.syntax.arrow._
import scalaz.syntax.applicative._
import bow.functions._

/**
  * User: Oleg
  * Date: 08-Feb-16
  * Time: 19:15
  */
final case class ZipStreamT[=>:[_, _], A, B](run: Stream[A] =>: Stream[B])
/*extends AnyVal*/

object ZipStreamT {
  def instance[=>:[_, _] : Arrow] = new ZipStreamTInstance[=>:]
}


final class ZipStreamTInstance[=>:[_, _]](implicit A: Arrow[=>:])
  extends ArrowChoice[ZipStreamT[=>:, ?, ?]]
    with ArrowPlus[ZipStreamT[=>:, ?, ?]] with EitherComposeFunctions {
  type =||>[a, b] = ZipStreamT[=>:, a, b]


  def arr[A, B](f: (A) => B): A =||> B = ZipStreamT(A.arr(_.map(f)))

  def id[A]: A =||> A = ZipStreamT(A.id)

  def zero[A, B]: A =||> B = ZipStreamT(constA[Stream[A], =>:](Stream.empty[B]))

  def plus[A, B](fa: A =||> B, fb: A =||> B): A =||> B = ZipStreamT((fa.run &&& fb.run) >>^ concat)

  def compose[A, B, C](f: B =||> C, g: A =||> B): A =||> C = ZipStreamT(f.run <<< g.run)

  def first[A, B, C](fa: A =||> B): (A, C) =||> (B, C) = ZipStreamT((fa.run.first[Stream[C]] ^>> unzip[A, C]) >>^ zip)

  override def second[A, B, C](f: A =||> B): (C, A) =||> (C, B) = ZipStreamT((f.run.second[Stream[C]] ^>> unzip[C, A]) >>^ zip)

  override def split[A, B, C, D](f: A =||> B, g: C =||> D): (A, C) =||> (B, D) =
    ZipStreamT(((f.run *** g.run) ^>> unzip[A, C]) >>^ zip)


  def concat[A]: ((Stream[A], Stream[A])) => Stream[A] = { case (a, b) => a append b }

  def zip[A, B]: ((Stream[A], Stream[B])) => Stream[(A, B)] = { case (a, b) => a zip b }

  def unzip[A, B]: Stream[(A, B)] => (Stream[A], Stream[B]) = { s => (s.map(_._1), s.map(_._2)) }

  /** Feed marked inputs through the argument arrow, passing the rest through unchanged to the output. */
  def left[A, B, C](fa: A =||> B): (A \/ C) =||> (B \/ C) = {
    val ls = fa.run <<< A.arr((_: Stream[A \/ C]).collect({ case -\/(a) => a }))
    val full = (A.id[Stream[A \/ C]] &&& ls) >>> A.arr((composeL[A, B, C] _).tupled)
    ZipStreamT(full)
  }

  /** A mirror image of left. */
  override def right[A, B, C](fa: A =||> B): (C \/ A) =||> (C \/ B) = {
    val rs = fa.run <<< A.arr((_: Stream[C \/ A]).collect({ case \/-(a) => a }))
    val full = (A.id[Stream[C \/ A]] &&& rs) >>> A.arr((composeR[C, B, A] _).tupled)
    ZipStreamT(full)
  }

  /** Split the input between the two argument arrows, retagging and merging their outputs. */
  override def choose[A1, A2, B1, B2](fa: A1 =||> B1)(fb: A2 =||> B2): (A1 \/ A2) =||> (B1 \/ B2) = {
    val ls = fa.run <<< A.arr((_: Stream[A1 \/ A2]).collect({ case -\/(a) => a }))
    val rs = fb.run <<< A.arr((_: Stream[A1 \/ A2]).collect({ case \/-(a) => a }))
    val full = (A.id[Stream[A1 \/ A2]] &&& ls &&& rs) >>>
      A.arr { case ((a, b), c) => (a, b, c) } >>>
      A.arr((composeBoth[A1, A2, B1, B2] _).tupled)
    ZipStreamT(full)
  }
}

trait EitherComposeFunctions {
  def composeL[A, B, C](src: Stream[A \/ C], ls: Stream[B]): Stream[B \/ C] = src match {
    case -\/(_) #:: src1 =>
      ls match {
        case b #:: ls1 => -\/(b) #:: composeL(src1, ls1)
        case _ => Stream.empty
      }
    case (r@ \/-(_)) #:: src1 => r #:: composeL(src1, ls)
  }

  def composeR[A, B, C](src: Stream[A \/ C], rs: Stream[B]): Stream[A \/ B] = src match {
    case \/-(_) #:: src1 =>
      rs match {
        case b #:: rs1 => \/-(b) #:: composeR(src1, rs1)
        case _ => Stream.empty
      }
    case (l@ -\/(_)) #:: src1 => l #:: composeR(src1, rs)
  }

  def composeBoth[A, B, C, D](src: Stream[A \/ B], ls: Stream[C], rs: Stream[D]): Stream[C \/ D] = src match {
    case -\/(_) #:: src1 =>
      ls match {
        case b #:: ls1 => -\/(b) #:: composeBoth(src1, ls1, rs)
        case _ => Stream.empty
      }
    case \/-(_) #:: src1 =>
      rs match {
        case d #:: rs1 => \/-(d) #:: composeBoth(src1, ls, rs1)
        case _ => Stream.empty
      }
  }
}
