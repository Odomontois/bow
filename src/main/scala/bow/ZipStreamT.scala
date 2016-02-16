package bow

import scala.language.higherKinds
import scalaz._
import scalaz.syntax.arrow._
import bow.functions._
import bow.std._
import bow.syntax._

/**
  * User: Oleg
  * Date: 08-Feb-16
  * Time: 19:15
  */
final class ZipStreamT[=>:[_, _], A, B](f: => Stream[A] =>: Stream[B]) {
  lazy val run = f
}

object ZipStreamT {
  def apply[=>:[_, _], A, B](f: => Stream[A] =>: Stream[B]) = new ZipStreamT(f)

  implicit def instance[=>:[_, _] : ArrowChoice] = new ZipStreamTInstance[=>:]

  implicit val aspect = ZipStreamAspect
}


final class ZipStreamTInstance[=>:[_, _]](implicit A: ArrowChoice[=>:])
  extends ArrowChoice[ZipStreamT[=>:, ?, ?]]
    with ArrowPlus[ZipStreamT[=>:, ?, ?]] with EitherComposeFunctions {
  type =||>[a, b] = ZipStreamT[=>:, a, b]


  def arr[A, B](f: (A) => B): A =||> B = ZipStreamT(A.arr(_.map(f)))

  override def id[A]: A =||> A = ZipStreamT(A.id)

  def zero[A, B]: A =||> B = ZipStreamT(A.constA[Stream[A], Stream[B]](Stream.empty[B]))

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
    val ls = fa.run ^>> ((_: Stream[A \/ C]).collect({ case -\/(a) => a }))
    val full = (A.id[Stream[A \/ C]] &&& ls) >>^ (composeL[A, B, C] _).tupled
    ZipStreamT(full)
  }


  /** A mirror image of left. */
  override def right[A, B, C](fa: A =||> B): (C \/ A) =||> (C \/ B) = {
    def rs = fa.run ^>> ((_: Stream[C \/ A]).collect({ case \/-(a) => a }))
    def full = (A.id[Stream[C \/ A]] &&& rs) >>^ (composeR[C, B, A] _).tupled
    ZipStreamT(full)
  }

  /** Split the input between the two argument arrows, retagging and merging their outputs. */
  override def choose[A1, A2, B1, B2](fa: => A1 =||> B1)(fb: => A2 =||> B2): (A1 \/ A2) =||> (B1 \/ B2) = {
    val ls = fa.run ^>> ((_: Stream[A1 \/ A2]).collect({ case -\/(a) => a }))
    val rs = fb.run ^>> ((_: Stream[A1 \/ A2]).collect({ case \/-(a) => a }))
    val full = (A.id[Stream[A1 \/ A2]] &&& ls &&& rs) >>^ { case ((a, b), c) => composeBoth(a, b, c) }
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

object ZipStreamAspect extends Aspect[ZipStreamT] {
  def liftClass[A[_, _]](ar: ArrowChoice[A]): ArrowChoice[ZipStreamT[A, ?, ?]] = new ZipStreamTInstance[A]()(ar)

  def tmap[A1[_, _], A2[_, _]](f: A1 ~~> A2)
                              (implicit ar1: ArrowChoice[A1], ar2: ArrowChoice[A2]) =
    new (ZipStreamT[A1, ?, ?] ~~> ZipStreamT[A2, ?, ?]) {
      def apply[b, c](stream: ZipStreamT[A1, b, c]) = ZipStreamT(f(stream.run))
    }

  def lift[A[_, _], b, c](x: A[b, c])(implicit ar: ArrowChoice[A]): ZipStreamT[A, b, c] =
    ZipStreamT(x.project[Stream])
}
