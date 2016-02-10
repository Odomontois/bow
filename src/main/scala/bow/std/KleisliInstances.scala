package bow.std

import bow.ArrowChoice

import scala.language.higherKinds
import scalaz.{Kleisli, Monad, \/}
import scalaz.syntax.monad._

/**
  * User: Oleg
  * Date: 10-Feb-16
  * Time: 15:33
  */

trait KleisliInstances {
  implicit def kleisliArrowChoice[M[_]](implicit M: Monad[M]) = new ArrowChoice[Kleisli[M, ?, ?]] {
    /** Feed marked inputs through the argument arrow, passing the rest through unchanged to the output. */
    type =>>[a, b] = Kleisli[M, a, b]


    def arr[A, B](f: (A) => B): A =>> B = Kleisli((x: A) => M.point(f(x)))

    def id[A]: A =>> A = Kleisli(M.point(_: A))

    def first[A, B, C](fa: A =>> B): (A, C) =>> (B, C) =
      Kleisli { case (a, c) => fa.run(a).map((_, c)) }

    override def second[A, B, C](f: A =>> B): (C, A) =>> (C, B) =
      Kleisli { case (c, a) => f.run(a).map((c, _)) }

    override def split[A, B, C, D](f: A =>> B, g: C =>> D): (A, C) =>> (B, D) =
      Kleisli { case (a, b) => for {
        c <- f run a
        d <- g run b
      } yield (c, d)
      }

    override def combine[A, B, C](fab: A =>> B, fac: A =>> C): A =>> (B, C) = Kleisli { a => for {
      b <- fab run a
      c <- fac run a
    } yield (b, c)
    }


    def left[A, B, C](fa: A =>> B): (A \/ C) =>> (B \/ C) = Kleisli {_.swap.traverse(fa.run).map(_.swap)}


    /** A mirror image of left. */
    override def right[A, B, C](fa: A =>> B): (C \/ A) =>> (C \/ B) = Kleisli {_.traverse(fa.run)}

    /** Split the input between the two argument arrows, retagging and merging their outputs. */
    override def choose[A1, A2, B1, B2](fa: A1 =>> B1)(fb: A2 =>> B2): (A1 \/ A2) =>> (B1 \/ B2) =
      Kleisli { x => for {
        b <- x traverse fb.run
        a <- b.swap traverse fa.run
      } yield a.swap
      }


    /** Same as choose but lazy on right argument */
    override def chooseLz[A1, A2, B1, B2](fa: => A1 =>> B1 )(fb: => A2 =>> B2): (A1 \/ A2) =>> (B1 \/ B2) =
      Kleisli { x => for {
        b <- x traverse fb.run
        a <- b.swap traverse fa.run
      } yield a.swap
      }

    /** Split the input between the two argument arrows and merge their outputs. */
    override def fanin[A, B, C](fa: A =>> C)(fb: B =>> C): (A \/ B) =>> C = Kleisli { x => for {
      b <- x traverse fb.run
      a <- b.swap traverse fa.run
    } yield a.merge
    }

    def compose[A, B, C](f: B =>> C, g: A =>> B): A =>> C = Kleisli {g.run(_) >>= f.run}

  }
}
