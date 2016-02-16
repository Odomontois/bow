package bow.sz

import bow.ArrowChoice
import bow.functions.LazyFunc

import scala.language.higherKinds
import scalaz.syntax.monad._
import scalaz.{Kleisli, Monad, \/}

/**
  * User: Oleg
  * Date: 10-Feb-16
  * Time: 15:33
  */

trait KleisliInstances {
  implicit def kleisliArrowChoice[M[_]](implicit M: Monad[M]) = new ArrowChoice[Kleisli[M, ?, ?]] {
    /** Feed marked inputs through the argument arrow, passing the rest through unchanged to the output. */
    type ~>>[a, b] = Kleisli[M, a, b]

    @inline def ~>>[a, b](f: a => M[b]) = Kleisli[M, a, b](f)

    def arr[A, B](f: (A) => B): A ~>> B = ~>>((x: A) => M.point(f(x)))

    override def id[A]: A ~>> A = ~>>(M.point(_: A))

    def first[A, B, C](fa: A ~>> B): (A, C) ~>> (B, C) =
      ~>> { case (a, c) => fa.run(a).map((_, c)) }

    override def second[A, B, C](f: A ~>> B): (C, A) ~>> (C, B) =
      ~>> { case (c, a) => f.run(a).map((c, _)) }

    override def split[A, B, C, D](f: A ~>> B, g: C ~>> D): (A, C) ~>> (B, D) =
      ~>> { case (a, b) => for {
        c <- f run a
        d <- g run b
      } yield (c, d)
      }

    override def combine[A, B, C](fab: A ~>> B, fac: A ~>> C): A ~>> (B, C) = ~>> { a => for {
      b <- fab run a
      c <- fac run a
    } yield (b, c)
    }

    def left[A, B, C](fa: A ~>> B): (A \/ C) ~>> (B \/ C) = ~>> {_.swap.traverse(fa.run).map(_.swap)}


    override def mkLazy[A, B](fa: => A ~>> B): A ~>> B = ~>>(new LazyFunc(fa.run))

    /** A mirror image of left. */
    override def right[A, B, C](fa: A ~>> B): (C \/ A) ~>> (C \/ B) = ~>>{_.traverse(fa.run)}

    /** Split the input between the two argument arrows, retagging and merging their outputs. */
    override def choose[A1, A2, B1, B2](fa: => A1 ~>> B1)(fb: => A2 ~>> B2): (A1 \/ A2) ~>> (B1 \/ B2) =
      ~>> { x => for {
        b <- x traverse fb.run
        a <- b.swap traverse fa.run
      } yield a.swap
      }

    /** Split the input between the two argument arrows and merge their outputs. */
    override def fanin[A, B, C](fa: => A ~>> C)(fb: => B ~>> C): (A \/ B) ~>> C = ~>> { x => for {
      b <- x traverse fb.run
      a <- b.swap traverse fa.run
    } yield a.merge
    }

    def compose[A, B, C](f: B ~>> C, g: A ~>> B): A ~>> C = ~>> {g.run(_) >>= f.run}

  }
}
