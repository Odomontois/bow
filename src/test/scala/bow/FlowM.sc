import bow.flow.FlowM

import scalaz.Free.Trampoline
import scalaz.Id._
import scalaz.{Monad, StreamT}
import scalaz.std.stream._
import scalaz.std.anyVal._
import scalaz.std.function._
import scalaz.syntax.monad._

val Ints = FlowM.Make[Id, Unit, Int, Int]
val IntsT = FlowM.Make[Function0, Unit, Int, Int]

def divgo(n: Int, d: Int): Ints.Flow =
  if (n % d == 0) Ints.Output(d, divgo(n / d, d))
  else divisors(n)

def divisors(n: Int): Ints.Flow =
  if (n == 1) Ints.End(Stream())
  else Ints.Input {
    case Some(d) => divgo(n, d)
    case None => Ints.End(Stream())
  }

val primes: Stream[Int] = 2 #:: {
  def isPrime(n: Int) = primes.takeWhile(p => p * p <= n).forall(p => n % p != 0)
  Stream.from(3, 2).filter(isPrime)
}

def range[F[_] : Monad](start: Int, end: Int, step: Int = 1): FlowM[F, Int, Unit, Int] =
  if (start >= end) FlowM.End(0)
  else FlowM.Output(start, range[F](start + step, end, step).point[F])

def time[R](block: => R): R = {
  val t0 = System.nanoTime()
  val result = block // call-by-name
  val t1 = System.nanoTime()
  println("Elapsed time: " + (t1 - t0) + "ns")
  result
}

time(range[Function0](1, 1000000).mapResult(_ => 0).collectFree(identity, StreamT.empty).run)
time(range[Id](1, 1000000).mapSumRec(identity, _ => 0, ()))
time(range[Id](1, 1000000).sumRec)
time (1 to 1000000 sum)
time(range[Id](1, 1000000).mapSumRec(identity, _ => 0, ()))
time(range[Id](1, 1000000).sumRec)
time (1 to 1000000 sum)




