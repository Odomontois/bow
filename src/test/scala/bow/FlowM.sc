import bow.flow.FlowM

import scalaz.Id._
import scalaz.StreamT
import scalaz.std.stream._


val Ints = FlowM.Make[Id, Unit, Int, Int]

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

def findDivs(n: Int): Stream[Int] =
  divisors(n).feed(StreamT.fromStream[Id, Int](primes)).toStream

findDivs(123123000)

