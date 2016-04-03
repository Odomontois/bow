import bow.flow.Flow
import Flow.{Input, Output, End}

val u = Flow.fromStream(Stream(1, 2, 3))

val v = u.flatMap(x => Flow.fromStream(Stream(x, x * 10, x * 100)))

v.feed(Stream()).toList

val a = Flow.map[Int, Int](_ * 2)
val b = Flow.map[Int, Int](_ + 1)

(a andThen b).feed(Stream(1, 2, 3)).toList
(a andThen b).feed(Stream.range(1, 2000)).sum

val adds = Flow.Id[Int].flatMap(x => Flow.fromStream(Stream(x, x + 1)))
val muls = Flow.Id[Int].flatMap(x => Flow.fromStream(Stream(x, x * 2)))

Flow.Id.input(1, 2, 3).toList
Flow.Id[Int].flatMap(x => Flow.stream(2, 3)).input(1, 2).toList



(for {
  x <- Flow.Id[Int]
  y = x * 2
  z <- Flow.fromStream(Stream.fill(x)(y - 3))
} yield z).feed(Stream.range(1, 10)).sum

val collatz = {
  def output(n: Int): Flow[Int, Int] = Output(n,
    if (n == 1) input
    else if (n % 2 == 0) output(n / 2)
    else output(n * 3 + 1)
  )

  def input: Flow[Int, Int] = Input{
    case Some(x) => output(x)
    case None => End()
  }

  input
}

collatz.input(2, 3, 4).toList

