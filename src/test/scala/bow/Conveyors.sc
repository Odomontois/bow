import bow.flow.Flow

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

val zz: Flow[Int, Int] = Flow.Id[Int].map( x => (x * 2, x)).flatMap{ case (y, x) => Flow.fromStream[Int, Int](Stream.fill(x)(y - 3)) }



//  for {
//  x <- Flow.Id[Int]
//  y = x * 2
//  z <- Flow.fromStream(Stream.fill(x)(y - 3))
//} yield z)