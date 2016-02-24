import bow.flow.Flow

val u = Flow.fromStream(Stream(1,2,3))

val v = u.flatMap(x => Flow.fromStream(Stream(x, x*10, x*100)))

v.feed(Stream()).toList

val a = Flow.map[Int,Int](_ * 2)
val b = Flow.map[Int, Int](_ + 1)

(a andThen b).feed(Stream.range(1,200000)).sum