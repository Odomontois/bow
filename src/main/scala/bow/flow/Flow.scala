package bow.flow

import java.util.NoSuchElementException

/**
  * User: Oleg
  * Date: 24-Feb-16
  * Time: 18:18
  */
sealed trait Flow[-A, +B] {
  self =>

  import Flow._

  def fold[T](end: => T,
              input: (Option[A] => Flow[A, B]) => T,
              output: (B, Flow[A, B]) => T): T

  def feed(source: Stream[A]): Stream[B] = feedMaybes(source).flatten

  def ++[A1 <: A, B1 >: B](conv: Flow[A1, B1]): Flow[A1, B1] = new Flow[A1, B1] {
    def fold[T](end: => T, input: (Option[A1] => Flow[A1, B1]) => T, output: (B1, Flow[A1, B1]) => T): T =
      self.fold(
        end = conv.fold(end, input, output),
        input = f => input(f andThen (_ ++ conv)),
        output = (res, next) => output(res, next ++ conv))
  }

  def feedIterator(source: Iterator[A]): Iterator[B] = new Iterator[B] {
    var ptr = self

    def hasNext: Boolean = ptr match {
      case End() => false
      case Input(f) if !source.hasNext => false
      case _ => true
    }

    @annotation.tailrec
    def next(): B = ptr match {
      case _ if !hasNext => throw new NoSuchElementException
      case Input(f) =>
        ptr = f(Some(source.next()))
        next()
      case Output(res, forward) =>
        ptr = forward
        res
    }
  }

  private def flatMapRest[A1 <: A, B1 >: B](f: B => Flow[A1, B1], rest: Flow[A1, B1]): Flow[A1, B1] =
    rest.fold(
      end = self.flatMap(f),
      input = run => Input(i => self.flatMapRest(f, run(i))),
      output = (res, next) => Output(res, self.flatMapRest(f, next))
    )


  def flatMap[A1 <: A, B1 >: B](f: B => Flow[A1, B1]): Flow[A1, B1] =
    self.fold(
      end = End(),
      input = run => Input(i => self.flatMap(f)),
      output = (res, next) => next.flatMapRest(f, f(res))
    )

  /** Trampolining inputs with Stream itself */
  private def feedMaybes(source: Stream[A]): Stream[Option[B]] = fold(
    end = Stream.empty,
    input = run => source match {
      case x #:: xs => None #:: run(Some(x)).feedMaybes(xs)
      case _ => None #:: run(None).feedMaybes(Stream.empty)
    },
    output = (res, next) => Some(res) #:: next.feedMaybes(source)
  )

  def andThen[C](other: Flow[B, C]): Flow[A, C] =
    self.fold(
      end = other.fold(
        end = End(),
        input = f => End[A,B]() andThen f(None),
        output = (res, next) => Output(res, End() andThen next)
      ),
      input = f => Input(x => f(x) andThen other),
      output = (b, selfNext) => other.fold(
        end = End(),
        input = f => selfNext andThen f(Some(b)),
        output = (c, otherNext) => Output(c, self andThen otherNext)
      )
    )
}

object Flow {
  def End[A, B]() = new Flow[A, B] {
    def fold[T](end: => T, input: (Option[A] => Flow[A, B]) => T, output: (B, Flow[A, B]) => T): T = end
  }

  def Input[A, B](run: Option[A] => Flow[A, B]) = new Flow[A, B] {
    def fold[T](end: => T, input: (Option[A] => Flow[A, B]) => T, output: (B, Flow[A, B]) => T): T = input(run)
  }

  def Output[A, B](res: => B, next: => Flow[A, B]) = new Flow[A, B] {
    def fold[T](end: => T, input: (Option[A] => Flow[A, B]) => T, output: (B, Flow[A, B]) => T): T = output(res, next)
  }

  def map[A, B](f: A => B): Flow[A, B] = Input {
    case Some(x) => Output(f(x), map(f))
    case None => End()
  }

  def fromStream[A](stream: Stream[A]): Flow[Any, A] = stream match {
    case x #:: xs => Output(x, fromStream(xs))
    case _ => End()
  }

  object End {
    def unapply[A, B](stream: Flow[A, B]): Boolean = stream.fold(true, _ => false, (_, _) => false)
  }

  object Input {
    def unapply[A, B](stream: Flow[A, B]): Option[Option[A] => Flow[A, B]] = stream.fold(None, Some(_), (_, _) => None)
  }

  object Output {
    def unapply[A, B](stream: Flow[A, B]): Option[(B, Flow[A, B])] = stream.fold(None, _ => None, (x, y) => Some(x, y))
  }

}
