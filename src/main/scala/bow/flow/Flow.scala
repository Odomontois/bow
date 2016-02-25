package bow.flow

import java.util.NoSuchElementException

import bow.ArrowChoice

import scalaz.{\/-, -\/, \/}

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

  def input(items: A*): Stream[B] = feed(items.toStream)

  def map[C](f: B => C): Flow[A, C] = fold(
    end = End(),
    input = run => Input(i => run(i).map(f)),
    output = (res, next) => Output(f(res), next.map(f))
  )

  def filter(f: B => Boolean): Flow[A, B] = fold(
    end = End(),
    input = run => Input(i => run(i).filter(f)),
    output = (res, next) => if (f(res)) Output(res, next.filter(f)) else next.filter(f)
  )

  def take(n: Long): Flow[A, B] = if (n <= 0) End()
  else fold(
    end = End(),
    input = run => Input(i => run(i).take(n)),
    output = (res, next) => Output(res, next.take(n - 1))
  )

  def drop(n: Long): Flow[A, B] = if (n <= 0) self
  else fold(
    end = End(),
    input = run => Input(i => run(i).drop(n)),
    output = (res, next) => next.drop(n - 1)
  )

  def takeWhile(f: B => Boolean): Flow[A, B] = fold(
    end = End(),
    input = run => Input(i => run(i).takeWhile(f)),
    output = (res, next) => if (f(res)) Output(res, next.takeWhile(f)) else End()
  )

  def dropWhile(f: B => Boolean): Flow[A, B] = fold(
    end = End(),
    input = run => Input(i => run(i).dropWhile(f)),
    output = (res, next) => if (f(res)) next.dropWhile(f) else Output(res, next)
  )

  /** filter for input */
  def censor[A1 <: A](f: A1 => Boolean): Flow[A1, B] = fold(
    end = End(),
    input = run => Input({
      case i@Some(x) => if (f(x)) run(i).censor(f) else self.censor(f)
      case None => run(None).censor(f)
    }),
    output = (res, next) => Output(res, next.censor(f))
  )

  /** take for input, take only N presented values */
  def accept(n: Long): Flow[A, B] = if (n <= 0) End()
  else fold(
    end = End(),
    input = run => Input({
      case i@Some(x) => run(i).accept(n - 1)
      case None => run(None).accept(n)
    }),
    output = (res, next) => Output(res, next.accept(n))
  )

  /** drop for input, drop first N presented values */
  def ignore(n: Long): Flow[A, B] = if (n <= 0) self
  else fold(
    end = End(),
    input = run => Input({
      case i@Some(x) => self.ignore(n - 1)
      case None => run(None).ignore(n)
    }),
    output = (res, next) => Output(res, next.ignore(n))
  )

  /** takeWhile for input, accept while predicate succedes */
  def acceptWhile[A1 <: A](f: A1 => Boolean): Flow[A1, B] = fold(
    end = End(),
    input = run => Input({
      case i@Some(x) => if (f(x)) run(i).acceptWhile(f) else End()
      case None => run(None).acceptWhile(f)
    }),
    output = (res, next) => Output(res, next.acceptWhile(f))
  )

  /** dropWhile for input, ingores while predicate succedes */
  def ignoreWhile[A1 <: A](f: A1 => Boolean): Flow[A1, B] = fold(
    end = End(),
    input = run => Input({
      case i@Some(x) => if (f(x)) self.ignoreWhile(f) else run(i)
      case None => run(None).ignoreWhile(f)
    }),
    output = (res, next) => Output(res, next.ignoreWhile(f))
  )

  private def flatMapRest[A1 <: A, C](f: B => Flow[A1, C], rest: Flow[A1, C]): Flow[A1, C] =
    rest.fold(
      end = self.flatMap(f),
      input = run => Input(i => self.flatMapRest(f, run(i))),
      output = (res, next) => Output(res, self.flatMapRest(f, next))
    )


  def flatMap[A1 <: A, C](f: B => Flow[A1, C]): Flow[A1, C] =
    self.fold(
      end = End(),
      input = run => Input(i => run(i).flatMap(f)),
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
        input = f => End[A, B]() andThen f(None),
        output = (res, next) => Output(res, End() andThen next)
      ),
      input = f => Input(x => f(x) andThen other),
      output = (b, selfNext) => other.fold(
        end = End(),
        input = f => selfNext andThen f(Some(b)),
        output = (c, otherNext) => Output(c, self andThen otherNext)
      )
    )

  /** supplementary state for `leftA` with both inputs remembered,
    * feeds remembered input ASAP */
  private def firstRemRL[C](leftI: A, rightI: C): Flow[(A, C), (B, C)] = fold(
    end = End(),
    input = run => run(Some(leftI)).firstRemR(rightI),
    output = (res, next) => Output((res, rightI), next.firstRemRL(leftI, rightI))
  )

  /** supplementary state for `leftA` with both inputs remembered */
  private def firstRemR[C](rightI: C): Flow[(A, C), (B, C)] = fold(
    end = End(),
    input = run => Input {
      case Some((a, c)) => run(Some(a)).firstRemR(c)
      case None => run(None).firstRemR(rightI)
    },
    output = (res, next) => Output((res, rightI), next.firstRemR(rightI))
  )

  /** arrow first combinator,
    * requires at least one input before output
    * feeds right output with the last remembered input
    * If there is not input, finish the flow */
  def first[C]: Flow[(A, C), (B, C)] = fold(
    end = End(),
    input = run => Input {
      case Some((a, c)) => run(Some(a)).firstRemR(c)
      case None => run(None).first
    },
    output = (b, next) => Input {
      case Some((a, c)) => Output((b, c), next.firstRemRL(a, c))
      case None => End()
    }
  )
  /** arrow with choice left combinator
    * outputs second as soon as it gets it*/
  def left[C]: Flow[A \/ C, B \/ C] = fold(
    end = End(),
    input = run => Input {
      case Some(-\/(a)) => run(Some(a)).left
      case Some(\/-(c)) => Output(\/-(c), self.left)
      case None => run(None).left
    }           ,
    output = (res, next) => Output(-\/(res), next.left)
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

  def Id[A]: Flow[A, A] = Input {
    case Some(x) => Output(x, Id)
    case None => End()
  }

  def map[A, B](f: A => B): Flow[A, B] = Input {
    case Some(x) => Output(f(x), map(f))
    case None => End()
  }

  def stream[A](items: A*) = fromStream(items.toStream)

  def fromStream[A, B](stream: Stream[B]): Flow[A, B] = stream match {
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

  implicit object flowArrowInstance extends ArrowChoice[Flow] {
    override def id[A]: Flow[A, A] = Flow.Id

    /** Feed marked inputs through the argument arrow, passing the rest through unchanged to the output. */
    def left[A, B, C](fa: Flow[A, B]): Flow[\/[A, C], \/[B, C]] = fa.left

    def arr[A, B](f: (A) => B): Flow[A, B] = Flow.map(f)

    def first[A, B, C](fa: Flow[A, B]): Flow[(A, C), (B, C)] = fa.first

    def compose[A, B, C](f: Flow[B, C], g: Flow[A, B]): Flow[A, C] = g andThen f
  }

}
