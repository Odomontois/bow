package bow.flow


import scala.language.higherKinds
import scalaz.{-\/, Applicative, BindRec, Free, Functor, Monad, Monoid, Semigroup, StreamT, WriterT, \/, \/-}
import scalaz.syntax.monad._
import scalaz.syntax.semigroup._
import scalaz.syntax.either._


/**
  * User: Oleg
  * Date: 25-Feb-16
  * Time: 19:34
  */
trait FlowM[F[_], R, A, B] {
  self =>

  final type FM = F[FlowM[F, R, A, B]]
  final type That = FlowM[F, R, A, B]
  final type End[X] = R => X
  final type Input[X] = (Option[A] => FM) => X
  final type Skip[X] = FM => X
  final type Output[X] = (=> B, FM) => X


  def fold[X](end: End[X], input: Input[X], skip: Skip[X], output: Output[X]): X

  def feed(source: StreamT[F, A])(implicit F: Applicative[F]): StreamT[F, B] = fold(
    end = _ => StreamT.empty[F, B],
    input = run => {
      val go = source.step.map {
        case StreamT.Done => run(None).map(_.feed(StreamT.empty))
        case StreamT.Skip(next) => F.point(feed(next()))
        case StreamT.Yield(x, next) => run(Some(x)).map(_.feed(next()))
      }

      StreamT[F, B](go.map(x => StreamT.Skip(StreamT[F, B](x.map(y => StreamT.Skip(y))))))
    },
    skip = next => StreamT[F, B](next.map(f => StreamT.Skip(f.feed(source)))),
    output = (res, next) => StreamT[F, B](next.map(f => StreamT.Yield(res, f.feed(source))))
  )

  def exec(source: StreamT[F, A])(implicit F: Monad[F]): F[R] = fold(
    end = x => F.point(x),
    input = run => source.step.flatMap {
      case StreamT.Done => run(None).flatMap(_.exec(StreamT.empty))
      case StreamT.Skip(next) => exec(next())
      case StreamT.Yield(x, next) => run(Some(x)).flatMap(_.exec(next()))
    },
    skip = next => next.flatMap(_.exec(source)),
    output = (_, next) => next.flatMap(_.exec(source))
  )

  import scalaz.syntax.functor._

  @inline private def _End(x: R): FlowM[F, R, A, B] = FlowM.End(x)
  @inline private def _Input(run: Option[A] => FM): FlowM[F, R, A, B] = FlowM.Input[F, R, A, B](run)
  @inline private def _Skip(lift: FM): FlowM[F, R, A, B] = FlowM.Skip[F, R, A, B](lift)
  @inline private def _Output(res: B, next: FM): FlowM[F, R, A, B] = FlowM.Output[F, R, A, B](res, next)

  final type FMOpt = F[FlowM[F, Option[R], A, B]]
  final type ThatOpt = FlowM[F, Option[R], A, B]
  @inline private def _EndSome(x: R): FlowM[F, Option[R], A, B] = FlowM.End(Some(x))
  @inline private def _EndNone: FlowM[F, Option[R], A, B] = FlowM.End(None)
  @inline private def _InputOpt(run: Option[A] => FMOpt): FlowM[F, Option[R], A, B] = FlowM.Input[F, Option[R], A, B](run)
  @inline private def _SkipOpt(lift: FMOpt): FlowM[F, Option[R], A, B] = FlowM.Skip[F, Option[R], A, B](lift)
  @inline private def _OutputOpt(res: B, next: FMOpt): FlowM[F, Option[R], A, B] = FlowM.Output[F, Option[R], A, B](res, next)

  import FlowM._
  import StreamT.{Done, Yield, Skip => SSkip}

  def +:(x: R)(implicit F: Functor[F], R: Semigroup[R]): That = new FlowM.Plus[F, R, A, B](x, self)


  def ++(that: => FlowM[F, R, A, B])(implicit F: Functor[F], R: Semigroup[R]): That = new FlowM[F, R, A, B] {
    def fold[X](end: End[X], input: Input[X], skip: Skip[X], output: Output[X]): X =
      self.fold(
        end = x => (x +: that).fold(end, input, skip, output),
        input = f => input(x => f(x).map(_ ++ that)),
        skip = f => skip(f.map(_ ++ that)),
        output = (res, next) => output(res, next.map(_ ++ that)))
  }

  def map[C](f: B => C)(implicit F: Functor[F]): FlowM[F, R, A, C] = fold(
    end = x => End[F, R, A, C](x),
    input = run => Input[F, R, A, C](x => run(x).map(_.map(f))),
    skip = lift => Skip[F, R, A, C](lift.map(_.map(f))),
    output = (res, next) => Output[F, R, A, C](f(res), next.map(_.map(f)))
  )

  def mapM[C](f: B => F[C])(implicit F: Monad[F]): FlowM[F, R, A, C] = fold(
    end = x => End[F, R, A, C](x),
    input = run => Input[F, R, A, C](x => run(x).map(_.mapM(f))),
    skip = lift => Skip[F, R, A, C](lift.map(_.mapM(f))),
    output = (res, next) => Skip[F, R, A, C](f(res).map(r => Output[F, R, A, C](r, next.map(_.mapM(f)))))
  )

  def filter(f: B => Boolean)(implicit F: Functor[F]): That = fold(
    end = _End,
    input = run => _Input(i => run(i).map(_.filter(f))),
    skip = next => _Skip(next.map(_.filter(f))),
    output = (res, next) => if (f(res)) _Output(res, next.map(_.filter(f))) else _Skip(next.map(_.filter(f)))
  )

  def filterM(f: B => F[Boolean])(implicit F: Functor[F]): That = fold(
    end = _End,
    input = run => _Input(i => run(i).map(_.filterM(f))),
    skip = next => _Skip(next.map(_.filterM(f))),
    output = (res, next) => _Skip(f(res).map {
      case true => _Output(res, next.map(_.filterM(f)))
      case false => _Skip(next.map(_.filterM(f)))
    })
  )

  def take(n: Long)(implicit F: Functor[F]): ThatOpt = if (n <= 0) _EndNone
  else fold(
    end = _EndSome,
    input = run => _InputOpt(i => run(i).map(_.take(n))),
    skip = next => _SkipOpt(next.map(_.take(n))),
    output = (res, next) => _OutputOpt(res, next.map(_.take(n - 1)))
  )

  def drop(n: Long)(implicit F: Functor[F]): That = if (n <= 0) self
  else fold(
    end = _End,
    input = run => _Input(i => run(i).map(_.drop(n))),
    skip = next => _Skip(next.map(_.drop(n))),
    output = (res, next) => _Skip(next.map(_.drop(n - 1)))
  )

  def takeWhile(f: B => Boolean)(implicit F: Functor[F]): ThatOpt = fold(
    end = _EndSome,
    input = run => _InputOpt(i => run(i).map(_.takeWhile(f))),
    skip = lift => _SkipOpt(lift.map(_.takeWhile(f))),
    output = (res, next) => if (f(res)) _OutputOpt(res, next.map(_.takeWhile(f))) else _EndNone
  )

  def takeWhileM(f: B => F[Boolean])(implicit F: Functor[F]): ThatOpt = fold(
    end = _EndSome,
    input = run => _InputOpt(i => run(i).map(_.takeWhileM(f))),
    skip = lift => _SkipOpt(lift.map(_.takeWhileM(f))),
    output = (res, next) => _SkipOpt(f(res).map {
      case true => _OutputOpt(res, next.map(_.takeWhileM(f)))
      case false => _EndNone
    })
  )

  def dropWhile(f: B => Boolean)(implicit F: Functor[F]): That = fold(
    end = _End,
    input = run => _Input(i => run(i).map(_.dropWhile(f))),
    skip = next => _Skip(next.map(_.dropWhile(f))),
    output = (res, next) => if (f(res)) _Skip(next.map(_.dropWhile(f))) else _Output(res, next)
  )

  def dropWhileM(f: B => F[Boolean])(implicit F: Functor[F]): That = fold(
    end = _End,
    input = run => _Input(i => run(i).map(_.dropWhileM(f))),
    skip = next => _Skip(next.map(_.dropWhileM(f))),
    output = (res, next) => _Skip(f(res).map {
      case true => _Skip(next.map(_.dropWhileM(f)))
      case false => _Output(res, next)
    }))

  /** map for input */
  def comap[C](f: C => A)(implicit F: Functor[F]): FlowM[F, R, C, B] = fold(
    end = End[F, R, C, B],
    input = run => Input[F, R, C, B](i => run(i.map(f)).map(_.comap(f))),
    skip = next => Skip[F, R, C, B](next.map(_.comap(f))),
    output = (res, next) => Output[F, R, C, B](res, next.map(_.comap(f)))
  )

  /** map for input */
  def comapM[C](f: C => F[A])(implicit F: Functor[F]): FlowM[F, R, C, B] = fold(
    end = End[F, R, C, B],
    input = run => Input[F, R, C, B] {
      case None => run(None).map(_.comapM(f))
      case Some(x) => f(x).map(y => Skip[F, R, C, B](run(Some(y)).map(_.comapM(f))))
    },
    skip = next => Skip[F, R, C, B](next.map(_.comapM(f))),
    output = (res, next) => Output[F, R, C, B](res, next.map(_.comapM(f)))
  )

  /** filter for input */
  def censor(f: A => Boolean)(implicit F: Applicative[F]): That = fold(
    end = _End,
    input = run => _Input {
      case i@Some(x) => if (f(x)) run(i).map(_.censor(f)) else F.point(self.censor(f))
      case None => run(None).map(_.censor(f))
    },
    skip = next => _Skip(next.map(_.censor(f))),
    output = (res, next) => _Output(res, next.map(_.censor(f)))
  )

  /** filter for input */
  def censorM(f: A => F[Boolean])(implicit F: Functor[F]): That = fold(
    end = _End,
    input = run => _Input {
      case i@Some(x) => f(x).map {
        case true => _Skip(run(i).map(_.censorM(f)))
        case false => self.censorM(f)
      }
      case None => run(None).map(_.censorM(f))
    },
    skip = next => _Skip(next.map(_.censorM(f))),
    output = (res, next) => _Output(res, next.map(_.censorM(f)))
  )

  /** take for input, take only N presented values */
  def accept(n: Long)(implicit F: Functor[F]): ThatOpt = if (n <= 0) _EndNone
  else fold(
    end = _EndSome,
    input = run => _InputOpt {
      case i@Some(_) => run(i).map(_.accept(n - 1))
      case None => run(None).map(_.accept(n))
    },
    skip = next => _SkipOpt(next.map(_.accept(n))),
    output = (res, next) => _OutputOpt(res, next.map(_.accept(n)))
  )

  /** drop for input, drop first N presented values */
  def ignore(n: Long)(implicit F: Applicative[F]): That = if (n <= 0) self
  else fold(
    end = _End,
    input = run => _Input {
      case i@Some(_) => F.point(self.ignore(n - 1))
      case None => run(None).map(_.ignore(n))
    },
    skip = next => _Skip(next.map(_.ignore(n))),
    output = (res, next) => _Output(res, next.map(_.ignore(n)))
  )

  /** takeWhile for input, accept while predicate succedes */
  def acceptWhile(f: A => Boolean)(implicit F: Applicative[F]): ThatOpt = fold(
    end = _EndSome,
    input = run => _InputOpt {
      case i@Some(x) => if (f(x)) run(i).map(_.acceptWhile(f)) else F.point(_EndNone)
      case None => run(None).map(_.acceptWhile(f))
    },
    skip = lift => _SkipOpt(lift.map(_.acceptWhile(f))),
    output = (res, next) => _OutputOpt(res, next.map(_.acceptWhile(f)))
  )

  /** takeWhile for input, accept while predicate succedes */
  def acceptWhileM(f: A => F[Boolean])(implicit F: Functor[F]): ThatOpt = fold(
    end = _EndSome,
    input = run => _InputOpt {
      case i@Some(x) => f(x).map {
        case true => _SkipOpt(run(i).map(_.acceptWhileM(f)))
        case false => _EndNone
      }
      case None => run(None).map(_.acceptWhileM(f))
    },
    skip = lift => _SkipOpt(lift.map(_.acceptWhileM(f))),
    output = (res, next) => _OutputOpt(res, next.map(_.acceptWhileM(f)))
  )

  /** dropWhile for input, ingores while predicate succedes */
  def ignoreWhile(f: A => Boolean)(implicit F: Applicative[F]): That = fold(
    end = _End,
    input = run => _Input {
      case i@Some(x) => if (f(x)) F.point(self.ignoreWhile(f)) else run(i)
      case None => run(None).map(_.ignoreWhile(f))
    },
    skip = lift => _Skip(lift.map(_.ignoreWhile(f))),
    output = (res, next) => _Output(res, next.map(_.ignoreWhile(f)))
  )

  /** dropWhile for input, ingores while predicate succedes */
  def ignoreWhileM(f: A => F[Boolean])(implicit F: Functor[F]): That = fold(
    end = _End,
    input = run => _Input {
      case i@Some(x) => f(x).map {
        case true => self.ignoreWhileM(f)
        case false => _Skip(run(i))
      }
      case None => run(None).map(_.ignoreWhileM(f))
    },
    skip = lift => _Skip(lift.map(_.ignoreWhileM(f))),
    output = (res, next) => _Output(res, next.map(_.ignoreWhileM(f)))
  )

  private def flatMapRest[C](f: B => FlowM[F, R, A, C], rest: FlowM[F, R, A, C])(implicit F: Functor[F], R: Semigroup[R]): FlowM[F, R, A, C] =
    rest.fold(
      end = x => x +: self.flatMap(f),
      input = run => Input[F, R, A, C](i => run(i).map(self.flatMapRest(f, _))),
      skip = lift => Skip[F, R, A, C](lift.map(self.flatMapRest(f, _))),
      output = (res, next) => Output[F, R, A, C](res, next.map(self.flatMapRest(f, _)))
    )

  def flatMap[C](f: B => FlowM[F, R, A, C])(implicit F: Functor[F], R: Semigroup[R]): FlowM[F, R, A, C] =
    self.fold(
      end = End[F, R, A, C],
      input = run => Input[F, R, A, C](i => run(i).map(_.flatMap(f))),
      skip = lift => Skip[F, R, A, C](lift.map(_.flatMap(f))),
      output = (res, next) => Skip[F, R, A, C](next.map(_.flatMapRest(f, f(res))))
    )

  def andThen[C](other: FlowM[F, R, B, C])(implicit F: Functor[F], R: Semigroup[R]): FlowM[F, R, A, C] =
    self.fold(
      end = x => other.fold[FlowM[F, R, A, C]](
        end = y => End[F, R, A, C](x |+| y),
        input = (run: Option[B] => F[FlowM[F, R, B, C]]) => Skip[F, R, A, C](run(None).map(s => self.andThen(s))),
        skip = next => Skip[F, R, A, C](next.map(self andThen _)),
        output = (res, next) => Output[F, R, A, C](res, next map (self andThen _))
      ),
      input = run => Input[F, R, A, C](x => run(x).map(_ andThen other)),
      skip = next => Skip[F, R, A, C](next.map(_ andThen other)),
      output = (res, selfNext) => other.fold[FlowM[F, R, A, C]](
        end = y => End(y),
        input = (run: Option[B] => F[FlowM[F, R, B, C]]) => Skip[F, R, A, C](selfNext.map(sn => Skip[F, R, A, C](run(Some(res)).map(sn andThen _)))),
        skip = next => Skip[F, R, A, C](next.map(self andThen _)),
        output = (c, otherNext) => Output[F, R, A, C](c, otherNext.map(self andThen _))
      )
    )

  private def repeatWith(start: FlowM[F, R, A, B])(implicit F: Functor[F]): FlowM[F, Nothing, A, B] = new FlowM[F, Nothing, A, B] {
    def fold[X](end: End[X], input: Input[X], skip: Skip[X], output: Output[X]): X =
      self.fold(
        end = x => self.repeatWith(start).fold(end, input, skip, output),
        input = f => input(x => f(x).map(_.repeatWith(start))),
        skip = f => skip(f.map(_.repeatWith(start))),
        output = (res, next) => output(res, next.map(_.repeatWith(start))))
  }

  def repeat(implicit F: Functor[F]): FlowM[F, Nothing, A, B] = self.repeatWith(self)

  def replicate(n: Long)(implicit F: Functor[F], R: Monoid[R]): FlowM[F, R, A, B] = if (n == 0) End(R.zero) else self ++ self.replicate(n - 1)

  def mapResult[S](f: R => S)(implicit F: Functor[F]): FlowM[F, S, A, B] = new FlowM[F, S, A, B] {
    def fold[X](end: End[X], input: Input[X], skip: Skip[X], output: Output[X]): X =
      self.fold(
        end = x => end(f(x)),
        input = g => input(x => g(x).map(_.mapResult(f))),
        skip = g => skip(g.map(_.mapResult(f))),
        output = (res, next) => output(res, next.map(_.mapResult(f))))
  }

  def collect(f: B => R, is: StreamT[F, A])(implicit F: Monad[F], R: Semigroup[R]): F[R] = fold(
    end = x => F.point(x),
    input = g => is.step.flatMap(_.apply(
      yieldd = (x, next) => g(Some(x)).flatMap(_.collect(f, next)),
      skip = self.collect(f, _),
      done = g(None).flatMap(_.collect(f, is))
    )),
    skip = _.flatMap(_.collect(f, is)),
    output = (x, next) => next.flatMap(u => f(x) +: u collect(f, is))
  )

  def collectFree(f: B => R, is: StreamT[F, A])(implicit F: Functor[F], R: Semigroup[R]): Free[F, R] = fold[Free[F, R]](
    end = x => Free.pure(x),
    input = g => Free[F, R](is.step.map(_.apply(
      yieldd = (x, next) => Free[F, R](g(Some(x)).map(_.collectFree(f, next))),
      skip = s => self.collectFree(f, s),
      done = Free[F, R](g(None).map(_.collectFree(f, is)))
    ))),
    skip = next => Free[F, R](next.map(_.collectFree(f, is))),
    output = (x, next) => Free[F, R](next.map(u => f(x) +: u collectFree(f, is)))
  )

  def collectRec(f: B => R, is: StreamT[F, A])(implicit F: Monad[F] with BindRec[F], R: Semigroup[R]): F[R] = F.tailrecM[(That, StreamT[F, A]), R]({ case (flow, stream) =>
    flow.fold(
      end = x => F.point(x.right[(That, StreamT[F, A])]),
      input = g => is.step.flatMap(_.apply(
        done = g(None).map(u => (u, is).left[R]),
        yieldd = (x, next) => g(Some(x)).map(u => (u, is).left[R]),
        skip = next => F.point((flow, next).left[R])
      )),
      skip = next => next.map(u => (u, stream).left[R]),
      output = (x, next) => next.map(u => (f(x) +: u, stream).left[R])
    )
  })((self, is))

  def mapSumRec[S](f: B => S, g: R => S, i: A)(implicit F: Monad[F] with BindRec[F], S: Semigroup[S]): F[S] = F.tailrecM[(That, Option[S]), S]({ case (flow, acc) => flow.fold[F[(That, Option[S]) \/ S]](
    end = x => F.point((acc match {
      case Some(y) => y |+| g(x)
      case None => g(x)
    }).right[(That, Option[S])]),
    input = h => h(Some(i)).map(u => (u, acc).left[S]),
    skip = next => next.map(u => (u, acc).left[S]),
    output = (x, next) => next.map(u => (u, acc match {
      case None => Some(f(x))
      case Some(y) => Some(y |+| f(x))
    }).left[S])
  )
  })((self, None))

  def sumRec(implicit F: Monad[F] with BindRec[F], R: Monoid[R], evA: Unit =:= A, evB: B =:= R): F[R] = F.tailrecM[(That, R), R]({
    case (flow, acc) => flow.fold[F[(That, R) \/ R]](
      end = x => F.point((acc |+| x).right[(That, R)]),
      input = g => g(Some(())).map(u => (u, acc).left[R]),
      skip = next => next.map(u => (u, acc).left[R]),
      output = (x, next) => next.map(u => (u, acc |+| x).left[R])
    )
  })((self, R.zero))

}

object FlowM {

  def End[F[_], R, A, B](x: R): FlowM[F, R, A, B] = new FlowM[F, R, A, B] {
    def fold[X](end: End[X], input: Input[X], skip: Skip[X], output: Output[X]): X = end(x)
  }

  def Input[F[_], R, A, B](run: Option[A] => F[FlowM[F, R, A, B]]): FlowM[F, R, A, B] = new FlowM[F, R, A, B] {
    def fold[X](end: End[X], input: Input[X], skip: Skip[X], output: Output[X]): X = input(run)
  }

  def Output[F[_], R, A, B](res: => B, next: => F[FlowM[F, R, A, B]]): FlowM[F, R, A, B] = new FlowM[F, R, A, B] {
    def fold[X](end: End[X], input: Input[X], skip: Skip[X], output: Output[X]): X = output(res, next)
  }

  def Skip[F[_], R, A, B](next: => F[FlowM[F, R, A, B]]): FlowM[F, R, A, B] = new FlowM[F, R, A, B] {
    def fold[X](end: End[X], input: Input[X], skip: Skip[X], output: Output[X]): X = skip(next)
  }

  object End {
    def unapply[F[_], R, A, B](flow: FlowM[F, R, A, B]): Option[R] =
      flow.fold(end = Some.apply, input = _ => None, skip = _ => None, output = (_, _) => None)
  }

  object Input {
    def unapply[F[_], R, A, B](flow: FlowM[F, R, A, B]): Option[Option[A] => F[FlowM[F, R, A, B]]] =
      flow.fold(end = _ => None, input = run => Some(run), skip = _ => None, output = (_, _) => None)
  }

  object Output {
    def unapply[F[_], R, A, B](flow: FlowM[F, R, A, B]): Option[(B, F[FlowM[F, R, A, B]])] =
      flow.fold(end = _ => None, input = _ => None, skip = _ => None, output = (res, next) => Some(res, next))
  }

  object Skip {
    def unapply[F[_], R, A, B](flow: FlowM[F, R, A, B]): Option[F[FlowM[F, R, A, B]]] =
      flow.fold(end = _ => None, input = _ => None, skip = res => Some(res), output = (_, _) => None)
  }

  def Id[F[_], A](implicit F: Monad[F]): FlowM[F, Unit, A, A] =
    Input[F, Unit, A, A] {
      case Some(x) => Output[F, Unit, A, A](x, Id[F, A].point[F]).point[F]
      case None => End[F, Unit, A, A]().point[F]
    }

  private case class Plus[F[_], R, A, B](x: R, flow: FlowM[F, R, A, B])(implicit F: Functor[F], R: Semigroup[R]) extends FlowM[F, R, A, B] {
    override def +:(y: R)(implicit F: Functor[F], R: Semigroup[R]) = new Plus[F, R, A, B](y |+| x, flow)

    def fold[X](end: End[X], input: Input[X], skip: Skip[X], output: Output[X]): X =
      flow.fold(
        end = y => end(x |+| y),
        input = run => input(y => run(y).map(x +: _)),
        skip = lift => skip(lift.map(x +: _)),
        output = (res, next) => output(res, next map (x +: _))
      )
  }

  def map[F[_], A, B](f: A => B)(implicit F: Monad[F]): FlowM[F, Unit, A, B] =
    Input[F, Unit, A, B] {
      case Some(x) => F.point(Output[F, Unit, A, B](f(x), F.point(map(f))))
      case None => F.point(End[F, Unit, A, B](()))
    }

  class Make[F[_], R, A, B] {
    type Flow = FlowM[F, R, A, B]

    def End(x: R): Flow = FlowM.End[F, R, A, B](x)

    def Input(run: Option[A] => F[Flow]): Flow = FlowM.Input[F, R, A, B](run)

    def Output(res: => B, next: => F[Flow]): Flow = FlowM.Output[F, R, A, B](res, next)

    def Skip(next: => F[Flow]): Flow = FlowM.Skip[F, R, A, B](next)
  }

  def Make[F[_], R, A, B] = new Make[F, R, A, B]
}
