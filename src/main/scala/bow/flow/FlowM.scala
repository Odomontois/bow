package bow.flow


import scala.language.higherKinds
import scalaz.{Applicative, Functor, Monad, Monoid, StreamT}
import scalaz.syntax.monad._
import scalaz.syntax.monoid._


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
    output = (res, next) => StreamT[F, B](
      next.map(f => StreamT.Yield(res, f.feed(source)))
    )
  )

  import scalaz.syntax.functor._

  @inline private def _End(x: R): FlowM[F, R, A, B] = FlowM.End(x)
  @inline private def _Input(run: Option[A] => FM): FlowM[F, R, A, B] = FlowM.Input[F, R, A, B](run)
  @inline private def _Skip(lift: FM): FlowM[F, R, A, B] = FlowM.Skip[F, R, A, B](lift)
  @inline private def _Output(res: B, next: FM): FlowM[F, R, A, B] = FlowM.Output[F, R, A, B](res, next)

  import FlowM._

  def +:(x: R)(implicit F: Functor[F], R: Monoid[R]): That =
    fold(
      end = y => _End(x |+| y),
      input = run => _Input(y => run(y).map(x +: _)),
      skip = lift => _Skip(lift.map(x +: _)),
      output = (res, next) => _Output(res, next map (x +: _))
    )

  def ++(that: FlowM[F, R, A, B])(implicit F: Functor[F], R: Monoid[R]): That = new FlowM[F, R, A, B] {
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
    end = x => _End(x),
    input = run => _Input(i => run(i).map(_.filter(f))),
    skip = next => _Skip(next.map(_.filter(f))),
    output = (res, next) => if (f(res)) _Output(res, next.map(_.filter(f))) else _Skip(next.map(_.filter(f)))
  )

  def filterM(f: B => F[Boolean])(implicit F: Functor[F]): That = fold(
    end = x => _End(x),
    input = run => _Input(i => run(i).map(_.filterM(f))),
    skip = next => _Skip(next.map(_.filterM(f))),
    output = (res, next) => _Skip(f(res).map {
      case true => _Output(res, next.map(_.filterM(f)))
      case false => _Skip(next.map(_.filterM(f)))
    })
  )

  def take(n: Long)(implicit F: Functor[F]): FlowM[F, Option[R], A, B] = if (n <= 0) End(None)
  else fold(
    end = x => End[F, Option[R], A, B](Some(x)),
    input = run => Input[F, Option[R], A, B](i => run(i).map(_.take(n))),
    skip = next => Skip[F, Option[R], A, B](next.map(_.take(n))),
    output = (res, next) => Output[F, Option[R], A, B](res, next.map(_.take(n - 1)))
  )

  def drop(n: Long)(implicit F: Functor[F]): That = if (n <= 0) self
  else fold(
    end = x => _End(x),
    input = run => _Input(i => run(i).map(_.drop(n))),
    skip = next => _Skip(next.map(_.drop(n))),
    output = (res, next) => _Skip(next.map(_.drop(n - 1)))
  )
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

  def map[F[_], A, B](f: A => B)(implicit F: Monad[F]): FlowM[F, Unit, A, B] =
    Input[F, Unit, A, B] {
      case Some(x) => F.point(Output[F, Unit, A, B](f(x), F.point(map(f))))
      case None => F.point(End[F, Unit, A, B](()))
    }
}
