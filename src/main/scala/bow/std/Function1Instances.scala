package bow.std

import bow.ArrowChoice

import scalaz.\/

/**
  * User: Oleg
  * Date: 09-Feb-16
  * Time: 18:21
  */
trait Function1Instances {
  implicit object function1Choice extends ArrowChoice[Function1] {

    class Choose[A1, A2, B1, B2](left: =>A1 => B1, right: => A2 => B2) extends ((A1 \/ A2) => (B1 \/ B2)) {
      lazy val _right = right
      lazy val _left = left

      def apply(x: A1 \/ A2) = x.bimap(_left, _right)
    }

    /** Feed marked inputs through the argument arrow, passing the rest through unchanged to the output. */
    def left[A, B, C](fa: (A) => B) = _.leftMap(fa)

    /** A mirror image of left. */
    override def right[A, B, C](fa: (A) => B) = _.map(fa)

    /** Split the input between the two argument arrows, retagging and merging their outputs. */
    override def choose[A1, A2, B1, B2](fa: A1 => B1)(fb: A2 => B2) = _.bimap(fa, fb)

    /** Same as choose but lazy on right argument */
    override def chooseLz[A1, A2, B1, B2](fa: => (A1) => B1)(fb: => (A2) => B2): (\/[A1, A2]) => \/[B1, B2] =
      new Choose(fa, fb)

    def arr[A, B](f: (A) => B): A => B = f

    def id[A]: A => A = identity

    def compose[A, B, C](f: B => C, g: A => B): (A) => C = f compose g

    def first[A, B, C](fa: (A) => B) = { case (x, y) => (fa(x), y) }

    /** Split the input between the two argument arrows and merge their outputs. */
    override def fanin[A, B, C](fa: (A) => C)(fb: (B) => C) = _.fold(fa, fb)

    override def second[A, B, C](f: (A) => B) = { case (x, y) => (x, f(y)) }

    override def split[A, B, C, D](f: (A) => B, g: (C) => D) = { case (x, y) => (f(x), g(y)) }

    override def mapfst[A, B, C](fab: (A) => B)(f: (C) => A) = fab compose f

    override def mapsnd[A, B, C](fab: (A) => B)(f: (B) => C) = fab andThen f
  }
}
