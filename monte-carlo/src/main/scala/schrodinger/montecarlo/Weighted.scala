/*
 * Copyright 2021 Arman Bilge
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package schrodinger.montecarlo

import cats.data.Ior
import cats.kernel.{CommutativeMonoid, CommutativeSemigroup}
import cats.{
  Align,
  CommutativeMonad,
  Eq,
  Functor,
  Invariant,
  InvariantMonoidal,
  Monad,
  Monoid,
  Semigroup,
  Show
}
import litter.{CommutativeZeroMonoid, ZeroGroup, ZeroMonoid}
import schrodinger.montecarlo.Weighted.{Heavy, Weightless}

import scala.annotation.tailrec

sealed abstract class Weighted[W, +A] extends Product with Serializable {

  def weight: W

  def isWeightless: Boolean
  final def isHeavy: Boolean = !isWeightless

  final def map[B](f: A => B): Weighted[W, B] = this match {
    case Heavy(w, a) => Heavy(w, f(a))
    case weightless @ Weightless(_) => weightless
  }

  final def flatMap[B](
      f: A => Weighted[W, B])(implicit W0: ZeroMonoid[W], W1: Eq[W]): Weighted[W, B] =
    this match {
      case Heavy(wa, a) =>
        f(a) match {
          case Heavy(wb, b) => Weighted(W0.combine(wa, wb), b)
          case weightless @ Weightless(_) => weightless
        }
      case weightless @ Weightless(_) => weightless
    }

  final def product[B](
      b: Weighted[W, B])(implicit W0: ZeroMonoid[W], W1: Eq[W]): Weighted[W, (A, B)] =
    (this, b) match {
      case (Heavy(wa, a), Heavy(wb, b)) => Weighted(W0.combine(wa, wb), (a, b))
      case (weightless @ Weightless(_), _) => weightless
      case (_, weightless @ Weightless(_)) => weightless
    }

  final def importance(f: A => W)(implicit W0: ZeroGroup[W], W1: Eq[W]): Weighted[W, A] =
    this match {
      case Heavy(w, a) => Weighted(W0.remove(f(a), w), a)
      case weightless @ Weightless(_) => weightless
    }

  final override def toString: String =
    show(Show.fromToString, Show.fromToString)

  final def show[B >: A](implicit A: Show[B], W: Show[W]): String = this match {
    case Heavy(w, a) => s"Heavy(${W.show(w)}, ${A.show(a)})"
    case Weightless(w) => s"Weightless(${W.show(w)})"
  }

  final def ===[B >: A](that: Weighted[W, B])(implicit A: Eq[B], W: Eq[W]): Boolean =
    (this, that) match {
      case (Heavy(wa, a), Heavy(wb, b)) => W.eqv(wa, wb) && A.eqv(a, b)
      case (Weightless(wa), Weightless(wb)) => W.eqv(wa, wb)
      case _ => false
    }
}

object Weighted extends WeightedInstances with WeightedFunctions {

  final case class Heavy[W, +A](weight: W, value: A) extends Weighted[W, A] {
    override def isWeightless = false
  }

  final case class Weightless[W](weight: W) extends Weighted[W, Nothing] {
    override def isWeightless = true
  }

  object Weightless {
    def apply[W](implicit W: ZeroMonoid[W]): Weightless[W] =
      Weightless(W.absorbing)
  }
}

sealed private[montecarlo] class WeightedInstances extends WeightedInstances0 {
  implicit def schrodingerMonteCarloCommutativeMonadForWeighted[W](
      implicit ev0: CommutativeZeroMonoid[W],
      ev1: Eq[W]): CommutativeMonad[Weighted[W, *]] =
    new WeightedCommutativeMonad[W] {
      implicit override val W0: CommutativeZeroMonoid[W] = ev0
      implicit override val W1: Eq[W] = ev1
    }

  implicit def schrodingerMonteCarloAlignForWeighted[W](
      implicit ev0: ZeroMonoid[W],
      ev1: Eq[W]): Align[Weighted[W, *]] =
    new WeightedAlign[W] {
      implicit override val W0: ZeroMonoid[W] = ev0
      implicit override val W1: Eq[W] = ev1
      override def functor: Functor[Weighted[W, *]] =
        schrodingerMonteCarloMonadForWeighted[W](W0, W1)
    }
}

sealed private[montecarlo] class WeightedInstances0 extends WeightedInstances1 {
  implicit def schrodingerMonteCarloMonadForWeighted[W](
      implicit ev0: ZeroMonoid[W],
      ev1: Eq[W]): Monad[Weighted[W, *]] =
    new WeightedMonad[W] {
      implicit override val W0: ZeroMonoid[W] = ev0
      implicit override val W1: Eq[W] = ev1
    }

  implicit def schrodingerMonteCarloEqForWeighted[W: Eq, A: Eq]: Eq[Weighted[W, A]] =
    new Eq[Weighted[W, A]] {
      override def eqv(x: Weighted[W, A], y: Weighted[W, A]): Boolean = x === y
    }

  implicit def schrodingerMonteCarloShowForWeighted[W: Show, A: Show]: Show[Weighted[W, A]] =
    new Show[Weighted[W, A]] {
      override def show(t: Weighted[W, A]): String = t.show
    }

  implicit def schrodingerMonteCarloCommutativeMonoidForWeighted[W, A](
      implicit ev0: CommutativeZeroMonoid[W],
      ev1: Eq[W],
      ev2: CommutativeMonoid[A]): CommutativeMonoid[Weighted[W, A]] =
    new WeightedCommutativeMonoid[W, A] {
      implicit override val A: CommutativeMonoid[A] = ev2
      implicit override val W0: CommutativeZeroMonoid[W] = ev0
      implicit override val W1: Eq[W] = ev1
    }
}

sealed private[montecarlo] class WeightedInstances1 extends WeightedInstances2 {
  implicit def schrodingerMonteCarloMonoidForWeighted[W, A](
      implicit ev0: ZeroMonoid[W],
      ev1: Eq[W],
      ev2: Monoid[A]): Monoid[Weighted[W, A]] =
    new WeightedMonoid[W, A] {
      implicit override val A: Monoid[A] = ev2
      implicit override val W0: ZeroMonoid[W] = ev0
      implicit override val W1: Eq[W] = ev1
    }
}

sealed private[montecarlo] class WeightedInstances2 extends WeightedInstances3 {
  implicit def schrodingerMonteCarloCommutativeSemigroupForWeighted[W, A](
      implicit ev0: CommutativeZeroMonoid[W],
      ev1: Eq[W],
      ev2: CommutativeSemigroup[A]): CommutativeSemigroup[Weighted[W, A]] =
    new WeightedCommutativeSemigroup[W, A] {
      implicit override val A: CommutativeSemigroup[A] = ev2
      implicit override val W0: CommutativeZeroMonoid[W] = ev0
      implicit override val W1: Eq[W] = ev1
    }

  implicit def schrodingerMonteCarloInvariantMonoidalForWeighted[F[_], W](
      implicit ev0: ZeroMonoid[W],
      ev1: Eq[W]): InvariantMonoidal[Weighted[W, *]] =
    new WeightedInvariantMonoidal[W] {
      implicit override val W0: ZeroMonoid[W] = ev0
      implicit override val W1: Eq[W] = ev1
    }
}

sealed private[montecarlo] class WeightedInstances3 {

  implicit def schrodingerMonteCarloSemigroupForWeighted[W, A](
      implicit ev0: ZeroMonoid[W],
      ev1: Eq[W],
      ev2: Semigroup[A]): Semigroup[Weighted[W, A]] =
    new WeightedSemigroup[W, A] {
      implicit override def A: Semigroup[A] = ev2
      implicit override def W0: ZeroMonoid[W] = ev0
      implicit override def W1: Eq[W] = ev1
    }

  implicit def schrodingerMonteCarloInvariantForWeighted[W]: Invariant[Weighted[W, *]] =
    new WeightedInvariant[W]
}

sealed private[montecarlo] class WeightedFunctor[W] extends Functor[Weighted[W, *]] {
  override def map[A, B](fa: Weighted[W, A])(f: A => B): Weighted[W, B] =
    fa.map(f)
}

sealed abstract private[montecarlo] class WeightedMonad[W]
    extends WeightedFunctor[W]
    with Monad[Weighted[W, *]] {

  implicit def W0: ZeroMonoid[W]
  implicit def W1: Eq[W]

  override def pure[A](x: A): Weighted[W, A] =
    Weighted.pure(x)

  override def flatMap[A, B](fa: Weighted[W, A])(f: A => Weighted[W, B]): Weighted[W, B] =
    fa.flatMap(f)

  override def product[A, B](fa: Weighted[W, A], fb: Weighted[W, B]): Weighted[W, (A, B)] =
    fa.product(fb)

  def tailRecM[A, B](a: A)(f: A => Weighted[W, Either[A, B]]): Weighted[W, B] = {
    @tailrec
    def loop(wab: Weighted[W, Either[A, B]]): Weighted[W, B] = wab match {
      case weightless @ Weightless(_) => weightless
      case Heavy(w, Right(b)) => Heavy(w, b)
      case Heavy(w1, aorb) =>
        aorb match {
          case Right(b) => Heavy(w1, b)
          case Left(a) =>
            f(a) match {
              case weightless @ Weightless(_) => weightless
              case Heavy(w2, aorb) => loop(Weighted(W0.combine(w1, w2), aorb))
            }
        }

    }

    loop(f(a))
  }

}

sealed abstract private[montecarlo] class WeightedCommutativeMonad[W]
    extends WeightedMonad[W]
    with CommutativeMonad[Weighted[W, *]] {
  implicit override def W0: CommutativeZeroMonoid[W]
}

sealed abstract private[montecarlo] class WeightedSemigroup[W, A]
    extends Semigroup[Weighted[W, A]] {
  implicit def A: Semigroup[A]
  implicit def W0: ZeroMonoid[W]
  implicit def W1: Eq[W]
  override def combine(wx: Weighted[W, A], wy: Weighted[W, A]): Weighted[W, A] =
    (wx, wy) match {
      case (Heavy(wx, x), Heavy(wy, y)) => Weighted(W0.combine(wx, wy), A.combine(x, y))
      case (weightless @ Weightless(_), _) => weightless
      case (_, weightless @ Weightless(_)) => weightless
    }
}

sealed abstract private[montecarlo] class WeightedCommutativeSemigroup[W, A]
    extends WeightedSemigroup[W, A]
    with CommutativeSemigroup[Weighted[W, A]] {
  implicit def A: CommutativeSemigroup[A]
  implicit def W0: CommutativeZeroMonoid[W]
}

sealed abstract private[montecarlo] class WeightedMonoid[W, A]
    extends WeightedSemigroup[W, A]
    with Monoid[Weighted[W, A]] {
  implicit def A: Monoid[A]
  override def empty: Weighted[W, A] = Weighted.pure(A.empty)
}

sealed abstract private[montecarlo] class WeightedCommutativeMonoid[W, A]
    extends WeightedMonoid[W, A]
    with CommutativeMonoid[Weighted[W, A]] {
  implicit def A: CommutativeMonoid[A]
  implicit def W0: CommutativeZeroMonoid[W]
}

sealed private[montecarlo] class WeightedInvariant[W] extends Invariant[Weighted[W, *]] {
  override def imap[A, B](fa: Weighted[W, A])(f: A => B)(g: B => A): Weighted[W, B] =
    fa.map(f)
}

sealed abstract private[montecarlo] class WeightedInvariantMonoidal[W]
    extends WeightedInvariant[W]
    with InvariantMonoidal[Weighted[W, *]] {
  implicit def W0: ZeroMonoid[W]
  implicit def W1: Eq[W]

  override def unit: Weighted[W, Unit] =
    Weighted.pure(())

  override def product[A, B](fa: Weighted[W, A], fb: Weighted[W, B]): Weighted[W, (A, B)] =
    fa.product(fb)
}

sealed abstract private[montecarlo] class WeightedAlign[W] extends Align[Weighted[W, *]] {
  implicit def W0: ZeroMonoid[W]
  implicit def W1: Eq[W]

  override def align[A, B](fa: Weighted[W, A], fb: Weighted[W, B]): Weighted[W, Ior[A, B]] =
    (fa, fb) match {
      case (Heavy(wa, a), Heavy(wb, b)) => Weighted(W0.combine(wa, wb), Ior.both(a, b))
      case (weightless @ Weightless(_), _) => weightless
      case (_, weightless @ Weightless(_)) => weightless
    }
}

sealed private[montecarlo] trait WeightedFunctions {
  def apply[W: Eq, A](weight: W, value: A)(implicit W: ZeroMonoid[W]): Weighted[W, A] =
    if (W.isAbsorbing(weight))
      weightless[W, A]
    else
      Heavy(weight, value)

  def pure[W, A](a: A)(implicit W: Monoid[W]): Weighted[W, A] =
    Heavy(W.empty, a)

  def weightless[W, A](implicit W: ZeroMonoid[W]): Weighted[W, A] =
    Weightless[W]
}