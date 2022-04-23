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

import algebra.ring.CommutativeRig
import algebra.ring.MultiplicativeCommutativeGroup
import algebra.ring.MultiplicativeCommutativeMonoid
import algebra.ring.MultiplicativeGroup
import algebra.ring.MultiplicativeMonoid
import algebra.ring.Rig
import algebra.ring.Semifield
import algebra.ring.Semiring
import cats.Align
import cats.Applicative
import cats.CommutativeMonad
import cats.Eq
import cats.Functor
import cats.Id
import cats.Invariant
import cats.InvariantMonoidal
import cats.Monad
import cats.Monoid
import cats.Semigroup
import cats.Show
import cats.data.Ior
import cats.kernel.CommutativeMonoid
import cats.kernel.CommutativeSemigroup
import cats.syntax.all.*
import schrodinger.math.syntax.*
import schrodinger.montecarlo.Weighted.Heavy
import schrodinger.montecarlo.Weighted.Weightless

import scala.annotation.tailrec

sealed abstract class Weighted[W, +A] extends Product, Serializable:

  def weight: W
  def density: W

  def isWeightless: Boolean
  final def isHeavy: Boolean = !isWeightless

  final def map[B](f: A => B): Weighted[W, B] = this match
    case Heavy(w, d, a) => Heavy(w, d, f(a))
    case weightless @ Weightless(_) => weightless

  final def flatMap[B](f: A => Weighted[W, B])(using Rig[W], Eq[W]): Weighted[W, B] =
    this match
      case Heavy(wa, da, a) =>
        f(a) match
          case Heavy(wb, db, b) => Weighted(wa * wb, da * db, b)
          case weightless @ Weightless(_) => weightless
      case weightless @ Weightless(_) => weightless

  final def product[B](b: Weighted[W, B])(using Rig[W], Eq[W]): Weighted[W, (A, B)] =
    (this, b) match
      case (Heavy(wa, da, a), Heavy(wb, db, b)) => Weighted(wa * wb, da * db, (a, b))
      case (weightless @ Weightless(_), _) => weightless
      case (_, weightless @ Weightless(_)) => weightless

  final def importance(f: A => W)(using Semifield[W], Eq[W]): Weighted[W, A] =
    importanceA[Id, A](f)

  final def importanceA[F[_]: Applicative, B >: A](
      f: B => F[W])(using Semifield[W], Eq[W]): F[Weighted[W, B]] =
    this match
      case Heavy(w, d, a) =>
        f(a).map(fa => Weighted(w * (fa / d), fa, a))
      case weightless @ Weightless(_) => weightless.pure

  final override def toString: String =
    show(using Show.fromToString, Show.fromToString)

  final def show[B >: A: Show](using Show[W]): String = this match
    case Heavy(w, d, a) => s"Heavy(${w.show}, ${d.show}, ${(a: B).show})"
    case Weightless(w) => s"Weightless(${w.show})"

  final def ===[B >: A: Eq](that: Weighted[W, B])(using Eq[W]): Boolean =
    (this, that) match
      case (Heavy(wa, da, a), Heavy(wb, db, b)) => wa === wb && da === db && b === a
      case (Weightless(wa), Weightless(wb)) => wa === wb
      case _ => false

object Weighted extends WeightedInstances, WeightedFunctions:

  final case class Heavy[W, +A](weight: W, density: W, value: A) extends Weighted[W, A]:
    override def isWeightless = false

  final case class Weightless[W](weight: W) extends Weighted[W, Nothing]:
    override def density: W = weight
    override def isWeightless = true

  object Weightless:
    def apply[W](using W: Semiring[W]): Weightless[W] =
      Weightless(W.zero)

sealed private[montecarlo] class WeightedInstances extends WeightedInstances0:
  given [W](using CommutativeRig[W], Eq[W]): CommutativeMonad[Weighted[W, _]] =
    new WeightedCommutativeMonad[W]

  given [W](using Rig[W], Eq[W]): Align[Weighted[W, _]] =
    new WeightedAlign[W]

sealed private[montecarlo] class WeightedInstances0 extends WeightedInstances1:
  given [W](using Rig[W], Eq[W]): Monad[Weighted[W, _]] =
    new WeightedMonad[W]

  given [W: Eq, A: Eq]: Eq[Weighted[W, A]] with
    override def eqv(x: Weighted[W, A], y: Weighted[W, A]): Boolean = x === y

  given [W: Show, A: Show]: Show[Weighted[W, A]] with
    override def show(t: Weighted[W, A]): String = t.show

  given [W, A](
      using CommutativeRig[W],
      Eq[W],
      CommutativeMonoid[A]): CommutativeMonoid[Weighted[W, A]] =
    new WeightedCommutativeMonoid[W, A]

sealed private[montecarlo] class WeightedInstances1 extends WeightedInstances2:
  given [W, A](using Rig[W], Eq[W], Monoid[A]): Monoid[Weighted[W, A]] =
    new WeightedMonoid[W, A]

sealed private[montecarlo] class WeightedInstances2 extends WeightedInstances3:
  given [W, A](
      using CommutativeRig[W],
      Eq[W],
      CommutativeSemigroup[A]): CommutativeSemigroup[Weighted[W, A]] =
    new WeightedCommutativeSemigroup[W, A]

  given [F[_], W](using Rig[W], Eq[W]): InvariantMonoidal[Weighted[W, _]] =
    new WeightedInvariantMonoidal[W]

sealed private[montecarlo] class WeightedInstances3:

  given [W, A](using Rig[W], Eq[W], Semigroup[A]): Semigroup[Weighted[W, A]] =
    new WeightedSemigroup[W, A]

  given [W]: Functor[Weighted[W, _]] =
    new WeightedFunctor[W]

sealed private[montecarlo] class WeightedFunctor[W] extends Functor[Weighted[W, _]]:
  override def map[A, B](fa: Weighted[W, A])(f: A => B): Weighted[W, B] =
    fa.map(f)

sealed private[montecarlo] class WeightedMonad[W](using Rig[W], Eq[W])
    extends WeightedFunctor[W],
      Monad[Weighted[W, _]]:

  override def pure[A](x: A): Weighted[W, A] =
    Weighted.pure(x)

  override def flatMap[A, B](fa: Weighted[W, A])(f: A => Weighted[W, B]): Weighted[W, B] =
    fa.flatMap(f)

  override def product[A, B](fa: Weighted[W, A], fb: Weighted[W, B]): Weighted[W, (A, B)] =
    fa.product(fb)

  def tailRecM[A, B](a: A)(f: A => Weighted[W, Either[A, B]]): Weighted[W, B] =
    @tailrec
    def loop(wab: Weighted[W, Either[A, B]]): Weighted[W, B] = wab match
      case weightless @ Weightless(_) => weightless
      case Heavy(w, d, Right(b)) => Heavy(w, d, b)
      case Heavy(w1, d1, ab) =>
        ab match
          case Right(b) => Heavy(w1, d1, b)
          case Left(a) =>
            f(a) match
              case weightless @ Weightless(_) => weightless
              case Heavy(w2, d2, ab) => loop(Weighted(w1 * w2, d1 * d2, ab))

    loop(f(a))

sealed private[montecarlo] class WeightedCommutativeMonad[W](using CommutativeRig[W], Eq[W])
    extends WeightedMonad[W],
      CommutativeMonad[Weighted[W, _]]

sealed private[montecarlo] class WeightedSemigroup[W, A](using Semigroup[A], Rig[W], Eq[W])
    extends Semigroup[Weighted[W, A]]:
  override def combine(wx: Weighted[W, A], wy: Weighted[W, A]): Weighted[W, A] =
    (wx, wy) match
      case (Heavy(wx, dx, x), Heavy(wy, dy, y)) => Weighted(wx * wy, dx * dy, x |+| y)
      case (weightless @ Weightless(_), _) => weightless
      case (_, weightless @ Weightless(_)) => weightless

sealed private[montecarlo] class WeightedCommutativeSemigroup[W, A](
    using CommutativeSemigroup[A],
    CommutativeRig[W],
    Eq[W])
    extends WeightedSemigroup[W, A],
      CommutativeSemigroup[Weighted[W, A]]

sealed private[montecarlo] class WeightedMonoid[W, A](using Monoid[A], Rig[W], Eq[W])
    extends WeightedSemigroup[W, A],
      Monoid[Weighted[W, A]]:
  override def empty: Weighted[W, A] = Weighted.pure(Monoid[A].empty)

sealed private[montecarlo] class WeightedCommutativeMonoid[W, A](
    using CommutativeMonoid[A],
    CommutativeRig[W],
    Eq[W])
    extends WeightedMonoid[W, A],
      CommutativeMonoid[Weighted[W, A]]
sealed private[montecarlo] class WeightedInvariantMonoidal[W](using Rig[W], Eq[W])
    extends WeightedFunctor[W],
      InvariantMonoidal[Weighted[W, _]]:

  override def unit: Weighted[W, Unit] =
    Weighted.pure(())

  override def product[A, B](fa: Weighted[W, A], fb: Weighted[W, B]): Weighted[W, (A, B)] =
    fa.product(fb)

sealed private[montecarlo] class WeightedAlign[W](using Rig[W], Eq[W])
    extends Align[Weighted[W, _]]:

  override def functor = Weighted.given_Monad_Weighted

  override def align[A, B](fa: Weighted[W, A], fb: Weighted[W, B]): Weighted[W, Ior[A, B]] =
    (fa, fb) match
      case (Heavy(wa, da, a), Heavy(wb, db, b)) =>
        Weighted(wa * wb, da * db, Ior.both(a, b))
      case (weightless @ Weightless(_), _) => weightless
      case (_, weightless @ Weightless(_)) => weightless

sealed private[montecarlo] trait WeightedFunctions:
  def apply[W: Eq, A](weight: W, density: W, value: A)(using W: Rig[W]): Weighted[W, A] =
    if W.isZero(weight) then weightless[W, A]
    else Heavy(weight, density, value)

  def apply[W, A](density: W, value: A)(using W: MultiplicativeMonoid[W]): Weighted[W, A] =
    Heavy(W.one, density, value)

  def pure[W, A](a: A)(using W: MultiplicativeMonoid[W]): Weighted[W, A] =
    Heavy(W.one, W.one, a)

  def weightless[W, A](using W: Semiring[W]): Weighted[W, A] =
    Weightless[W]
