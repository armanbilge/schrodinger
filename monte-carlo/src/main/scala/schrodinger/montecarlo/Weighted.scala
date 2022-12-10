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
import cats.InvariantSemigroupal
import cats.Monad
import cats.Monoid
import cats.Semigroup
import cats.Show
import cats.data.Ior
import cats.kernel.CommutativeMonoid
import cats.kernel.CommutativeSemigroup
import cats.kernel.Hash
import cats.kernel.Order
import cats.syntax.all.*
import schrodinger.math.syntax.*
import schrodinger.montecarlo.Weighted.Heavy
import schrodinger.montecarlo.Weighted.Weightless

sealed abstract class Weighted[W, A] extends Product, Serializable:

  def valueOption: Option[A]
  def weight: W
  def density: W

  def isWeightless: Boolean
  final def isHeavy: Boolean = !isWeightless

  final def imap[B](f: A => B)(g: B => A): Weighted[W, B] = this match
    case Heavy(w, d, a) => Heavy(w, d, f(a))
    case Weightless(w) => new Weightless(w)

  final def product[B](b: Weighted[W, B])(using Semiring[W], Eq[W]): Weighted[W, (A, B)] =
    (this, b) match
      case (Heavy(wa, da, a), Heavy(wb, db, b)) => Weighted(wa * wb, da * db, (a, b))
      case (Weightless(w), _) => new Weightless(w)
      case (_, Weightless(w)) => new Weightless(w)

  final def importance(f: A => W)(using Semifield[W], Eq[W]): Weighted[W, A] =
    importanceA[Id](f)

  final def importanceA[F[_]: Applicative](
      f: A => F[W],
  )(using Semifield[W], Eq[W]): F[Weighted[W, A]] =
    this match
      case Heavy(w, d, a) =>
        f(a).map(fa => Weighted(w * (fa / d), fa, a))
      case weightless => weightless.pure

  final def normalize(z: W)(using MultiplicativeGroup[W]): Weighted[W, A] = this match
    case Heavy(w, d, a) => Heavy(w / z, d / z, a)
    case weightless => weightless

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

object Weighted extends WeightedInstances:

  def apply[W: Eq, A](weight: W, density: W, value: A)(using W: Semiring[W]): Weighted[W, A] =
    if W.isZero(weight) then weightless[W, A]
    else Heavy(weight, density, value)

  def apply[W, A](density: W, value: A)(using W: MultiplicativeMonoid[W]): Weighted[W, A] =
    Heavy(W.one, density, value)

  def pure[W, A](a: A)(using W: MultiplicativeMonoid[W]): Weighted[W, A] =
    Heavy(W.one, W.one, a)

  def weightless[W, A](using W: Semiring[W]): Weighted[W, A] = Weightless[W, A]

  final case class Heavy[W, A](weight: W, density: W, value: A) extends Weighted[W, A]:
    def isWeightless: false = false
    def valueOption: Some[A] = Some(value)

  final case class Weightless[W, A](weight: W) extends Weighted[W, A]:
    def density: W = weight
    def isWeightless: true = true
    def valueOption: None.type = None

  object Weightless:
    def apply[W, A](using W: Semiring[W]): Weightless[W, A] =
      Weightless(W.zero)

sealed private[montecarlo] class WeightedInstances extends WeightedInstances0:
  given [F[_], W](using Rig[W], Eq[W]): InvariantMonoidal[Weighted[W, _]] =
    WeightedInvariantMonoidal[W]

  given [W: Order, A: Order]: Order[Weighted[W, A]] =
    Order.by(w => (w.weight, w.density, w.valueOption))

  given [W: Show, A: Show]: Show[Weighted[W, A]] with
    def show(t: Weighted[W, A]): String = t.show

sealed private[montecarlo] class WeightedInstances0 extends WeightedInstances1:
  given [W]: Invariant[Weighted[W, _]] = WeightedInvariant[W]

  given [W: Hash, A: Hash]: Hash[Weighted[W, A]] =
    Hash.by(w => (w.weight, w.density, w.valueOption))

sealed private[montecarlo] class WeightedInstances1:
  given [W: Eq, A: Eq]: Eq[Weighted[W, A]] with
    def eqv(x: Weighted[W, A], y: Weighted[W, A]): Boolean = x === y

sealed private[montecarlo] class WeightedInvariant[W] extends Invariant[Weighted[W, _]]:
  def imap[A, B](fa: Weighted[W, A])(f: A => B)(g: B => A) = fa.imap(f)(g)

sealed private[montecarlo] class WeightedInvariantSemigroupal[W](using Semiring[W], Eq[W])
    extends WeightedInvariant[W],
      InvariantSemigroupal[Weighted[W, _]]:
  def product[A, B](wa: Weighted[W, A], wb: Weighted[W, B]) = wa.product(wb)

sealed private[montecarlo] class WeightedInvariantMonoidal[W](using Rig[W], Eq[W])
    extends WeightedInvariantSemigroupal[W],
      InvariantMonoidal[Weighted[W, _]]:
  def unit = Weighted.pure(())
