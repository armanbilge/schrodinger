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

package schrodinger
package stats

import algebra.ring.MultiplicativeMonoid
import algebra.ring.MultiplicativeSemigroup
import cats.Applicative
import cats.Apply
import cats.Invariant
import cats.InvariantMonoidal
import cats.InvariantSemigroupal
import cats.syntax.all.*
import schrodinger.kernel.*
import schrodinger.math.syntax.*

opaque type Density[F[_], P, A] <: (A => F[P]) = A => F[P]

object Density extends DensityLowPriority0, DensityDistributionInstances:

  inline def apply[F[_], P, A](f: A => F[P]): Density[F, P, A] = f

sealed private class DensityLowPriority0 extends DensityLowPriority1:
  given [F[_]: Applicative, P: MultiplicativeMonoid]: InvariantMonoidal[Density[F, P, _]] =
    DensityInvariantMonoidal()

sealed private class DensityLowPriority1 extends DensityLowPriority2:
  given [F[_]: Apply, P: MultiplicativeSemigroup]: InvariantSemigroupal[Density[F, P, _]] =
    DensityInvariantSemigroupal()

sealed private class DensityLowPriority2:
  given [F[_], P]: Invariant[Density[F, P, _]] = DensityInvariant()

sealed private class DensityInvariant[F[_], P] extends Invariant[Density[F, P, _]]:
  def imap[A, B](da: Density[F, P, A])(f: A => B)(g: B => A) =
    Density(da.compose(g))

sealed private class DensityInvariantSemigroupal[F[_]: Apply, P: MultiplicativeSemigroup]
    extends DensityInvariant[F, P],
      InvariantSemigroupal[Density[F, P, _]]:
  def product[A, B](da: Density[F, P, A], db: Density[F, P, B]) =
    Density((a, b) => (da(a), db(b)).mapN(_ * _))

sealed private class DensityInvariantMonoidal[F[_]: Applicative, P](
    using P: MultiplicativeMonoid[P])
    extends DensityInvariantSemigroupal[F, P],
      InvariantMonoidal[Density[F, P, _]]:
  def unit = Density(_ => P.one.pure)

sealed private trait DensityDistributionInstances
    extends BernoulliInstances,
      BetaInstances,
      CategoricalInstances,
      DiscreteUniformInstances,
      ExponentialInstances,
      GammaInstances,
      GaussianInstances,
      LogNormalInstances,
      UniformInstances
