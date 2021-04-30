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

package schrodinger.distributions

import cats.Functor
import schrodinger.RandomT

final class Gaussian[F[_], S, A] private[distributions] (impl: GaussianImpl[F, S, A])
    extends Serializable {
  def apply: RandomT[F, S, A] = impl.apply
}

object Gaussian {
  def standard[F[_], S](implicit G: Gaussian[F, S, Double]): RandomT[F, S, Double] =
    G.apply

  def apply[F[_]: Functor, S](mean: Double, standardDeviation: Double)(
      implicit G: Gaussian[F, S, Double]): RandomT[F, S, Double] =
    standard.map(mean + _ * standardDeviation)

  implicit def schrodingerDistributionsGaussian[F[_], S, A](
      implicit impl: PrioritizeGenerator[GaussianImpl[F, S, A]]): Gaussian[F, S, A] =
    new Gaussian(impl.join)
}

trait GaussianImpl[F[_], S, A] extends Serializable {
  def apply: RandomT[F, S, A]
}
