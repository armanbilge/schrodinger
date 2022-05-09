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

import algebra.ring.Rig
import algebra.ring.Semifield
import cats.Applicative
import cats.syntax.all.*
import schrodinger.kernel.Bernoulli
import schrodinger.kernel.FairBernoulli
import schrodinger.math.Monus
import schrodinger.math.syntax.*

private trait BernoulliInstances:

  given [F[_]: Applicative, A](
      using A: Semifield[A]
  ): FairBernoulli[Density[F, A, _], Boolean] with
    def fairBernoulli =
      val half = A.fromInt(2).reciprocal.pure
      Density(_ => half)

  given [F[_]: Applicative, A: Monus](using A: Rig[A]): Bernoulli[Density[F, A, _], A, Boolean]
    with
    def bernoulli(successProbability: A) =
      val success = successProbability.pure
      val failure = (A.one ∸ successProbability).pure
      Density(if _ then success else failure)
