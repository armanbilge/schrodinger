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

import algebra.ring.Semifield
import cats.Monad
import cats.data.NonEmptyList
import cats.kernel.Eq
import cats.kernel.Hash
import cats.syntax.all.*
import schrodinger.kernel.Categorical
import schrodinger.math.syntax.*
import schrodinger.stats.Density

trait ImportanceSample[F[_], G[_], A]:
  def importanceSample(target: F[A], proposal: G[A], sampleCount: Int): G[A]

object ImportanceSample:

  inline def apply[F[_], G[_], A](using is: ImportanceSample[F, G, A])(
      target: F[A],
      proposal: G[A],
      sampleCount: Int): G[A] =
    is.importanceSample(target, proposal, sampleCount)

  given [F[_]: Monad, P: Hash, A: Hash](
      using P: Semifield[P],
      c: Categorical[F, NonEmptyList[P], Long]
  ): ImportanceSample[Density[F, P, _], WeightedT[F, P, _], A] with

    def importanceSample(
        target: Density[F, P, A],
        proposal: WeightedT[F, P, A],
        sampleCount: Int) = WeightedT {
      proposal.importanceF(target).value.replicateA(sampleCount).flatMap { samples =>
        val marginal = P.sum(samples.view.map(_.weight)) / P.fromInt(sampleCount)
        Categorical(NonEmptyList.fromListUnsafe(samples).fproduct(_.weight)).map {
          case Weighted.Heavy(_, d, a) => Weighted(d / marginal, a)
          case weightless @ Weighted.Weightless(_) => weightless
        }
      }
    }
