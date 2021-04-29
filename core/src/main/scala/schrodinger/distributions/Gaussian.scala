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

import cats.{Applicative, Id}
import schrodinger.{Dist, DistT}
import DistT.*=>
import schrodinger.generators.Generator

object Gaussian {

  val standard: Dist[Double] = DistT[Id, Double](new (Id *=> λ[S => (S, Double)]) {
    override def apply[S](s: Id[S])(implicit rng: Generator[S]): (S, Double) =
      rng.nextGaussian.run(s)
  })

  def standardF[F[_]: Applicative]: DistT[F, Double] =
    DistT.fromDist(standard)

  def apply(mean: Double, standardDeviation: Double): Dist[Double] =
    standard.map(mean + _ * standardDeviation)

  def applyF[F[_]: Applicative](mean: Double, standardDeviation: Double): DistT[F, Double] =
    DistT.fromDist(apply(mean, standardDeviation))

}
