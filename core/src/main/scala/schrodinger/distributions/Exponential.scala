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

import cats.Applicative
import schrodinger.{Dist, DistT}

object Exponential {

  // TODO Replace with Ahrens-Dieter sampler?
  val standard: Dist[Double] =
    Uniform.double.map(U => -math.log(1 - U))

  def apply(rate: Double): Dist[Double] =
    standard.map(_ / rate)

  def applyF[F[_]: Applicative](rate: Double): DistT[F, Double] =
    DistT.fromDist(apply(rate))

}
