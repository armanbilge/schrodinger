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

object Bernoulli {

  val fair: Dist[Boolean] = DistT[Id, Boolean](new (Id *=> λ[S => (S, Boolean)]) {
    override def apply[S](s: S)(implicit rng: Generator[S]): (S, Boolean) =
      rng.nextBoolean.run(s)
  })

  def fairF[F[_]](implicit F: Applicative[F]): DistT[F, Boolean] =
    DistT.fromDist(fair)

  /**
   * @param p the probability of `true`
   */
  def apply(p: Float): Dist[Boolean] =
    if (p == 0.5f)
      fair
    else
      Uniform.float.map(_ < p)

  /**
   * @param p the probability of `true`
   */
  def applyF[F[_]: Applicative](p: Float): DistT[F, Boolean] =
    DistT.fromDist(apply(p))

  /**
   * @param p the probability of `true`
   */
  def apply(p: Double): Dist[Boolean] =
    if (p == 0.5)
      fair
    else
      Uniform.double.map(_ < p)

  /**
   * @param p the probability of `true`
   */
  def applyF[F[_]: Applicative](p: Double): DistT[F, Boolean] =
    DistT.fromDist(apply(p))

}
