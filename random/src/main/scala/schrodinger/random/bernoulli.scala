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

package schrodinger.random

import cats.Functor
import cats.syntax.all.given
import schrodinger.kernel.{Bernoulli, Random, Uniform}
import schrodinger.math.Interval.*
import schrodinger.math.Interval.given

object bernoulli extends BernoulliInstances

trait BernoulliInstances:
  given schrodingerRandomFairBernoulliForBoolean[F[_]: Functor: Random]
      : Bernoulli[0.5, Boolean][F] =
    val fair = Random[F].int.map(_ >= 0)
    _ => fair

  given schrodingerRandomBernoulliForDoubleBoolean[F[_]: Functor: Random](
      using Uniform.Aux[F, 0.0 <=@< 1.0, Double]): Bernoulli[Double, Boolean][F] with
    override def apply(params: Bernoulli.Params[Double]): F[Boolean] =
      import params.*
      Uniform(0.0 <=@< 1.0).map(_ < successProbability)
