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

object bernoulli extends BernoulliInstances

trait BernoulliInstances:
  given schrodingerRandomBernoulliForDoubleBoolean[F[_]: Functor: Random](
      using Uniform[F, Unit, Double]): Bernoulli[F, Double, Boolean] with
    override val fair: F[Boolean] = Random[F].int.map(_ >= 0)
    override def apply(successProbability: Double): F[Boolean] =
      Uniform.standard.map(_ < successProbability)
