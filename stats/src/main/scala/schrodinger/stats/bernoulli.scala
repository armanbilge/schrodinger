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

package schrodinger.stats

import schrodinger.kernel.Density
import schrodinger.kernel.Bernoulli
import cats.Applicative
import cats.syntax.all.given

object bernoulli

trait BernoulliInstances:
  given schrodingerStatsBernoulliForDoubleBoolean[F[_]: Applicative]
      : Bernoulli[Density[F, LogDouble], Double, Boolean] with
    private val half = LogDouble(0.5).pure[F]
    override val fair = _ => half
    override def apply(successProbability: Double) =
      val success = LogDouble(successProbability).pure[F]
      val failure = LogDouble(1 - successProbability).pure[F]
      x => if x then success else failure
