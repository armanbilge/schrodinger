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

import algebra.ring.MultiplicativeGroup
import algebra.ring.Rig
import algebra.ring.Semifield
import cats.Applicative
import cats.syntax.all.given
import schrodinger.kernel.Bernoulli
import schrodinger.kernel.Density
import schrodinger.math.LogDouble
import schrodinger.math.Monus
import schrodinger.math.syntax.*

object bernoulli

trait BernoulliInstances:
  given schrodingerStatsFairBernoulliForBoolean[F[_]: Applicative]
      : Bernoulli[0.5, Boolean][Density[F, LogDouble]] =
    val half = LogDouble(0.5).pure[F]
    val fair: Density[F, LogDouble][Boolean] = _ => half
    _ => fair

  given schrodingerStatsBernoulliForDoubleBoolean[F[_]: Applicative]
      : Bernoulli[Double, Boolean][Density[F, LogDouble]] = params =>
    import params.*
    val success = LogDouble(successProbability).pure[F]
    val failure = LogDouble(1 - successProbability).pure[F]
    x => if x then success else failure

  given schrodingerStatsFairBernoulliForSemifieldBoolean[F[_]: Applicative, A](
      using A: Semifield[A]): Bernoulli[0.5, Boolean][Density[F, A]] =
    val half = A.reciprocal(A.fromInt(2)).pure
    val fair: Density[F, A][Boolean] = _ => half
    _ => fair

  given schrodingerStatsBernoulliForRigWithMonusBoolean[F[_]: Applicative, A: Monus](
      using A: Rig[A]): Bernoulli[A, Boolean][Density[F, A]] =
    case Bernoulli.Params(successProbability) =>
      val success = successProbability.pure
      val failure = (A.one ∸ successProbability).pure
      x => if x then success else failure
