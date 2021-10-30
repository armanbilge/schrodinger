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

import cats.Applicative
import cats.syntax.all.*
import org.apache.commons.math3.distribution.BetaDistribution
import schrodinger.kernel.Density
import schrodinger.kernel.Beta
import schrodinger.math.LogDouble

object beta extends BetaInstances

trait BetaInstances:

  given schrodingerStatsBetaForDouble[F[_]: Applicative]: Beta[Double][Density[F, LogDouble]] =
    case Beta.Params(alpha, beta) =>
      val distribution = new BetaDistribution(null, alpha, beta)
      x => LogDouble.exp(distribution.logDensity(x)).pure[F]