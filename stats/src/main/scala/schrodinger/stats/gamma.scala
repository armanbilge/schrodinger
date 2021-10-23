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
import org.apache.commons.math3.distribution.GammaDistribution
import schrodinger.kernel.Density
import schrodinger.kernel.Gamma
import schrodinger.math.LogDouble

object gamma extends GammaInstances

trait GammaInstances:

  given schrodingerStatsGammaForDouble[F[_]: Applicative]
      : Gamma[Double][Density[F, LogDouble]] =
    case Gamma.Params(shape, rate) =>
      val distribution = new GammaDistribution(null, shape, 1.0 / rate)
      x => LogDouble.exp(distribution.logDensity(x)).pure[F]
