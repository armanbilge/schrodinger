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
import org.apache.commons.math3.distribution.NormalDistribution
import schrodinger.kernel.Gaussian
import schrodinger.math.LogDouble

object gaussian:
  given [F[_]: Applicative]: Gaussian[Density[F, LogDouble, _], Double] with
    def gaussian = gaussian(0, 1)
    def gaussian(mean: Double, standardDeviation: Double) =
      val distribution = new NormalDistribution(null, mean, standardDeviation)
      Density(x => LogDouble.exp(distribution.logDensity(x)).pure)
