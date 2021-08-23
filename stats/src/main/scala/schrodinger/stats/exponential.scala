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
import cats.syntax.all.given
import schrodinger.kernel.Density
import schrodinger.kernel.Exponential

object exponential extends ExponentialInstances

trait ExponentialInstances:

  given schrodingerStatsExponentialForDouble[F[_]: Applicative]
      : Exponential[Double][Density[F, LogDouble]] with
    override def apply(params: Exponential.Params[Double]) =
      import params.*
      val logRate = LogDouble(rate)
      x => (logRate * LogDouble.exp(-rate * x)).pure[F]
