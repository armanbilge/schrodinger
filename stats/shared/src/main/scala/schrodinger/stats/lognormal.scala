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
import schrodinger.kernel.Gaussian
import schrodinger.kernel.LogNormal
import schrodinger.math.LogDouble

object logNormal:
  given [F[_]: Applicative](using
      gaussian: Gaussian[Density[F, LogDouble, _], Double],
  ): LogNormal[Density[F, LogDouble, _], Double] with
    def logNormal = logNormal(0, 1)
    def logNormal(mu: Double, sigma: Double) =
      val delegate = gaussian.gaussian(mu, sigma)
      Density { x =>
        val logx = Math.log(x)
        delegate(logx).map(_ / LogDouble.exp(logx))
      }
