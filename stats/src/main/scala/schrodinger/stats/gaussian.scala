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
import schrodinger.math.LogDouble

object gaussian {
  private final val `log(2π)/2` = 0.9189385332046727417803297

  given [F[_]](using F: Applicative[F]): Gaussian[Density[F, LogDouble, _], Double] with {
    def gaussian = gaussian(0, 1)
    def gaussian(mean: Double, standardDeviation: Double) = {
      val `σ√2π` = LogDouble(standardDeviation) * LogDouble.exp(`log(2π)/2`)
      Density { x =>
        F.pure {
          val z = (x - mean) / standardDeviation
          LogDouble.exp(-0.5 * z * z) / `σ√2π`
        }
      }
    }
  }
}
