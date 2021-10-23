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
import cats.syntax.all.*
import schrodinger.kernel.Gaussian
import schrodinger.kernel.LogNormal

object lognormal extends LogNormalInstances

trait LogNormalInstances:
  given schrodingerRandomLogNormalForDouble[F[_]: Functor: Gaussian[Double]]
      : LogNormal[Double][F] =
    case LogNormal.Params(mu, sigma) => Gaussian(mu, sigma).map(math.exp)
