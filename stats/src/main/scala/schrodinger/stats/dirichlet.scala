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

import cats.syntax.all.*
import cats.instances.vector.*
import cats.Applicative
import schrodinger.kernel.Density
import schrodinger.kernel.Dirichlet
import schrodinger.math.LogDouble
import org.apache.commons.math3.special.Gamma.logGamma

object dirichlet extends DirichletInstances

trait DirichletInstances:
  given schrodingerStatsExponentialForVectorDouble[F[_]: Applicative]
      : Dirichlet[Vector[Double], Vector[Double]][Density[F, LogDouble]] =
    case Dirichlet.Params(concentration) =>
      def Gamma(x: Double) = LogDouble.exp(logGamma(x))
      val normalization = concentration.map(Gamma).reduce(_ * _) / Gamma(concentration.sum)
      x =>
        ((concentration, x)
          .parMapN((alpha, x) => LogDouble(x) ** (alpha - 1))
          .reduce(_ * _) / normalization).pure[F]
