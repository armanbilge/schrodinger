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

import algebra.instances.all.*
import algebra.ring.AdditiveMonoid
import cats.Applicative
import cats.NonEmptyParallel
import cats.NonEmptyTraverse
import cats.syntax.all.*
import schrodinger.kernel
import schrodinger.kernel.Dirichlet
import schrodinger.math.LogDouble
import schrodinger.math.special.gamma

object dirichlet:
  given [F[_]: Applicative, G[_]: NonEmptyTraverse: NonEmptyParallel]
      : Dirichlet[Density[F, LogDouble, _], G[Double]] with
    def dirichlet(concentration: G[Double]) =

      val normalizingConstant =
        gamma(AdditiveMonoid[Double].sum(concentration.toIterable))
          / concentration.reduceMap(gamma)(_ * _)

      Density { x =>
        val density =
          (concentration, x).parMapN((alpha, x) => LogDouble(x) ** (alpha - 1)).reduce(_ * _)
        (normalizingConstant * density).pure
      }
