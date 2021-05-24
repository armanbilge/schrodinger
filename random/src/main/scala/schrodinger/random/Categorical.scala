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
import cats.syntax.all._
import schrodinger.kernel.Random

object Categorical {

  def apply[F[_]: Functor: Random](weights: Seq[Double]): F[Int] = {
    val cumulative = weights.toArray
    var i = 1
    while (i < cumulative.length) {
      cumulative(i) += cumulative(i - 1)
      i += 1
    }
    Uniform.double.map { U =>
      val i = java.util.Arrays.binarySearch(cumulative, U * cumulative(cumulative.length - 1))
      if (i >= 0) i else -(i + 1)
    }
  }

  def apply[F[_]]: ApplyMapPartiallyApplied[F] =
    new ApplyMapPartiallyApplied[F]

  final private[random] class ApplyMapPartiallyApplied[F[_]](val dummy: Boolean = false)
      extends AnyVal {
    def apply[A](categories: Map[A, Double])(implicit F: Functor[F], R: Random[F]): F[A] = {
      val weights = categories.values.toSeq
      val values = categories.keys.toIndexedSeq
      Categorical(weights).map(values)
    }
  }
}
