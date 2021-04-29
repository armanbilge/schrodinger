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

package schrodinger.distributions

import cats.Applicative
import schrodinger.{Dist, DistT}

import java.util

object Categorical {

  private def int(p: Array[Double]): Dist[Int] = {
    val cumulative = p
    var i = 1
    while (i < cumulative.length) {
      cumulative(i) += cumulative(i - 1)
      i += 1
    }
    Uniform.double.map { U =>
      val i = util.Arrays.binarySearch(cumulative, U * cumulative(cumulative.length - 1))
      if (i >= 0) i else -(i + 1)
    }
  }

  def int(p: Seq[Double]): Dist[Int] =
    int(p.toArray)

  def intF[F[_]: Applicative](p: IndexedSeq[Double]): DistT[F, Int] =
    DistT.fromDist(int(p))

  def apply[A](categories: Map[A, Double]): Dist[A] = {
    val a = categories.keys.toIndexedSeq
    val p = categories.values.toArray
    int(p).map(a)
  }

  def applyF[F[_]: Applicative, A](categories: Map[A, Double]): DistT[F, A] =
    DistT.fromDist(apply(categories))

}
