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
package random

import cats.Functor
import cats.syntax.all.*
import schrodinger.kernel.Categorical
import schrodinger.kernel.Uniform
import schrodinger.math.Interval.*
import schrodinger.math.LogDouble
import scala.reflect.ClassTag

object categorical extends CategoricalInstances

trait CategoricalInstances:
  given schrodingerRandomCategoricalForSeqDouble[F[_]: Functor: Uniform[0.0 <=@< 1.0, Double]]
      : Categorical[Seq[Double], Int][F] =
    case Categorical.Params(support) =>
      val cumulative = support.toArray
      var i = 1
      while i < cumulative.length do
        cumulative(i) += cumulative(i - 1)
        i += 1
      Uniform(0.0 <=@< 1.0).map { U =>
        val i =
          java.util.Arrays.binarySearch(cumulative, U * cumulative(cumulative.length - 1))
        if i >= 0 then i else -(i + 1)
      }

  given schrodingerRandomCategoricalForSeqLogDouble[
      F[_]: Functor: Uniform[0.0 <=@< 1.0, Double]]: Categorical[Seq[LogDouble], Int][F] =
    case Categorical.Params(support) =>
      val cumulative = new Array[Double](support.size)
      var max = LogDouble.Zero
      var i = 0
      while i < cumulative.length do
        max = max.max(support(i))
        i += 1
      cumulative(0) = (support(0) / max).real
      i = 1
      while i < cumulative.length do
        cumulative(i) = (support(i) / max).real + cumulative(i - 1)
        i += 1
      Uniform(0.0 <=@< 1.0).map { U =>
        val i =
          java.util.Arrays.binarySearch(cumulative, U * cumulative(cumulative.length - 1))
        if i >= 0 then i else -(i + 1)
      }

  given [F[_]: Functor: Categorical[Vector[R], Int], R, A]: Categorical[Map[A, R], A][F] with
    override def apply(params: Categorical.Params[Map[A, R]]): F[A] =
      import params.*

      val as = Vector.newBuilder[A]
      as.sizeHint(support)
      val ps = Vector.newBuilder[R]
      ps.sizeHint(support)
      support.foreach {
        case a -> p =>
          as += a
          ps += p
      }

      val a = as.result
      Categorical(ps.result).map(a)
