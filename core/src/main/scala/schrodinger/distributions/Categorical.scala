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

import cats.Functor
import schrodinger.RandomT

import java.util

final class Categorical[F[_], S, A, C] private[distributions] (
    impl: CategoricalImpl[F, S, A, C])
    extends Serializable {
  def apply(a: A): RandomT[F, S, C] = impl(a)
}

object Categorical {
  def apply[F[_], S](probabilities: Seq[Double])(
      implicit C: Categorical[F, S, Seq[Double], Int]): RandomT[F, S, Int] =
    C(probabilities)

  def apply[F[_]: Functor, S, A](categories: Map[A, Double])(
      implicit C: Categorical[F, S, Seq[Double], Int]): RandomT[F, S, A] = {
    val a = categories.keys.toIndexedSeq
    val p = categories.values.toSeq
    C(p).map(a)
  }

  implicit def schrodingerDistributionsCategorical[F[_], S, A, C](
      implicit
      impl: PrioritizeGenerator[CategoricalImpl[F, S, A, C]]): Categorical[F, S, A, C] =
    new Categorical(impl.join)
}

trait CategoricalImpl[F[_], S, A, C] extends Serializable {
  def apply(args: A): RandomT[F, S, C]
}

object CategoricalImpl {
  implicit def schrodingerDistributionsCategoricalForDoubleArray[F[_]: Functor, S](
      implicit U: Uniform[F, S, Unit, Double]): CategoricalImpl[F, S, Seq[Double], Int] =
    new CategoricalImpl[F, S, Seq[Double], Int] {
      override def apply(p: Seq[Double]): RandomT[F, S, Int] = {
        val cumulative = p.toArray
        var i = 1
        while (i < cumulative.length) {
          cumulative(i) += cumulative(i - 1)
          i += 1
        }
        U(()).map { U =>
          val i = util.Arrays.binarySearch(cumulative, U * cumulative(cumulative.length - 1))
          if (i >= 0) i else -(i + 1)
        }
      }
    }
}
