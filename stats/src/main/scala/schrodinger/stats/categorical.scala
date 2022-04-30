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

import algebra.ring.Semifield
import cats.Applicative
import cats.Foldable
import cats.syntax.all.*
import schrodinger.kernel.Categorical
import schrodinger.kernel.Density
import schrodinger.math.LogDouble
import schrodinger.math.syntax.*

object categorical extends CategoricalInstances

trait CategoricalInstances:
  given [F[_]: Applicative, G[_]: Foldable]
      : Categorical[G[LogDouble], Int][Density[F, LogDouble]] =
    case Categorical.Params(support) =>
      val sum = LogDouble.sum(support.toIterable)
      i => (support.get(i).getOrElse(LogDouble.Zero) / sum).pure[F]

  given [F[_]: Applicative, G[_]: Foldable, A](
      using A: Semifield[A]): Categorical[G[A], Int][Density[F, A]] =
    case Categorical.Params(support) =>
      val sum = A.sum(support.toIterable)
      i => (support.get(i).getOrElse(A.zero) / sum).pure

  given schrodingerStatsCategoricalForIArrayLogDouble[F[_]: Applicative]
      : Categorical[IArray[LogDouble], Int][Density[F, LogDouble]] = params =>
    import params.*
    val sum = LogDouble.sum(support)
    i => (support(i) / sum).pure[F]

  given [F[_]: Applicative, W, A](
      using W: Semifield[W]): Categorical[Map[A, W], A][Density[F, W]] = params =>
    import params.*
    val sum = W.sum(support.values)
    a => (support.get(a).fold(W.zero)(_ / sum)).pure
