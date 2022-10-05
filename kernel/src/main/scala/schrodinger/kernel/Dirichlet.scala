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

package schrodinger.kernel

import algebra.ring.Semifield
import cats.Apply
import cats.NonEmptyTraverse
import cats.syntax.all.*
import schrodinger.math.syntax.*

trait Dirichlet[F[_], V]:
  def dirichlet(concentration: V): F[V]

object Dirichlet:
  inline def apply[F[_], V](concentration: V)(using d: Dirichlet[F, V]): F[V] =
    d.dirichlet(concentration)

  given [F[_]: Apply, G[_]: NonEmptyTraverse, A](using
      A: Semifield[A],
      g: Gamma[F, A],
  ): Dirichlet[F, G[A]] with
    def dirichlet(concentration: G[A]) =
      concentration.nonEmptyTraverse(Gamma(_, A.one)).map { xs =>
        val sum = A.sum(xs.toIterable)
        xs.map(_ / sum)
      }
