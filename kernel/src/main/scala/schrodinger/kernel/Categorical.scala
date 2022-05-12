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

import algebra.Priority
import algebra.ring.AdditiveMonoid
import algebra.ring.Semiring
import cats.Functor
import cats.Invariant
import cats.NonEmptyTraverse
import cats.Reducible
import cats.data.Chain
import cats.data.NonEmptyVector
import cats.kernel.Hash
import cats.kernel.Order
import cats.syntax.all.*
import schrodinger.math.syntax.*

trait Categorical[F[_], V, I]:
  def categorical(probabilites: V): F[I]

object Categorical:
  inline def apply[F[_], V, I](probabilites: V)(
      using c: Categorical[F, V, I]
  ): F[I] = c.categorical(probabilites)

  def apply[F[_], P, G[_]: NonEmptyTraverse, A](support: G[(A, P)])(
      using F: Priority[Functor[F], (Invariant[F], Hash[A])],
      P: Semiring[P],
      c: Categorical[F, G[P], Long]
  ): F[A] =
    val probabilities = support.map(_._2)
    F match
      case Priority.Preferred(given Functor[f]) =>
        apply(probabilities).map(support.get(_).get._1)
      case Priority.Fallback((given Invariant[f], _)) =>
        val inv = support.toIterable.view.map(_._1).zipWithIndex.toMap
        apply(probabilities).imap(support.get(_).get._1)(inv.getOrElse(_, -1))

  given [F[_]: Functor, G[_]: Reducible, P: Order](
      using P: AdditiveMonoid[P],
      u: Uniform[F, P]): Categorical[F, G[P], Long] with
    def categorical(probabilities: G[P]) =
      val cumSum =
        probabilities.reduceLeftTo(Chain.one(_))((c, p) => c :+ (c.lastOption.get + p)).toVector
      val sum = cumSum.last
      Uniform(P.zero, sum).map(
        cumSum.search(_)(using Order[P].toOrdering).insertionPoint
      )
