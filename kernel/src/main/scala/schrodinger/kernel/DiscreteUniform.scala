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
import cats.Functor
import cats.Invariant
import cats.FlatMap
import cats.Reducible
import cats.kernel.Hash
import cats.syntax.all.*

trait DiscreteUniform[F[_], I]:
  def discreteUniform(n: I): F[I]

object DiscreteUniform:
  inline def apply[F[_], I](n: I)(using u: DiscreteUniform[F, I]): F[I] =
    u.discreteUniform(n)

  def apply[F[_], G[_]: Reducible, A: Hash](support: G[A])(
      using F: Priority[Functor[F], InvariantAndHash[F, A]],
      u: DiscreteUniform[F, Long]): F[A] =
    F match
      case Priority.Preferred(given Functor[f]) =>
        apply(support.size).map(support.get(_).get)
      case Priority.Fallback(InvariantAndHash(given Invariant[f], _)) =>
        val inv = support.toIterable.view.zipWithIndex.toMap
        apply(support.size).imap(support.get(_).get)(inv.getOrElse(_, -1))

  given [F[_]: FlatMap: Random]: DiscreteUniform[F, Long] with
    def discreteUniform(n: Long) =
      if (n & -n) == n then Random.long.map(_ & (n - 1))
      else
        Random
          .long
          .map { x =>
            val b = x >>> 1
            val v = b % n
            Option.when(b - v + (n - 1) >= 0)(v)
          }
          .untilDefinedM
