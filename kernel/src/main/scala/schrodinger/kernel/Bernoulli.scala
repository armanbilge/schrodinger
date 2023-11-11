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

import cats.Functor
import cats.kernel.Order
import cats.syntax.all.*

trait FairBernoulli[F[_], B]:
  def fairBernoulli: F[B]

object FairBernoulli:
  given [F[_]: Functor: Random]: FairBernoulli[F, Boolean] with
    def fairBernoulli = Random.int.map(_ >= 0)

trait Bernoulli[F[_], P, B]:
  def bernoulli(successProbability: P): F[B]

object Bernoulli:

  inline def apply[F[_], P, B](successProbability: P)(using b: Bernoulli[F, P, B]): F[B] =
    b.bernoulli(successProbability)

  inline def fair[F[_], B](using b: FairBernoulli[F, B]): F[B] = b.fairBernoulli

  given [F[_]: Functor, P: Order](using Uniform[F, P]): Bernoulli[F, P, Boolean] with
    def bernoulli(successProbability: P) = Uniform.standard.map(_ < successProbability)
