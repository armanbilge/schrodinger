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
import schrodinger.math.syntax.*

trait FairBernoulli[F[_], B]:
  def fairBernoulli: F[B]

trait Bernoulli[F[_], P, B] extends FairBernoulli[F, B]:
  def bernoulli(successProbability: P): F[B]

object Bernoulli:

  inline def apply[F[_], P, B](successProbability: P)(using b: Bernoulli[F, P, B]): F[B] =
    b.bernoulli(successProbability)

  inline def fair[F[_], B](using b: FairBernoulli[F, B]): F[B] = b.fairBernoulli

  trait Default[F[_], P](using P: Semifield[P]) extends Bernoulli[F, P, Boolean]:
    def fairBernoulli = bernoulli(P.fromInt(2).reciprocal)
