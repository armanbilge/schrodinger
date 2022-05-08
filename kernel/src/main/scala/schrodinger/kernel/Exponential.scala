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

import algebra.ring.MultiplicativeMonoid

trait Exponential[F[_], A]:
  def exponential: F[A]
  def exponential(rate: A): F[A]

object Exponential:
  inline def apply[F[_], A](rate: A)(using e: Exponential[F, A]): F[A] =
    e.exponential(rate)

  inline def standard[F[_], A](using e: Exponential[F, A]): F[A] =
    e.exponential

  trait Default[F[_], A](using A: MultiplicativeMonoid[A]) extends Exponential[F, A]:
    def exponential = exponential(A.one)
