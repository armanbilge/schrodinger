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

import algebra.ring.Rig

trait Gaussian[F[_], A]:
  def gaussian: F[A]
  def gaussian(mean: A, standardDeviation: A): F[A]

object Gaussian:
  inline def apply[F[_], A](mean: A, standardDeviation: A)(using g: Gaussian[F, A]): F[A] =
    g.gaussian(mean, standardDeviation)

  inline def standard[F[_], A](using g: Gaussian[F, A]): F[A] = g.gaussian

  trait Default[F[_], A](using A: Rig[A]) extends Gaussian[F, A]:
    def gaussian = gaussian(A.zero, A.one)
