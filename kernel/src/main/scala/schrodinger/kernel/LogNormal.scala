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

trait LogNormal[F[_], A]:
  def logNormal: F[A]
  def logNormal(mu: A, sigma: A): F[A]

object LogNormal:
  inline def apply[F[_], A](mu: A, sigma: A)(using ln: LogNormal[F, A]): F[A] =
    ln.logNormal(mu, sigma)

  inline def standard[F[_], A](using ln: LogNormal[F, A]): F[A] =
    ln.logNormal

  trait Default[F[_], A](using A: Rig[A]) extends LogNormal[F, A]:
    def logNormal = logNormal(A.zero, A.one)
