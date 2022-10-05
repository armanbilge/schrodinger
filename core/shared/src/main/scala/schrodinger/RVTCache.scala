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

import cats.~>
import cats.effect.kernel.Sync

final class RVTCache[F[_], S, A](private[schrodinger] val default: A):
  val get: RVT[F, S, A] = RVT.Retrieve(this)
  def set(value: A): RVT[F, S, Unit] = RVT.Store(this, value)

object RVTCache:
  def in[F[_], G[_], S, A](default: A)(using F: Sync[F]): F[RVTCache[G, S, A]] =
    F.delay(RVTCache[G, S, A](default))
