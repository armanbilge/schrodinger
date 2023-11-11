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

import cats.Id
import cats.Monad
import cats.effect.SyncIO
import cats.effect.kernel.Sync

trait Simulator[F[_]: Monad]:
  type G[_]
  given runtime: Sync[G]
  def upgrade[A](fa: F[A]): G[A]
  def downgrade[A](ga: G[A]): F[A]

object Simulator extends SimulatorLowPriority:
  given [F[_]](using F: Sync[F]): Simulator[F] with
    type G[A] = F[A]
    given runtime: Sync[G] = F
    def upgrade[A](fa: F[A]): G[A] = fa
    def downgrade[A](ga: G[A]): F[A] = ga

sealed abstract private[schrodinger] class SimulatorLowPriority:
  given Simulator[Id] with
    type G[A] = SyncIO[A]
    given runtime: Sync[G] = SyncIO.syncForSyncIO
    def upgrade[A](a: A): SyncIO[A] = SyncIO.pure(a)
    def downgrade[A](ga: SyncIO[A]): A = ga.unsafeRunSync()
