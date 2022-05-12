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

import cats.effect.SyncIO
import cats.syntax.all.*
import org.typelevel.vault.Key
import schrodinger.kernel.testkit.PureRV
import schrodinger.kernel.testkit.SplitMix64

given GaussianCache[PureRV[SplitMix64, _], Double] with
  val key = Key.newKey[SyncIO, Double].unsafeRunSync()
  def getAndClear: PureRV[SplitMix64, Option[Double]] =
    PureRV.getExtra(key) <* PureRV.setExtra(key, None)
  def set(a: Double): PureRV[SplitMix64, Unit] = PureRV.setExtra(key, Some(a))
