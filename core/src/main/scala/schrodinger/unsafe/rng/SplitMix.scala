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

package schrodinger.unsafe.rng

import cats.Applicative
import cats.effect.kernel.Clock
import cats.syntax.all.*

import java.lang

final class SplitMix(private[rng] var seed: Long, val gamma: Long) extends Serializable

object SplitMix:

  def fromTime[F[_]: Applicative: Clock]: F[SplitMix] =
    (Clock[F].realTime.map(_.toMillis), Clock[F].monotonic.map(_.toNanos)).mapN {
      (currentTimeMillis, nanoTime) =>
        val s = mix64(currentTimeMillis) ^ mix64(nanoTime)
        SplitMix(mix64(s), mixGamma(s + GoldenGamma))
    }

  given SplittableRng[SplitMix] with
    extension (s: SplitMix)

      override def copy(): SplitMix = SplitMix(s.seed, s.gamma)

      override def nextInt(): Int =
        s.seed += s.gamma
        mix32(s.seed)

      override def nextLong(): Long =
        s.seed += s.gamma
        mix64(s.seed)

      private def nextGamma(): Long =
        s.seed += s.gamma
        mixGamma(s.seed)

      override def split(): SplitMix =
        val seed = s.nextLong()
        val gamma = s.nextGamma()
        SplitMix(seed, gamma)

  val GoldenGamma = 0x9e3779b97f4a7c15L

  private def mix64(_z: Long) =
    var z = _z
    z = (z ^ z >>> 30) * 0xbf58476d1ce4e5b9L
    z = (z ^ z >>> 27) * 0x94d049bb133111ebL
    z ^ z >>> 31

  private def mix32(_z: Long) =
    var z = _z
    z = (z ^ z >>> 33) * 0x62a9d9ed799705f5L
    ((z ^ z >>> 28) * 0xcb24d0a5c88c35b3L >>> 32).toInt

  private def mixGamma(_z: Long) =
    var z = _z
    z = (z ^ z >>> 33) * 0xff51afd7ed558ccdL

    z = (z ^ z >>> 33) * 0xc4ceb9fe1a85ec53L
    z = z ^ z >>> 33 | 1L

    val n = lang.Long.bitCount(z ^ z >>> 1)
    if n < 24 then z ^ 0xaaaaaaaaaaaaaaaaL
    else z
