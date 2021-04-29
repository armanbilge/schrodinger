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

package schrodinger.generators

import cats.{Eq, Id}
import cats.data.StateT
import schrodinger.generators.SplitMix.{mix32, mix64, mixGamma, State}

import java.lang

class SplitMix extends Generator[State] with BoxMullerGaussian[State] {

  private val nextSeed = StateT.modify[Id, State](s => s.copy(seed = s.seed + s.gamma))

  final override val nextInt: StateT[Id, State, Int] = nextSeed.inspect(s => mix32(s.seed))

  final override val nextLong: StateT[Id, State, Long] = nextSeed.inspect(s => mix64(s.seed))

  final private val nextGamma = nextSeed.inspect(s => mixGamma(s.seed))

  final override val split: StateT[Id, State, State] = for {
    seed <- nextLong
    gamma <- nextGamma
  } yield State(seed, gamma, Double.NaN)

  override protected val getExtraGaussian: StateT[Id, State, Double] =
    StateT.inspect(_.extra)

  override protected def setExtraGaussian(x: Double): StateT[Id, State, Unit] =
    StateT.modify(_.copy(extra = x))

}

object SplitMix {

  implicit val DefaultInstance: SplitMix = new SplitMix

  private[generators] val GoldenGamma = 0x9e3779b97f4a7c15L

  private def mix64(_z: Long) = {
    var z = _z
    z = (z ^ (z >>> 30)) * 0xbf58476d1ce4e5b9L
    z = (z ^ (z >>> 27)) * 0x94d049bb133111ebL
    z ^ (z >>> 31)
  }

  private def mix32(_z: Long) = {
    var z = _z
    z = (z ^ (z >>> 33)) * 0x62a9d9ed799705f5L
    (((z ^ (z >>> 28)) * 0xcb24d0a5c88c35b3L) >>> 32).toInt
  }

  private def mixGamma(_z: Long) = {
    var z = _z
    z = (z ^ (z >>> 33)) * 0xff51afd7ed558ccdL

    z = (z ^ (z >>> 33)) * 0xc4ceb9fe1a85ec53L
    z = (z ^ (z >>> 33)) | 1L

    val n = lang.Long.bitCount(z ^ (z >>> 1))
    if (n < 24) z ^ 0xaaaaaaaaaaaaaaaaL
    else z
  }

  final case class State private (seed: Long, gamma: Long, extra: Double = Double.NaN)

  implicit def eqState: Eq[State] = Eq.fromUniversalEquals

  /**
   * Uses two longs (such as `currentTimeMillis` and `nanoTime`) to create an initial state.
   */
  def initialState(currentTimeMillis: Long, nanoTime: Long): State = {
    val s = mix64(currentTimeMillis) ^ mix64(nanoTime)
    State(mix64(s), mixGamma(s + GoldenGamma))
  }

}
