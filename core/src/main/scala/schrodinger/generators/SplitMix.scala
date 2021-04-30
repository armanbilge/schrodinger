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

import cats.data.StateT
import cats.{Applicative, Eq, Id}
import schrodinger.distributions.{GeneratorOverride, UniformImpl}
import schrodinger.{Random, RandomT}

import java.lang

final case class SplitMix private (seed: Long, gamma: Long, extra: Double = Double.NaN)

object SplitMix extends SplitMixInstances {

  /**
   * Uses two longs (such as `currentTimeMillis` and `nanoTime`) to create an initial state.
   */
  def initialState(currentTimeMillis: Long, nanoTime: Long): SplitMix = {
    val s = mix64(currentTimeMillis) ^ mix64(nanoTime)
    SplitMix(mix64(s), mixGamma(s + GoldenGamma))
  }

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

  private val nextSeed = StateT.modify[Id, SplitMix](s => s.copy(seed = s.seed + s.gamma))
  private[generators] val nextInt = Random(nextSeed.inspect(s => mix32(s.seed)))
  private[generators] val nextLong = Random(nextSeed.inspect(s => mix64(s.seed)))
  private[generators] val nextGamma = Random(nextSeed.inspect(s => mixGamma(s.seed)))
  private[generators] val split = for {
    seed <- nextLong
    gamma <- nextGamma
  } yield SplitMix(seed, gamma, Double.NaN)
}

private[schrodinger] sealed class SplitMixInstances {
  implicit def schrodingerGeneratorsEqForSplitMix: Eq[SplitMix] =
    Eq.fromUniversalEquals

  implicit def schrodingerGeneratorsUniformIntForSplitMix[F[_]](
      implicit F: Applicative[F]): UniformImpl[F, SplitMix, Unit, Int] with GeneratorOverride =
    new UniformImpl[F, SplitMix, Unit, Int] with GeneratorOverride {
      override def apply(args: Unit): RandomT[F, SplitMix, Int] =
        RandomT.fromRandom(SplitMix.nextInt)
    }

  implicit def schrodingerGeneratorsUniformLongForSplitMix[F[_]](
      implicit F: Applicative[F]): UniformImpl[F, SplitMix, Unit, Long] with GeneratorOverride =
    new UniformImpl[F, SplitMix, Unit, Long] with GeneratorOverride {
      override def apply(args: Unit): RandomT[F, SplitMix, Long] =
        RandomT.fromRandom(SplitMix.nextLong)
    }

  implicit def schrodingerGeneratorsSplitForSplitMix[F[_]](
      implicit F: Applicative[F]): Split[F, SplitMix] with GeneratorOverride =
    new Split[F, SplitMix] with GeneratorOverride {
      override def split: RandomT[F, SplitMix, SplitMix] =
        RandomT.fromRandom(SplitMix.split)
    }
}
