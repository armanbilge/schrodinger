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
import munit.ScalaCheckSuite
import org.apache.commons.rng.core.source64
import org.apache.commons.rng.sampling.distribution.ChengBetaSampler
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop.*
import schrodinger.kernel.Beta
import schrodinger.kernel.testkit.PureRV
import schrodinger.kernel.testkit.SplitMix64

class BetaSuite extends ScalaCheckSuite:
  val N = 100

  case class BetaParams(alpha: Double, beta: Double)

  given Arbitrary[BetaParams] =
    Arbitrary(
      for
        alpha <- Gen.posNum[Double]
        beta <- Gen.posNum[Double]
      yield BetaParams(alpha, beta)
    )

  property("match Apache implementation") {
    forAll { (seed: Long, params: BetaParams) =>
      val apache =
        new ChengBetaSampler(new source64.SplitMix64(seed), params.alpha, params.beta)
      Beta[PureRV[SplitMix64, _], Double](params.alpha, params.beta)
        .replicateA(N)
        .simulate(SplitMix64(seed))
        .value ===
        List.fill(N)(apache.sample())
    }
  }
