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

import cats.syntax.all.*
import munit.ScalaCheckSuite
import org.apache.commons.rng.core.source64
import org.apache.commons.rng.sampling.distribution.AhrensDieterMarsagliaTsangGammaSampler
import org.apache.commons.rng.sampling.distribution.BoxMullerNormalizedGaussianSampler
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop.*
import schrodinger.kernel.testkit.PureRV
import schrodinger.kernel.testkit.SplitMix64

class GammaSuite extends ScalaCheckSuite {
  val N = 100

  case class GammaParams(shape: Double, rate: Double)

  given Arbitrary[GammaParams] =
    Arbitrary(
      for
        shape <- Gen.posNum[Double]
        rate <- Gen.posNum[Double]
      yield GammaParams(shape, rate),
    )

  property("match Apache implementation") {
    forAll { (seed: Long, params: GammaParams) =>
      val GammaParams(shape, rate) = params
      val apache =
        if shape < 1 then
          new AhrensDieterMarsagliaTsangGammaSampler(
            new source64.SplitMix64(seed),
            shape,
            1.0 / rate,
          )
        else {
          val splitMix = new source64.SplitMix64(seed)
          val gaussian = new BoxMullerNormalizedGaussianSampler(splitMix)
          new MarsagliaTsangGammaSampler(splitMix, gaussian, shape, 1.0 / rate)
        }

      Gamma[PureRV[SplitMix64, _], Double](shape, rate)
        .replicateA(N)
        .simulate(SplitMix64(seed))
        .value ===
        List.fill(N)(apache.sample())
    }
  }
}
