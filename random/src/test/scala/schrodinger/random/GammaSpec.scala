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

package schrodinger.random

import cats.syntax.all.*
import org.apache.commons.rng.core.source64
import org.apache.commons.rng.sampling.distribution.AhrensDieterMarsagliaTsangGammaSampler
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import schrodinger.kernel.Gamma
import schrodinger.kernel.testkit.PureRV
import schrodinger.kernel.testkit.SplitMix64
import schrodinger.random.all.given

class GammaSpec extends Specification with ScalaCheck:
  val N = 100

  "Gamma" should {
    "match Apache implementation" in {
      prop { (seed: Long, _shape: Double) =>
        val shape = 1/2.0
        val apache = new AhrensDieterMarsagliaTsangGammaSampler(new source64.SplitMix64(seed), math.abs(shape), 1.0)
        Gamma[PureRV[SplitMix64, _], Double, Double, Double](math.abs(shape), 1.0)
          .replicateA(N)
          .simulate(SplitMix64(seed))
          .value ===
          List.fill(N)(apache.sample())
      }
    }
  }
