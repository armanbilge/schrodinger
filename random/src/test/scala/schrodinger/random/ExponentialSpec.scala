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
import org.apache.commons.rng.sampling.distribution.AhrensDieterExponentialSampler
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import schrodinger.kernel.Exponential
import schrodinger.kernel.testkit.PureRV
import schrodinger.kernel.testkit.SplitMix64
import schrodinger.random.all.given

class ExponentialSpec extends Specification with ScalaCheck:
  val N = 100

  "Exponential" should {
    "match Apache implementation" in {
      prop { (seed: Long) =>
        val apache = new AhrensDieterExponentialSampler(new source64.SplitMix64(seed), 1.0)
        Exponential
          .standard[PureRV[SplitMix64, _], Double]
          .replicateA(N)
          .simulate(SplitMix64(seed))
          .value ===
          List.fill(N)(apache.sample())
      }
    }
  }
