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

import org.apache.commons.rng.core.source64.SplitMix64
import org.apache.commons.rng.sampling.distribution.AhrensDieterExponentialSampler
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import schrodinger.RV
import schrodinger.kernel.Exponential
import schrodinger.random.all.given

class ExponentialSpec extends Specification with ScalaCheck:

  "Exponential" should {
    "generate standard exponential variates" in {
      prop { (rng: SplitMix64) =>
        val x = Exponential.standard[RV[SplitMix64, _], Double].simulate(rng).value
        val y = new AhrensDieterExponentialSampler(rng, 1.0).sample()
        x should_=== y
      }
    }
  }
