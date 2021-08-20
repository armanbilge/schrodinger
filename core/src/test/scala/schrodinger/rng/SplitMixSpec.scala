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

package schrodinger.rng

import cats.syntax.all.given
import org.scalacheck.{Arbitrary, Gen}
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import schrodinger.RV
import schrodinger.kernel.Random

import java.util.SplittableRandom

class SplitMixSpec extends Specification with ScalaCheck:

  val N = 100

  given Arbitrary[SplitMix] = Arbitrary(
    Gen.long.map(SplitMix(_, SplitMix.GoldenGamma))
  )

  def splittableRandom(state: SplitMix) =
    new SplittableRandom(state.seed)

  "SplitMix" should {

    "generate ints" in {
      prop { (state: SplitMix) =>
        val ints = List.fill(N)(Random[RV[SplitMix, _]].int).sequence.simulate(state).value
        val random = splittableRandom(state)
        val expectedInts = List.fill(N)(random.nextInt())
        ints should_=== expectedInts
      }
    }

    "generate longs" in {
      prop { (state: SplitMix) =>
        val longs = List.fill(N)(Random[RV[SplitMix, _]].long).sequence.simulate(state).value
        val random = splittableRandom(state)
        val expectedLongs = List.fill(N)(random.nextLong())
        longs should_=== expectedLongs
      }
    }

    "split" in {
      prop { (state: SplitMix) =>
        val random = splittableRandom(state)
        import SplitMix.schrodingerRngSplittableRngForSplitMix.given
        val ints = List.fill(N)(state.unsafeSplit().unsafeNextInt())
        val expectedInts = List.fill(N)(random.split().nextInt())
        ints should_=== expectedInts
      }
    }
  }
