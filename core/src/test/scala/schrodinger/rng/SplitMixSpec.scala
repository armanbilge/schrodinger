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

package schrodinger
package unsafe.rng

import cats.syntax.all.*
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

import java.util.SplittableRandom

class SplitMixSpec extends Specification with ScalaCheck:

  val N = 100

  given Arbitrary[SplitMix] = Arbitrary(
    Gen.long.map(SplitMix(_, SplitMix.GoldenGamma))
  )

  "SplitMix" should {

    "generate ints" in {
      prop { (state: SplitMix) =>
        val ints = RV.int[SplitMix].replicateA(N).simulate(state)
        val random = new SplittableRandom(state.seed)
        val expectedInts = List.fill(N)(random.nextInt())
        ints === expectedInts
      }
    }

    "generate longs" in {
      prop { (state: SplitMix) =>
        val longs = RV.long[SplitMix].replicateA(N).simulate(state)
        val random = new SplittableRandom(state.seed)
        val expectedLongs = List.fill(N)(random.nextLong())
        longs === expectedLongs
      }
    }

    "split" in {
      prop { (state: SplitMix) =>
        val ints = RV.int[SplitMix].split.semiflatMap(identity).replicateA(N).simulate(state)
        val random = new SplittableRandom(state.seed)
        val expectedInts = List.fill(N)(random.split().nextInt())
        ints === expectedInts
      }
    }
  }
