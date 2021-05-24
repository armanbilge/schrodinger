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

import cats.MonadError
import cats.syntax.all._
import org.scalacheck.{Arbitrary, Gen}
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import schrodinger.{RV, RVT}
import schrodinger.random.Uniform

import java.util.SplittableRandom

class SplitMixSpec extends Specification with ScalaCheck {

  val N = 100

  implicit val arbitraryState: Arbitrary[SplitMix] = Arbitrary(
    Gen.long.map(SplitMix(_, SplitMix.GoldenGamma))
  )

  def splittableRandom(state: SplitMix) =
    new SplittableRandom(state.seed)

  "SplitMix" should {

    "generate ints" in {
      prop { (state: SplitMix) =>
        val ints = List.fill(N)(Uniform.int[RV[SplitMix, *]]).sequence.simulate(state).value
        val random = splittableRandom(state)
        val expectedInts = List.fill(N)(random.nextInt())
        ints should_=== expectedInts
      }
    }

    "generate longs" in {
      prop { (state: SplitMix) =>
        val longs = List.fill(N)(Uniform.long[RV[SplitMix, *]]).sequence.simulate(state).value
        val random = splittableRandom(state)
        val expectedLongs = List.fill(N)(random.nextLong())
        longs should_=== expectedLongs
      }
    }

    "split" in {
      prop { (state: SplitMix) =>
        val ints =
          List
            .fill(N)(
              MonadError[RVT[Option, SplitMix, *], Unit]
                .raiseError[Int](())
                .handleErrorWith(_ => Uniform.int[RVT[Option, SplitMix, *]]))
            .sequence
            .simulate(state)
            .get
        val random = splittableRandom(state)
        val expectedInts = List.fill(N)(random.split().nextInt())
        ints should_=== expectedInts
      }
    }
  }

}
