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
import munit.ScalaCheckSuite
import org.apache.commons.rng.core.source32.PcgXshRr32 as ApachePcgXshRr32
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop.*

class PcgSpec extends ScalaCheckSuite:

  val N = 100

  given Arbitrary[Pcg32] = Arbitrary(
    for
      state <- Gen.long
      inc <- Gen.long
    yield Pcg32(state, inc | 1)
  )

  property("Pcg32XshRr generate ints") {
    forAll { (state: Pcg32) =>
      val ints = RV.int[Pcg32].replicateA(N + 1).simulate(state).tail
      val provider = new ApachePcgXshRr32(Array(state.state - state.inc, state.inc >>> 1))
      val expectedInts = List.fill(N)(provider.nextInt())
      ints === expectedInts
    }
  }
