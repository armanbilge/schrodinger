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

import cats.instances.list._
import cats.syntax.traverse._
import org.apache.commons.rng.core.source32.{PcgXshRr32 => ApachePcgXshRr32}
import org.scalacheck.Arbitrary
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import schrodinger.generators.Pcg.State64

class PcgSpec extends Specification with ScalaCheck {

  val N = 100
  val pcgXshRr32 = new Pcg32XshRr

  implicit val arbitraryState64 = Arbitrary(
    for {
      state <- Arbitrary.arbLong.arbitrary
      inc <- Arbitrary.arbLong.arbitrary
    } yield State64(state, inc | 1)
  )

  "Pcg32XshRr" should {
    "generate ints" in {
      prop { state: State64 =>
        val ints = List.fill(N + 1)(pcgXshRr32.nextInt).sequence.runA(state).tail
        val provider = new ApachePcgXshRr32(Array(state.state - state.inc, state.inc >>> 1))
        val expectedInts = List.fill(N)(provider.nextInt())
        ints should_=== expectedInts
      }
    }
  }

}
