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
import org.scalacheck.Arbitrary
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import schrodinger.generators.SplitMix.State

import java.util.SplittableRandom
import scala.annotation.nowarn
import scala.collection.JavaConverters._

class SplitMixSpec extends Specification with ScalaCheck {

  val N = 100
  val splitMix = SplitMix.DefaultInstance

  implicit val arbitraryState = Arbitrary(
    Arbitrary.arbLong.arbitrary.map(State(_, SplitMix.GoldenGamma))
  )

  def splittableRandom(state: State) =
    new SplittableRandom(state.seed)

  "SplitMix" should {

    "generate ints" in {
      prop { state: State =>
        val ints = List.fill(N)(splitMix.nextInt).sequence.runA(state)
        val expectedInts =
          splittableRandom(state).ints().iterator().asScala.take(N).toList: @nowarn(
            "msg=deprecated")
        ints should_== expectedInts
      }
    }

    "generate longs" in {
      prop { state: State =>
        val longs = List.fill(N)(splitMix.nextLong).sequence.runA(state)
        val expectedLongs =
          splittableRandom(state).longs().iterator().asScala.take(N).toList: @nowarn(
            "msg=deprecated")
        longs should_== expectedLongs
      }
    }

    "split" in {
      prop { state: State =>
        val longs =
          List.fill(N)(splitMix.split.map(splitMix.nextLong.runA)).sequence.runA(state)
        val random = splittableRandom(state)
        val expectedLongs = List.fill(N)(random.split().nextLong())
        longs should_== expectedLongs
      }
    }
  }

}
