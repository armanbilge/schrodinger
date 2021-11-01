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

package schrodinger.kernel.testkit

import cats.syntax.all.*
import cats.Traverse
import org.apache.commons.rng.core.source64
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import schrodinger.kernel.Random

class SplitMix64Spec extends Specification, ScalaCheck:
  val N = 100

  "SplitMix64" should {
    "match Apache implementation" in prop { (seed: Long) =>
      val apache = new source64.SplitMix64(seed)
      Random[PureRV[SplitMix64, _]].long.replicateA(N).simulate(SplitMix64(seed)).value ===
        List.fill(N)(apache.nextLong())
    }
  }
