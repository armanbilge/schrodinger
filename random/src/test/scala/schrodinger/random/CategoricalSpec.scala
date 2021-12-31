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
import cats.instances.vector.*
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import schrodinger.kernel.Categorical
import schrodinger.kernel.testkit.SplitMix64
import schrodinger.kernel.testkit.PureRV
import schrodinger.math.LogDouble
import schrodinger.random.all.given

import scala.language.implicitConversions

class CategoricalSpec extends Specification, ScalaCheck:

  val N = 1000

  "Categorical" should {
    "generate valid samples" in prop { (rng: SplitMix64) =>
      val sample = Categorical[PureRV[SplitMix64, _], Vector[LogDouble], Int](
        Vector(LogDouble.Zero, LogDouble.One, LogDouble.Two))
      sample.replicateA(N).simulate(rng).value.toSet must contain(beAnyOf(1, 2))
    }
  }
