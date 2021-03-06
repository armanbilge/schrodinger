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

package schrodinger.kernel

import cats.syntax.all.*
import cats.instances.vector.*
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import schrodinger.kernel.Categorical
import schrodinger.kernel.testkit.SplitMix64
import schrodinger.kernel.testkit.PureRV
import schrodinger.math.LogDouble
import algebra.instances.all.*

import scala.language.implicitConversions
import cats.data.NonEmptyVector

class CategoricalSpec extends Specification, ScalaCheck:

  val N = 1000

  "Categorical" should {
    "generate valid samples" in prop { (rng: SplitMix64) =>
      val sample = Categorical[PureRV[SplitMix64, _], NonEmptyVector[Double], Long](
        NonEmptyVector.of(0.0, 1.0, 2.0))
      val samples = sample.replicateA(N).simulate(rng).value
      samples.toSet must be_==(Set(1, 2)) and
        (samples.count(_ == 2L) must be_>(samples.count(_ == 1L)))
    }
  }
