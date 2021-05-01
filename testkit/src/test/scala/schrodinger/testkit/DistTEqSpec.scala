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

package schrodinger.testkit

import cats.{Eq, Id}
import cats.laws.discipline.ExhaustiveCheck
import cats.syntax.traverse._
import org.scalacheck.{Arbitrary, Gen}
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import schrodinger.Random
import schrodinger.random.Bernoulli
import schrodinger.generators.{Split, SplitMix}
import schrodinger.generators.SplitMix._
import schrodinger.testkit.RandomTEqSpec.DifferentRandoms
import schrodinger.testkit.random._

object RandomTEqSpec {
  final case class DifferentRandoms[S](dist1: Random[S, Boolean], dist2: Random[S, Boolean])
}

class RandomTEqSpec extends Specification with ScalaCheck {

  implicit val arbitrary = Arbitrary(
    for {
      p <- Gen.choose(0, 0.25)
    } yield DifferentRandoms(Bernoulli[Id, SplitMix](p), Bernoulli[Id, SplitMix](1 - p))
  )

  implicit val seeds = ExhaustiveCheck.instance(
    List
      .fill(1)(Split[Id, SplitMix].split)
      .sequence
      .simulate(SplitMix.initialState(System.currentTimeMillis(), System.nanoTime()))
  )

  implicit val confidence = Confidence(1000, 0.95)

  "RandomTEq" should {
    "recognize equivalent distributions" in {
      prop { (dist: Random[SplitMix, Boolean]) =>
        Eq[Random[SplitMix, Boolean]].eqv(dist, dist)
      }
    }

    "recognize non-equivalent distributions" in {
      prop { different: DifferentRandoms[SplitMix] =>
        import different._
        Eq[Random[SplitMix, Boolean]].neqv(dist1, dist2)
      }
    }
  }

}
