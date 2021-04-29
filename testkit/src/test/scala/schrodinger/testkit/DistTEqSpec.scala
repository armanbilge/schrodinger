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

import cats.Eq
import cats.laws.discipline.ExhaustiveCheck
import cats.syntax.traverse._
import org.scalacheck.{Arbitrary, Gen}
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import schrodinger.Dist
import schrodinger.distributions.Bernoulli
import schrodinger.generators.SplitMix
import schrodinger.generators.SplitMix._
import schrodinger.testkit.DistTEqSpec.DifferentDists
import schrodinger.testkit.dist._

object DistTEqSpec {
  final case class DifferentDists(dist1: Dist[Boolean], dist2: Dist[Boolean])
}

class DistTEqSpec extends Specification with ScalaCheck {

  implicit val arbitrary = Arbitrary(
    for {
      p <- Gen.choose(0, 0.25)
    } yield DifferentDists(Bernoulli(p), Bernoulli(1 - p))
  )

  implicit val seeds = ExhaustiveCheck.instance(
    List
      .fill(1)(SplitMix.DefaultInstance.split)
      .sequence
      .runA(SplitMix.initialState(System.currentTimeMillis(), System.nanoTime()))
  )

  implicit val confidence = Confidence(1000, 0.95)

  "DistTEq" should {
    "recognize equivalent distributions" in {
      prop { (dist: Dist[Boolean]) => Eq[Dist[Boolean]].eqv(dist, dist) }
    }

    "recognize non-equivalent distributions" in {
      prop { different: DifferentDists =>
        import different._
        Eq[Dist[Boolean]].neqv(dist1, dist2)
      }
    }
  }

}
