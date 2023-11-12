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

package schrodinger.laws

import cats.kernel.Eq
import cats.kernel.laws.discipline.*
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll
import org.typelevel.discipline.Laws
import schrodinger.math.Monus

trait MonusTests[A](laws: MonusLaws[A]) extends Laws {

  def monus(using Arbitrary[A], Arbitrary[A => A], Eq[A], Eq[Option[A]]): RuleSet = {
    val props = Seq(
      "axiom 1" -> forAll(laws.monusAxiom1),
      "axiom 2" -> forAll(laws.monusAxiom2),
      "axiom 3" -> forAll(laws.monusAxiom3),
      "axiom 4" -> forAll(laws.monusAxiom4),
      "natural order consistency" -> forAll(laws.monusNaturalOrderConsistency),
    ) ++ PartialOrderTests[A](using laws.A.naturalOrder).partialOrder.all.properties

    DefaultRuleSet("monus", None, props*)
  }
}

object MonusTests {
  def apply[A: Monus]: MonusTests[A] = new MonusTests(MonusLaws[A]) {}
}
