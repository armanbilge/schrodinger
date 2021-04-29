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

import cats.kernel.Eq
import cats.kernel.instances.boolean._
import cats.kernel.laws.discipline._
import cats.kernel.laws.discipline.MonoidTests
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll

trait BinoidTests[A] extends MonoidTests[A] {

  def laws: BinoidLaws[A]

  def binoid(implicit arbA: Arbitrary[A], eqA: Eq[A]): RuleSet =
    new DefaultRuleSet(
      "binoid",
      Some(monoid),
      "left absorb" -> forAll(laws.leftAbsorb _),
      "right absorb" -> forAll(laws.rightAbsorb _),
      "is absorbing" -> forAll((a: A) => laws.isId(a, eqA))
    )

}

object BinoidTests {
  def apply[A: Binoid]: BinoidTests[A] =
    new BinoidTests[A] { def laws: BinoidLaws[A] = BinoidLaws[A] }
}
