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
import cats.kernel.laws.discipline.CommutativeMonoidTests
import org.scalacheck.{Arbitrary, Prop}

trait CommutativeBinoidTests[A] extends CommutativeMonoidTests[A] with BinoidTests[A] {
  def laws: CommutativeBinoidLaws[A]

  def commutativeBinoid(implicit arbA: Arbitrary[A], eqA: Eq[A]): RuleSet =
    new RuleSet {
      val name: String = "commutativeBinoid"
      val bases: Seq[(String, RuleSet)] = Nil
      val parents: Seq[RuleSet] = Seq(commutativeMonoid, binoid)
      val props: Seq[(String, Prop)] = Nil
    }

}

object CommutativeBinoidTests {
  def apply[A: CommutativeBinoid]: CommutativeBinoidTests[A] =
    new CommutativeBinoidTests[A] {
      def laws: CommutativeBinoidLaws[A] = CommutativeBinoidLaws[A]
    }
}
