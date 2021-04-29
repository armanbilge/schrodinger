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
import org.scalacheck.{Arbitrary, Prop}

trait CommutativeGroup0Tests[A] extends Group0Tests[A] with CommutativeBinoidTests[A] {
  def laws: CommutativeGroup0Laws[A]

  def commutativeGroup0(implicit arbA: Arbitrary[A], eqA: Eq[A]): RuleSet =
    new RuleSet {
      val name: String = "commutativeGroup0"
      val bases: Seq[(String, RuleSet)] = Nil
      val parents: Seq[RuleSet] = Seq(group0, commutativeBinoid)
      val props: Seq[(String, Prop)] = Nil
    }

}

object CommutativeGroup0Tests {
  def apply[A: CommutativeGroup0]: CommutativeGroup0Tests[A] =
    new CommutativeGroup0Tests[A] {
      def laws: CommutativeGroup0Laws[A] = CommutativeGroup0Laws[A]
    }
}
