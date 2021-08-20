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

package schrodinger.kernel.laws

import cats.kernel.Eq
import cats.laws.discipline.*
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll
import org.typelevel.discipline.Laws
import schrodinger.kernel.PseudoRandom

trait PseudoRandomTests[F[_], G[_], S] extends Laws:
  val laws: PseudoRandomLaws[F, G, S]

  def pseudoRandom[A](using Arbitrary[F[A]], Arbitrary[S], Eq[G[A]]): RuleSet =
    new RuleSet {
      val name = "pseudoRandom"
      val bases = Nil
      val parents = Nil
      val props = Seq("reproducible" -> forAll((fa: F[A], s: S) => laws.reproducible(fa, s)))
    }

object PseudoRandomTests:
  def apply[F[_], G[_], S](using PseudoRandom.Aux[F, G, S]): PseudoRandomTests[F, G, S] =
    new PseudoRandomTests[F, G, S] { val laws = PseudoRandomLaws[F, G, S] }
