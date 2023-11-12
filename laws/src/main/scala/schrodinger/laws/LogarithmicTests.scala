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
import org.scalacheck.Prop
import org.scalacheck.Prop.forAll
import org.typelevel.discipline.Laws
import schrodinger.math.Logarithmic

trait LogarithmicTests[A, L](laws: LogarithmicLaws[A, L]) extends Laws {

  def logarithmic(using Arbitrary[A], Arbitrary[L], Eq[A], Eq[L]): RuleSet = {
    val props = Seq[(String, Prop)](
      "logarithm round trip" -> forAll(laws.logarithmRoundTrip(_)),
      "linear round trip" -> forAll(laws.linearRoundTrip(_)),
      "one is zero" -> laws.oneIsZero,
      "times is plus" -> forAll(laws.timesIsPlus(_, _)),
      "div is minus" -> forAll(laws.divIsMinus(_, _)),
    )

    DefaultRuleSet("logarithmic", None, props*)
  }
}

object LogarithmicTests {

  def apply[A, L](using Logarithmic[A, L]): LogarithmicTests[A, L] =
    new LogarithmicTests(LogarithmicLaws[A, L]) {}
}
