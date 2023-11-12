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

package schrodinger.kernel.testkit

import cats.kernel.laws.discipline.EqTests
import cats.laws.discipline.ExhaustiveCheck
import cats.laws.discipline.MonadTests
import cats.syntax.all.*
import munit.DisciplineSuite
import org.scalacheck.Arbitrary

class PureRVTSuite extends DisciplineSuite {
  given Confidence = Confidence(1000, 0.9, 0.9)
  given ExhaustiveCheck[SplitMix64] = ExhaustiveCheck.instance(List(SplitMix64(1234567890L)))
  // TODO PureRV needs a cogen
  given Arbitrary[(PureRV[SplitMix64, Boolean] => PureRV[SplitMix64, Boolean])] =
    Arbitrary(
      Arbitrary.arbitrary[Boolean => Boolean].map(f => (_: PureRV[SplitMix64, Boolean]).map(f)),
    )

  override def scalaCheckInitialSeed = "InZXKRfzpRf8Ujb786p43rDN_G7LkKV64730MJvidUO="

  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(
      if System.getProperty("java.vm.name") == "Scala Native" then 10 else 100,
    )

  checkAll("PureRV", EqTests[PureRV[SplitMix64, Boolean]].eqv.random)
  checkAll("PureRV", MonadTests[PureRV[SplitMix64, _]].monad[Boolean, Boolean, Boolean].random)
}
