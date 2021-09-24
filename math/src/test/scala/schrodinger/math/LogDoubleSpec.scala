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

package schrodinger.math

import algebra.laws.RingLaws
import cats.kernel.Eq
import cats.kernel.laws.discipline.HashTests
import cats.kernel.laws.discipline.OrderTests
import cats.kernel.laws.discipline.SerializableTests
import org.scalacheck.Arbitrary
import org.scalacheck.Cogen
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import org.typelevel.discipline.specs2.mutable.Discipline

class LogDoubleSpec extends Specification, Discipline, ScalaCheck {

  given Arbitrary[LogDouble] = Arbitrary(
    Arbitrary.arbDouble.arbitrary.map(x => LogDouble(math.abs(x))))
  given Cogen[LogDouble] = Cogen.cogenDouble.contramap(_.log)

  def approxEq(eps: Double): Eq[LogDouble] = {
    case (LogDouble.NaN, LogDouble.NaN) => true
    case (LogDouble.Zero, LogDouble.Zero) => true
    case (LogDouble.One, LogDouble.One) => true
    case (x, y) => ((x.log - y.log) / (x.log max y.log)).abs < eps
  }

  {
    given Eq[LogDouble] = approxEq(1e-8)
    checkAll("CommutativeRig[LogDouble]", RingLaws[LogDouble].commutativeRig)
    checkAll(
      "MultiplicativeCommutativeGroup[LogDouble]",
      RingLaws[LogDouble].multiplicativeCommutativeGroup)
  }

  checkAll("Order[LogDouble]", OrderTests[LogDouble].order)
  checkAll("Hash[LogDouble]", HashTests[LogDouble].hash)
  checkAll(
    "Serializable[LogDoubleAlgebra]",
    SerializableTests.serializable(
      LogDouble.given_CommutativeRig_LogDouble_MultiplicativeCommutativeGroup_LogDouble_Order_LogDouble_Hash_LogDouble)
  )

}
