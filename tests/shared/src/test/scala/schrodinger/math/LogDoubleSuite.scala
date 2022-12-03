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
import algebra.ring.CommutativeSemifield
import cats.kernel.Eq
import cats.kernel.laws.discipline.HashTests
import cats.kernel.laws.discipline.OrderTests
import cats.kernel.laws.discipline.SerializableTests
import munit.DisciplineSuite
import org.scalacheck.Arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen
import org.scalacheck.Prop.*
import schrodinger.laws.LogarithmicTests
import schrodinger.laws.MonusTests

class LogDoubleSuite extends DisciplineSuite:

  given Arbitrary[LogDouble] = Arbitrary(Gen.double.map(LogDouble.exp(_)))
  given Cogen[LogDouble] = Cogen.cogenDouble.contramap(_.log)

  def approxEq(eps: Double): Eq[LogDouble] =
    case (LogDouble.Zero, LogDouble.Zero) => true
    case (LogDouble.One, LogDouble.One) => true
    case (x, y) if x.isNaN & y.isNaN => true
    case (x, y) => ((x.log - y.log) / (x.log.max(y.log))).abs < eps

  {
    given eq: Eq[LogDouble] = approxEq(1e-8)
    checkAll("LogDouble", RingLaws[LogDouble].commutativeSemifield)
    checkAll("LogDouble", MonusTests[LogDouble].monus)

    property("correctly add identical values") {
      forAll((x: LogDouble) => (x + x) == LogDouble.Two * x)
    }

    property("have alley-lawful subtraction") {
      forAll { (x: LogDouble, y: LogDouble) =>
        val z = x - y
        if x > y then eq.eqv(z + y, x)
        else z.isNaN
      }
    }

    property("have consistent IArray sum") {
      forAll { (xs: List[LogDouble]) =>
        eq.eqv(LogDouble.sum(IArray.from(xs)), CommutativeSemifield[LogDouble].sum(xs))
      }
    }

    property("have consistent Iterable sum") {
      forAll { (xs: List[LogDouble]) =>
        eq.eqv(LogDouble.sum(xs), CommutativeSemifield[LogDouble].sum(xs))
      }

    }
  }

  checkAll("LogDouble", OrderTests[LogDouble].order)
  checkAll("LogDouble", HashTests[LogDouble].hash)
  checkAll("LogDoubleAlgebra", SerializableTests.serializable(CommutativeSemifield[LogDouble]))

  checkAll("LogDouble", LogarithmicTests[Double, LogDouble].logarithmic)
