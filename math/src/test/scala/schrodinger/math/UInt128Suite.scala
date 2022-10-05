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

import munit.DisciplineSuite
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop.*

class UInt128Suite extends DisciplineSuite:

  given Arbitrary[UInt128] = Arbitrary(
    for
      lo <- Arbitrary.arbLong.arbitrary
      hi <- Arbitrary.arbLong.arbitrary
    yield UInt128(hi, lo)
  )

  final case class Shift(shift: Int)
  given Arbitrary[Shift] = Arbitrary(Gen.chooseNum(0, 127).map(Shift(_)))

  property("roundtrip with BigInt") {
    forAll { (x: UInt128) => UInt128(x.toBigInt) == x }
  }

  property("add") {
    forAll { (x: UInt128, y: UInt128) =>
      (x + y).toBigInt == (x.toBigInt + y.toBigInt) % BigInt(2).pow(128)
    }
  }

  property("multiply") {
    forAll { (x: UInt128, y: UInt128) =>
      (x * y).toBigInt == (x.toBigInt * y.toBigInt) % BigInt(2).pow(128)
    }
  }

  property("or") {
    forAll { (x: UInt128, y: UInt128) => (x | y).toBigInt == (x.toBigInt | y.toBigInt) }
  }

  property("and") {
    forAll { (x: UInt128, y: UInt128) => (x & y).toBigInt == (x.toBigInt & y.toBigInt) }
  }

  property("xor") {
    forAll { (x: UInt128, y: UInt128) => (x ^ y).toBigInt == (x.toBigInt ^ y.toBigInt) }
  }

  property("shift left") {
    forAll { (x: UInt128, y: Shift) =>
      (x << y.shift).toBigInt == (x.toBigInt << y.shift) % BigInt(2).pow(128)
    }
  }

  property("shift right") {
    forAll { (x: UInt128, y: Shift) => (x >> y.shift).toBigInt == x.toBigInt >> y.shift }
  }
