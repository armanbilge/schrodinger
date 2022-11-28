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

package schrodinger.unsafe

import cats.effect.IO
import cats.syntax.all.*
import munit.CatsEffectSuite
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.effect.PropF.forAllF
import scodec.bits.*

class ThreefishSuite extends CatsEffectSuite:

  given Arbitrary[Threefish] = Arbitrary(
    for
      s0 <- arbitrary[Long]
      s1 <- arbitrary[Long]
      s2 <- arbitrary[Long]
      s3 <- arbitrary[Long]
    yield Threefish(s0, s1, s2, s3),
  )

  enum RngOp:
    case NextInt, NextLong, SplitRight, SplitLeft
  object RngOp:
    given Arbitrary[RngOp] = Arbitrary(Gen.oneOf(NextInt, NextLong, SplitRight, SplitLeft))

  test("generate") {
    forAllF { (rng: Threefish, ops: List[RngOp]) =>
      import RngOp.*

      def go(rng: Threefish, ops: List[RngOp]): IO[Unit] = ops match
        case Nil => IO.unit
        case NextInt :: tail => IO(rng.nextInt()) >> go(rng, ops)
        case NextLong :: tail => IO(rng.nextLong()) >> go(rng, ops)
        case SplitRight :: tail => IO(rng.split()) >> go(rng, ops)
        case SplitLeft :: tail => IO(rng.split()).flatMap(go(_, ops))

      go(rng, ops)
    }
  }

  test("skein_golden_kat_internals") {
    val out = new Array[Long](4)
    Threefish.processBlock(new Array(4), new Array(4), out)
    assertEquals(
      out.map(ByteVector.fromLong(_)).reduce(_ ++ _),
      hex"94EEEA8B1F2ADA84ADF103313EAE6670952419A1F4B16D53D83F13E63C9F6B11",
    )
  }
