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
import munit.ScalaCheckEffectSuite
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.effect.PropF.forAllF
import scodec.bits.*

class ThreefishSuite extends CatsEffectSuite, ScalaCheckEffectSuite:

  override protected def scalaCheckTestParameters =
    super.scalaCheckTestParameters
      .withMinSize(128 * 64)
      .withMinSize(128 * 1024)

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

  def run(rng: Threefish, ops: List[RngOp]): IO[Threefish] =
    import RngOp.*

    def go(rng: Threefish, ops: List[RngOp]): IO[Threefish] = ops match
      case Nil => IO.pure(rng)
      case NextInt :: tail => IO(rng.nextInt()) >> go(rng, tail)
      case NextLong :: tail => IO(rng.nextLong()) >> go(rng, tail)
      case SplitRight :: tail => IO(rng.split()) >> go(rng, tail)
      case SplitLeft :: tail => IO(rng.split()).flatMap(go(_, tail))

    go(rng, ops)

  test("generate") {
    forAllF { (rng: Threefish, ops: List[RngOp]) =>
      run(rng, ops).void
    }
  }

  test("split") {
    import RngOp.*

    forAllF(arbitrary[Threefish], Gen.listOfN(128, Gen.oneOf(SplitRight, SplitLeft))) {
      (rng: Threefish, ops: List[RngOp]) =>
        run(rng, ops).flatMap { rng =>
          (IO(rng.state()), IO(rng.split()).flatMap(l => IO(l.state())), IO(rng.state()))
            .flatMapN { (parent, left, right) =>
              IO(assert(clue(Set(parent, left, right)).size == 3))
            }
        }
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
