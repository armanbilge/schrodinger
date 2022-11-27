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

import cats.effect.SyncIO
import cats.syntax.all.*
import munit.ScalaCheckSuite
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop.forAllNoShrink
import scodec.bits.*

import scala.collection.mutable

class ThreefishSuite extends ScalaCheckSuite:

  given Arbitrary[Threefish] = Arbitrary(
    for
      s0 <- arbitrary[Long]
      s1 <- arbitrary[Long]
      s2 <- arbitrary[Long]
      s3 <- arbitrary[Long]
    yield Threefish(s0, s1, s2, s3),
  )

  enum RngProg:
    case Next(andThen: RngProg)
    case Split(left: RngProg, right: RngProg)
    case Done

  object RngProg:

    given Arbitrary[RngProg] = Arbitrary {
      def gen(depth: Int): Gen[RngProg] = Gen.frequency(
        1 -> Gen.const(Done),
        (if depth > 0 then 9 else 0) -> (for
          left <- Gen.delay(gen(depth - 1))
          right <- Gen.delay(gen(depth - 1))
        yield Split(left, right)),
        90 -> Gen.delay(gen(depth - 1)).map(Next(_)),
      )
      gen(128 * 4)
    }

    def run(
        onNext: (Threefish, (Long, Long, Long, Long)) => SyncIO[Unit],
        onSplit: (Threefish, Threefish) => SyncIO[Unit],
    )(tf: Threefish, prog: RngProg): SyncIO[Unit] =
      def go(tf: Threefish, prog: RngProg): SyncIO[Unit] = prog match
        case Next(andThen) =>
          val nextLong = SyncIO(tf.nextLong())
          (nextLong, nextLong, nextLong, nextLong).flatMapN { (l0, l1, l2, l3) =>
            onNext(tf, (l0, l1, l2, l3)) >> go(tf, andThen)
          }
        case Split(left, right) =>
          SyncIO(tf.split()).flatTap(onSplit(tf, _)).flatMap(go(_, right)) >> go(tf, left)
        case Done => SyncIO.unit

      go(tf, prog)

  property("prefix free") {
    forAllNoShrink { (tf: Threefish, prog: RngProg) =>
      SyncIO(mutable.Set.empty[(Long, Long, Long, Long)])
        .flatMap { states =>
          SyncIO(states.add(tf.state())) >>
            RngProg.run(
              (tf, next) =>
                SyncIO {
                  states.add(tf.state())
                  assert(states(next))
                },
              (left, right) =>
                SyncIO {
                  states.add(left.state())
                  states.add(right.state())
                },
            )(tf, prog)
        }
        .unsafeRunSync()
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
