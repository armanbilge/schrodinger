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

import cats.effect.IO
import cats.effect.unsafe.implicits.*
import cats.syntax.all.*
import munit.CatsEffectSuite
import schrodinger.kernel.Random
import schrodinger.unsafe.rng.SplitMix

import scala.concurrent.duration.*

class RVIOSuite extends CatsEffectSuite:

  given RV: RVIO.Algebra[SplitMix] = RVIO.algebra[SplitMix].unsafeRunSync()
  val randomSum = (RV.sleep(0.millis) *> RV.int).parReplicateA(100).map(_.sum)

  test("run deterministically") {
    for
      rng <- SplitMix.fromTime[IO]
      program = randomSum.simulate(rng)
      (left, right) <- program.both(program)
    yield left === right
  }
