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
import org.specs2.cats.effect.CatsEffect
import org.specs2.mutable.Specification
import schrodinger.kernel.Random
import schrodinger.unsafe.rng.SplitMix

class RVIOSpec extends Specification with CatsEffect:

  given RV: RVIO.Algebra[SplitMix] = RVIO.algebra[SplitMix].unsafeRunSync()
  val randomSum = List.fill(10000)(()).parFoldMapA(_ => RV.int)

  "RVIO" should {
    "run deterministically" in {
      for
        rng <- SplitMix.fromTime[IO]
        program = randomSum.simulate(rng)
        (left, right) <- program.both(program)
      yield left === right
    }
  }
