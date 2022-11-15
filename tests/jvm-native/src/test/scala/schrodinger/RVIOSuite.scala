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
import cats.effect.syntax.all.*
import cats.effect.unsafe.implicits.*
import cats.syntax.all.*
import munit.CatsEffectSuite
import munit.ScalaCheckEffectSuite
import org.scalacheck.effect.PropF.*
import schrodinger.kernel.Gaussian
import schrodinger.kernel.Random
import schrodinger.testkit.RVTestkit
import schrodinger.unsafe.rng.SplitMix

import scala.concurrent.duration.*

class RVIOSuite extends CatsEffectSuite, ScalaCheckEffectSuite, RVTestkit:

  test("run deterministically") {
    for
      given RVIO.Algebra[SplitMix] <- RVIO.algebra[SplitMix]
      randomSum = Random.int.delayBy(0.millis).parReplicateA(100).map(_.sum)
      rng <- SplitMix.fromTime[IO]
      program = randomSum.simulate(rng)
      (left, right) <- program.both(program)
    yield left === right
  }

  test("streams are not identical") {
    forAllF { (seed: SplitMix) =>
      RVIO.algebra[SplitMix].flatMap { case given RVIO.Algebra[SplitMix] =>
        val nextLong = Random.long[RVIO[SplitMix, *]]
        val prog = nextLong *> nextLong.both(nextLong).evalMap { (left, right) =>
          IO(assert(clue(left) != clue(right)))
        }
        prog.simulate(seed)
      }
    }
  }

  test("streams of gaussians are not identical") {
    forAllF { (seed: SplitMix) =>
      RVIO.algebra[SplitMix].flatMap { case given RVIO.Algebra[SplitMix] =>
        val nextGaussian = Gaussian.standard[RVIO[SplitMix, *], Double]
        val prog = nextGaussian *> nextGaussian.both(nextGaussian).evalMap { (left, right) =>
          IO(assert(clue(left) != clue(right)))
        }
        prog.simulate(seed)
      }
    }
  }
