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

package schrodinger.generators
import cats.{Id, Monad}
import cats.data.StateT
import cats.syntax.monad._

import java.lang

/**
 * Implements a binary expansion to simulate uniform random doubles.
 * It is considerably more complicated than the default implementation,
 * but is able to take full advantage of the precision of double representation.
 * Specifically it can generate doubles in the range (0, 2^-53^)
 * (with the appropriate probability) which the default implementation cannot.
 *
 * @see http://mumble.net/~campbell/tmp/random_real.c
 */
trait BinaryExpansionDouble[S] extends Generator[S] {

  override def nextDouble: StateT[Id, S, Double] =
    nextBinaryExpansionDouble.iterateWhile(_ == 1.0)

  def nextBinaryExpansionDouble: StateT[Id, S, Double] =
    // Adapted from http://mumble.net/~campbell/tmp/random_real.c with notice:
    // Copyright (c) 2014, Taylor R Campbell
    // Verbatim copying and distribution of this entire article are
    // permitted worldwide, without royalty, in any medium, provided
    // this notice, and the copyright notice, are preserved.
    Monad[StateT[Id, S, *]]
      .iterateWhileM((0L, -64)) {
        case (_, exponent) =>
          nextLong.map { significand =>
            (significand, exponent - (if (significand == 0) 64 else 0))
          }
      } {
        case (significand, exponent) =>
          significand == 0 & exponent >= -1074
      }
      .flatMap {
        case (significand, exponent) =>
          if (exponent < -1074)
            StateT.pure(0.0)
          else {
            val shift = lang.Long.numberOfLeadingZeros(significand)
            for {
              (significand, exponent) <-
                if (shift != 0) {
                  nextLong.map { random64 =>
                    val exp = exponent - shift
                    val sig = (significand << shift) | (random64 >>> (64 - shift))
                    (sig, exp)
                  }
                } else StateT.pure[Id, S, (Long, Int)]((significand, exponent))
            } yield math.scalb((significand | 1).toDouble, exponent)
          }
      }

}
