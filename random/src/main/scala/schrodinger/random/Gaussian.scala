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

package schrodinger.random

import cats.{Apply, Monad}
import cats.syntax.all._
import schrodinger.kernel.Random

object Gaussian {
  def standardPair[F[_]: Monad: Random]: F[(Double, Double)] =
    boxMuller.iterateWhile(bm => bm.s >= 1.0 | bm.s == 0.0).map { bm =>
      import bm._
      val scale = math.sqrt(-2.0 * math.log(s) / s)
      (scale * x, scale * y)
    }

  private def boxMuller[F[_]: Apply: Random]: F[BoxMuller] =
    Uniform.double.map2(Uniform.double) { (x, y) => BoxMuller(x * 2 - 1, y * 2 - 1) }

  final private case class BoxMuller(x: Double, y: Double, s: Double)
  private object BoxMuller {
    def apply(x: Double, y: Double): BoxMuller =
      BoxMuller(x, y, x * x + y * y)
  }
}
