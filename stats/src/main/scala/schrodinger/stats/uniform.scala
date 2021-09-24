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
package stats

import cats.Applicative
import cats.syntax.all.given
import schrodinger.kernel.Density
import schrodinger.kernel.Uniform
import schrodinger.math.LogDouble
import schrodinger.math.Interval.*
import schrodinger.math.Interval.given

object uniform extends UniformInstances

trait UniformInstances:

  given schrodingerStatsStandardUniformForInt[F[_]: Applicative]
      : Uniform[Int.MinValue.type <=@<= Int.MaxValue.type, Int][Density[F, LogDouble]] =
    val density =
      (_: Int) =>
        LogDouble(Int.MaxValue.toDouble - Int.MinValue.toDouble + 1).reciprocal.pure[F]
    _ => density

  given schrodingerStatsStandardUniformForLong[F[_]: Applicative]
      : Uniform[Long.MinValue.type <=@<= Long.MaxValue.type, Long][Density[F, LogDouble]] =
    val density =
      (_: Long) =>
        LogDouble(Long.MaxValue.toDouble - Long.MinValue.toDouble + 1).reciprocal.pure[F]
    _ => density

  given schrodingerStatsStandardUniformForDouble[F[_]: Applicative]
      : Uniform[0.0 <=@< 1.0, Double][Density[F, LogDouble]] =
    val zero = LogDouble.Zero.pure[F]
    val one = LogDouble.One.pure[F]
    val density = (x: Double) => if 0 <= x && x <= 1 then one else zero
    _ => density

  given schrodingerStatsUniformForIntRange[F[_]: Applicative]
      : Uniform[Range, Int][Density[F, LogDouble]] with
    override def apply(params: Uniform.Params[Range]) =
      val range = params.support
      val zero = LogDouble.Zero.pure[F]
      val density =
        LogDouble((range.last.toDouble - range.start.toDouble + 1).abs).reciprocal.pure[F]
      x => if range.contains(x) then density else zero
