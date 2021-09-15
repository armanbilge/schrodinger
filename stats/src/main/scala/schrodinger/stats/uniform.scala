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

object uniform extends UniformInstances

trait UniformInstances:

  given schrodingerStatsStandardUniformForInt[F[_]: Applicative]
      : Uniform[Unit, Int][Density[F, LogDouble]] with
    private val density =
      LogDouble(Int.MaxValue.toDouble - Int.MinValue.toDouble + 1).reciprocal.pure[F]
    override def apply(params: Uniform.Params[Unit]) = _ => density

  given schrodingerStatsStandardUniformForLong[F[_]: Applicative]
      : Uniform[Unit, Int][Density[F, LogDouble]] with
    private val density =
      LogDouble(Long.MaxValue.toDouble - Long.MinValue.toDouble + 1).reciprocal.pure[F]
    override def apply(params: Uniform.Params[Unit]) = _ => density

  given schrodingerStatsStandardUniformForDouble[F[_]: Applicative]
      : Uniform[Unit, Double][Density[F, LogDouble]] with
    private val zero = LogDouble.Zero.pure[F]
    private val one = LogDouble.One.pure[F]
    override def apply(params: Uniform.Params[Unit]) = x =>
      if 0 <= x && x <= 1 then one else zero

  given schrodingerStatsUniformForIntRange[F[_]: Applicative]
      : Uniform[Range, Int][Density[F, LogDouble]] with
    override def apply(params: Uniform.Params[Range]) =
      val range = params.support
      val density =
        LogDouble((range.last.toDouble - range.start.toDouble + 1).abs).reciprocal.pure[F]
      _ => density
