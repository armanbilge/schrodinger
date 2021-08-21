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

package schrodinger.stats

import schrodinger.kernel.Uniform
import schrodinger.kernel.Density
import cats.syntax.all.given
import cats.Applicative

object uniform extends UniformInstances

trait UniformInstances:

  given schrodingerStatsStandardUniformForInt[F[_]: Applicative]
      : Uniform[Density[F, LogDouble], Unit, Int] with
    private val density =
      LogDouble(Int.MaxValue.toDouble - Int.MinValue.toDouble + 1).reciprocal.pure[F]
    override def apply(support: Unit) = _ => density

  given schrodingerStatsStandardUniformForLong[F[_]: Applicative]
      : Uniform[Density[F, LogDouble], Unit, Int] with
    private val density =
      LogDouble(Long.MaxValue.toDouble - Long.MinValue.toDouble + 1).reciprocal.pure[F]
    override def apply(support: Unit) = _ => density

  given schrodingerStatsStandardUniformForDouble[F[_]: Applicative]
      : Uniform[Density[F, LogDouble], Unit, Double] with
    private val zero = LogDouble.Zero.pure[F]
    private val one = LogDouble.One.pure[F]
    override def apply(support: Unit) = x => if 0 <= x && x <= 1 then one else zero

  given schrodingerStatsUniformForIntRange[F[_]: Applicative]
      : Uniform[Density[F, LogDouble], Range, Int] with
    override def apply(range: Range) =
      val density =
        LogDouble((range.last.toDouble - range.start.toDouble + 1).abs).reciprocal.pure[F]
      _ => density
