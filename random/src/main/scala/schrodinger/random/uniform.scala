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

import cats.{Functor, Monad}
import cats.syntax.all.*
import schrodinger.kernel.{Random, Uniform}
import schrodinger.math.Interval.*

import java.lang
import scala.annotation.switch
import scala.collection.immutable.NumericRange

object uniform extends UniformInstances

trait UniformInstances:

  given schrodingerRandomUniformInt[F[_]: Random]
      : Uniform[Int.MinValue.type <=@<= Int.MaxValue.type, Int][F] = _ => Random[F].int

  given schrodingerRandomUniformLong[F[_]: Random]
      : Uniform[Long.MinValue.type <=@<= Long.MaxValue.type, Long][F] = _ => Random[F].long

  given `schrodingerRandomUniformFloat[0,1)`[F[_]: Functor: Random]
      : Uniform[0f <=@< 1f, Float][F] =
    val x = Random[F].int.map(x => (x >>> 8) * 5.9604645e-8f)
    _ => x

  given `schrodingerRandomUniformFloat(0,1]`[F[_]: Functor: Random]
      : Uniform[0f <@<= 1f, Float][F] =
    val x = Random[F].int.map(x => ((x >>> 8) + 1) * 5.9604645e-8f)
    _ => x

  given `schrodingerRandomUniformDouble[0,1)`[F[_]: Functor: Random]
      : Uniform[0d <=@< 1d, Double][F] =
    val x = Random[F].long.map(x => (x >>> 11) * 1.1102230246251565e-16)
    _ => x

  given `schrodingerRandomUniformDouble(0,1]`[F[_]: Functor: Random]
      : Uniform[0d <@<= 1d, Double][F] =
    val x = Random[F].long.map(x => ((x >>> 11) + 1) * 1.1102230246251565e-16)
    _ => x

  given schrodingerRandomUniformForIntRange[F[_]: Monad: Random]: Uniform[Range, Int][F] with
    override def apply(params: Uniform.Params[Range]): F[Int] =
      val range = params.support
      require(range.nonEmpty)
      if range.start == 0 & range.step == 1 then
        if range.last == Int.MaxValue then Random[F].int.map(_ & Int.MaxValue)
        else nonNegativeInt(range.last + 1)
      else
        (range.step: @switch) match
          case 1 => boundedInt(range.start, range.last)
          case 2 => ??? // TODO
          case _ => apply(Uniform.Params(range.indices)).map(range)

    private def nonNegativeInt(n: Int): F[Int] =
      if (n & -n) == n then Random[F].int.map(_ & (n - 1))
      else
        Random[F]
          .int
          .map { x =>
            val b = x >>> 1
            val v = b % n
            (b, v)
          }
          .iterateWhile(bv => bv._1 - bv._2 + (n - 1) < 0)
          .map(_._2)

    private def boundedInt[F[_]: Monad: Random](from: Int, to: Int): F[Int] =
      import lang.Integer.*
      val width = to - from + 1
      if width == 0 then Random[F].int
      else
        val cap =
          if compareUnsigned(width, Int.MinValue) > 0 then width
          else retryCap(width)
        if cap == 0 then Random[F].int.map(x => from + remainderUnsigned(x, width))
        else
          Random[F]
            .int
            .iterateUntil(x => compareUnsigned(x, cap) <= 0)
            .map(x => remainderUnsigned(x, width) + from)

    private def retryCap(width: Int): Int =
      import lang.Integer.*
      val q = divideUnsigned(Int.MinValue, width)
      val r = remainderUnsigned(Int.MinValue, width)
      val n = (q << 1) + divideUnsigned(r << 1, width)
      n * width

  given schrodingerRandomUniformForLongRange[F[_]: Monad: Random]
      : Uniform[NumericRange[Long], Long][F] with
    override def apply(params: Uniform.Params[NumericRange[Long]]): F[Long] =
      val range = params.support
      require(range.nonEmpty)
      if range.start == 0 & range.step == 1 then
        if range.last == Long.MaxValue then Random[F].long.map(_ & Long.MaxValue)
        else nonNegativeLong(range.last + 1)
      else
        (range.step: @switch) match
          case 1 => boundedLong(range.start, range.last)
          case _ => ??? // TODO

    private def nonNegativeLong[F[_]: Monad: Random](n: Long): F[Long] =
      if (n & -n) == n then Random[F].long.map(_ & (n - 1))
      else
        Random[F]
          .long
          .map { x =>
            val b = x >>> 1
            val v = b % n
            (b, v)
          }
          .iterateWhile(bv => bv._1 - bv._2 + (n - 1) < 0)
          .map(_._2)

    private def boundedLong[F[_]: Monad: Random](from: Long, to: Long): F[Long] =
      import lang.Long.*
      val width = to - from + 1
      if width == 0 then Random[F].long
      else
        val cap =
          if compareUnsigned(width, Long.MinValue) > 0 then width
          else retryCap(width)
        if cap == 0 then Random[F].long.map(x => from + remainderUnsigned(x, width))
        else
          Random[F]
            .long
            .iterateUntil(x => compareUnsigned(x, cap) <= 0)
            .map(x => remainderUnsigned(x, width) + from)

    private def retryCap(width: Long): Long =
      import lang.Long.*
      val q = divideUnsigned(Long.MinValue, width)
      val r = remainderUnsigned(Long.MinValue, width)
      val n = (q << 1) + divideUnsigned(r << 1, width)
      n * width
