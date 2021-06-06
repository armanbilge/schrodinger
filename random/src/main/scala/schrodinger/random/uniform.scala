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
import cats.syntax.all._
import schrodinger.kernel.{Random, Uniform}

import java.lang
import scala.annotation.switch
import scala.collection.immutable.NumericRange

object uniform extends UniformInstances

trait UniformInstances {

  implicit def schrodingerRandomStandardUniformForFloat[F[_]: Functor: Random]
      : Uniform[F, Unit, Float] =
    new Uniform[F, Unit, Float] {
      private val standard = Random[F].int.map(x => (x >>> 8) * 5.9604645e-8f)
      override def apply(support: Unit): F[Float] = standard
    }

  implicit def schrodingerRandomStandardUniformForDouble[F[_]: Functor: Random]
      : Uniform[F, Unit, Double] =
    new Uniform[F, Unit, Double] {
      private val standard = Random[F].long.map(x => (x >>> 11) * 1.1102230246251565e-16)
      override def apply(support: Unit): F[Double] = standard
    }

  implicit def schrodingerRandomUniformForIntRange[F[_]: Monad: Random]
      : Uniform[F, Range, Int] =
    new Uniform[F, Range, Int] {
      override def apply(range: Range): F[Int] = {
        require(range.nonEmpty)
        if (range.start == 0 & range.step == 1)
          if (range.last == Int.MaxValue)
            Random[F].int.map(_ & Int.MaxValue)
          else
            nonNegativeInt(range.last + 1)
        else
          (range.step: @switch) match {
            case 1 => boundedInt(range.start, range.last)
            case 2 => ??? // TODO
            case _ => apply(range.indices).map(range)
          }
      }

      private def nonNegativeInt(n: Int): F[Int] =
        if ((n & -n) == n)
          Random[F].int.map(_ & (n - 1))
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

      private def boundedInt[F[_]: Monad: Random](from: Int, to: Int): F[Int] = {
        import lang.Integer._
        val width = to - from + 1
        if (width == 0)
          Random[F].int
        else {
          val cap =
            if (compareUnsigned(width, Int.MinValue) > 0)
              width
            else
              retryCap(width)
          if (cap == 0)
            Random[F].int.map(x => from + remainderUnsigned(x, width))
          else
            Random[F]
              .int
              .iterateUntil(x => compareUnsigned(x, cap) <= 0)
              .map(x => remainderUnsigned(x, width) + from)
        }
      }

      private def retryCap(width: Int): Int = {
        import lang.Integer._
        val q = divideUnsigned(Int.MinValue, width)
        val r = remainderUnsigned(Int.MinValue, width)
        val n = (q << 1) + divideUnsigned(r << 1, width)
        n * width
      }
    }

  implicit def schrodingerRandomUniformForLongRange[F[_]: Monad: Random]
      : Uniform[F, NumericRange[Long], Long] =
    new Uniform[F, NumericRange[Long], Long] {
      override def apply(range: NumericRange[Long]): F[Long] = {
        require(range.nonEmpty)
        if (range.start == 0 & range.step == 1)
          if (range.last == Long.MaxValue)
            Random[F].long.map(_ & Long.MaxValue)
          else
            nonNegativeLong(range.last + 1)
        else
          (range.step: @switch) match {
            case 1 => boundedLong(range.start, range.last)
            case _ => ??? // TODO
          }
      }

      private def nonNegativeLong[F[_]: Monad: Random](n: Long): F[Long] =
        if ((n & -n) == n)
          Random[F].long.map(_ & (n - 1))
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

      private def boundedLong[F[_]: Monad: Random](from: Long, to: Long): F[Long] = {
        import lang.Long._
        val width = to - from + 1
        if (width == 0)
          Random[F].long
        else {
          val cap =
            if (compareUnsigned(width, Long.MinValue) > 0)
              width
            else
              retryCap(width)
          if (cap == 0)
            Random[F].long.map(x => from + remainderUnsigned(x, width))
          else
            Random[F]
              .long
              .iterateUntil(x => compareUnsigned(x, cap) <= 0)
              .map(x => remainderUnsigned(x, width) + from)
        }
      }

      private def retryCap(width: Long): Long = {
        import lang.Long._
        val q = divideUnsigned(Long.MinValue, width)
        val r = remainderUnsigned(Long.MinValue, width)
        val n = (q << 1) + divideUnsigned(r << 1, width)
        n * width
      }

    }

}
