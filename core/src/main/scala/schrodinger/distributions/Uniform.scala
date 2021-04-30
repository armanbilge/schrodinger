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

package schrodinger.distributions

import cats.syntax.monad._
import cats.{Functor, Monad}
import schrodinger.RandomT

import java.lang
import scala.annotation.switch
import scala.collection.immutable.NumericRange

final class Uniform[F[_], S, A, U] private[distributions] (impl: UniformImpl[F, S, A, U])
    extends Serializable {
  def apply(a: A): RandomT[F, S, U] = impl(a)
}

object Uniform {

  def int[F[_], S](implicit U: Uniform[F, S, Unit, Int]): RandomT[F, S, Int] =
    U(())

  def long[F[_], S](implicit U: Uniform[F, S, Unit, Long]): RandomT[F, S, Long] =
    U(())

  def float[F[_], S](implicit U: Uniform[F, S, Unit, Float]): RandomT[F, S, Float] =
    U(())

  def double[F[_], S](implicit U: Uniform[F, S, Unit, Double]): RandomT[F, S, Double] =
    U(())

  def apply[F[_], S](range: Range)(implicit U: Uniform[F, S, Range, Int]): RandomT[F, S, Int] =
    U(range)

  def apply[F[_], S](range: NumericRange[Long])(
      implicit U: Uniform[F, S, NumericRange[Long], Long]): RandomT[F, S, Long] =
    U(range)

  def apply[F[_]: Functor, S, A](seq: Seq[A])(
      implicit U: Uniform[F, S, Range, Int]): RandomT[F, S, A] =
    U(seq.indices).map(seq)

  implicit def schrodingerDistributionsUniform[F[_], S, A, U](
      implicit impl: PrioritizeGenerator[UniformImpl[F, S, A, U]]): Uniform[F, S, A, U] =
    new Uniform(impl.join)
}

trait UniformImpl[F[_], S, A, U] extends Serializable {
  def apply(args: A): RandomT[F, S, U]
}

object UniformImpl {

  implicit def schrodingerDistributionsUniformForInt[F[_]: Functor, S](
      implicit U: Uniform[F, S, Unit, Long]): UniformImpl[F, S, Unit, Int] =
    new UniformImpl[F, S, Unit, Int] {
      override def apply(args: Unit): RandomT[F, S, Int] =
        Uniform.long.map(x => (x >>> 32).toInt)
    }

  implicit def schrodingerDistributionsUniformForLong[F[_]: Monad, S](
      implicit U: Uniform[F, S, Unit, Int]): UniformImpl[F, S, Unit, Long] =
    new UniformImpl[F, S, Unit, Long] {
      override def apply(args: Unit): RandomT[F, S, Long] = {
        val nextInt = Uniform.int
        for {
          hi <- nextInt
          lo <- nextInt
        } yield hi.toLong << 32 | lo.toLong
      }
    }

  implicit def schrodingerDistributionsUniformForFloat[F[_]: Functor, S](
      implicit U: Uniform[F, S, Unit, Int]): UniformImpl[F, S, Unit, Float] =
    new UniformImpl[F, S, Unit, Float] {
      override def apply(args: Unit): RandomT[F, S, Float] =
        Uniform.int.map(x => (x >>> 8) * 5.9604645e-8f)
    }

  implicit def schrodingerDistributionsUniformForDouble[F[_]: Functor, S](
      implicit U: Uniform[F, S, Unit, Long]): UniformImpl[F, S, Unit, Double] =
    new UniformImpl[F, S, Unit, Double] {
      override def apply(args: Unit): RandomT[F, S, Double] =
        Uniform.long.map(x => (x >>> 11) * 1.1102230246251565e-16)
    }

  private def nonNegativeInt[F[_]: Monad, S](
      nextInt: RandomT[F, S, Int],
      n: Int): RandomT[F, S, Int] =
    if ((n & -n) == n)
      nextInt.map(_ & (n - 1))
    else
      Monad[RandomT[F, S, *]]
        .iterateWhile(nextInt.map { x =>
          val b = x >>> 1
          val v = b % n
          (b, v)
        })(bv => bv._1 - bv._2 + (n - 1) < 0)
        .map(_._2)

  private def retryCap(width: Int): Int = {
    import lang.Integer._
    val q = divideUnsigned(Int.MinValue, width)
    val r = remainderUnsigned(Int.MinValue, width)
    val n = (q << 1) + divideUnsigned(r << 1, width)
    n * width
  }

  private def intRange[F[_]: Monad, S](
      nextInt: RandomT[F, S, Int],
      from: Int,
      to: Int): RandomT[F, S, Int] = {
    import lang.Integer._
    val width = to - from + 1
    if (width == 0)
      nextInt
    else {
      val cap =
        if (compareUnsigned(width, Int.MinValue) > 0)
          width
        else
          retryCap(width)
      if (cap == 0)
        nextInt.map(x => from + remainderUnsigned(x, width))
      else
        Monad[RandomT[F, S, *]]
          .iterateUntil(nextInt)(x => compareUnsigned(x, cap) <= 0)
          .map(x => remainderUnsigned(x, width) + from)
    }
  }

  implicit def schrodingerDistributionsUniformForRange[F[_]: Monad, S](
      implicit U: Uniform[F, S, Unit, Int]): UniformImpl[F, S, Range, Int] =
    new UniformImpl[F, S, Range, Int] {
      override def apply(range: Range): RandomT[F, S, Int] = {
        require(range.nonEmpty)
        val nextInt = Uniform.int
        if (range.start == 0 & range.step == 1)
          if (range.last == Int.MaxValue)
            nextInt.map(_ & Int.MaxValue)
          else
            nonNegativeInt(nextInt, range.last + 1)
        else
          (range.step: @switch) match {
            case 1 =>
              intRange(nextInt, range.start, range.last)
            case 2 =>
              val mask = -2 & (range.start % 2).abs
              intRange(nextInt, range.start, range.last).map(_ & mask)
              ??? // TODO Broken
            case _ =>
              apply(range.indices).map(range)
          }

      }
    }

  private def nonNegativeLong[F[_]: Monad, S](
      nextLong: RandomT[F, S, Long],
      n: Long): RandomT[F, S, Long] =
    if ((n & -n) == n)
      nextLong.map(_ & (n - 1))
    else
      nextLong
        .map { x =>
          val b = x >>> 1
          val v = b % n
          (b, v)
        }
        .iterateWhile(bv => bv._1 - bv._2 + (n - 1) < 0)
        .map(_._2)

  private def retryCap(width: Long): Long = {
    import lang.Long._
    val q = divideUnsigned(Int.MinValue, width)
    val r = remainderUnsigned(Int.MinValue, width)
    val n = (q << 1) + divideUnsigned(r << 1, width)
    n * width
  }

  private def longRange[F[_]: Monad, S](
      nextLong: RandomT[F, S, Long],
      from: Long,
      to: Long): RandomT[F, S, Long] = {
    import lang.Long._
    val width = to - from + 1
    if (width == 0)
      nextLong
    else {
      val cap =
        if (compareUnsigned(width, Long.MinValue) > 0)
          width
        else
          retryCap(width)
      if (cap == 0)
        nextLong.map(x => from + remainderUnsigned(x, width))
      else
        Monad[RandomT[F, S, *]]
          .iterateUntil(nextLong)(x => compareUnsigned(x, cap) <= 0)
          .map(x => remainderUnsigned(x, width) + from)
    }
  }

  implicit def schrodingerDistributionsUniformForLongRange[F[_]: Monad, S](
      implicit U: Uniform[F, S, Unit, Long]): UniformImpl[F, S, NumericRange[Long], Long] =
    new UniformImpl[F, S, NumericRange[Long], Long] {
      override def apply(range: NumericRange[Long]): RandomT[F, S, Long] = {
        require(range.nonEmpty)
        val nextLong = Uniform.long
        if (range.start == 0 & range.step == 1)
          if (range.last == Long.MaxValue)
            nextLong.map(_ & Long.MaxValue)
          else
            nonNegativeLong(nextLong, range.last + 1)
        else
          (range.step: @switch) match {
            case 1 =>
              longRange(nextLong, range.start, range.last)
            case _ =>
              ??? // TODO
          }
      }
    }
}
