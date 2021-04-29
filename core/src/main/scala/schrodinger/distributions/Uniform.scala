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

import cats.{Applicative, Id, Monad}
import cats.syntax.monad._
import schrodinger.DistT.*=>
import schrodinger.generators.Generator
import schrodinger.{Dist, DistT}

import java.lang
import scala.annotation.switch
import scala.collection.immutable.NumericRange

object Uniform {

  val int: Dist[Int] = DistT[Id, Int](new (Id *=> λ[S => (S, Int)]) {
    override def apply[S](s: S)(implicit rng: Generator[S]): (S, Int) =
      rng.nextInt.run(s)
  })

  def intF[F[_]: Applicative]: DistT[F, Int] =
    DistT.fromDist(int)

  val long: Dist[Long] = DistT[Id, Long](new (Id *=> λ[S => (S, Long)]) {
    override def apply[S](s: S)(implicit rng: Generator[S]): (S, Long) =
      rng.nextLong.run(s)
  })

  def longF[F[_]: Applicative]: DistT[F, Long] =
    DistT.fromDist(long)

  val float: Dist[Float] = DistT[Id, Float](new (Id *=> λ[S => (S, Float)]) {
    override def apply[S](s: S)(implicit rng: Generator[S]): (S, Float) =
      rng.nextFloat.run(s)
  })

  def floatF[F[_]](implicit F: Applicative[F]): DistT[F, Float] =
    DistT.fromDist(float)

  val double: Dist[Double] = DistT[Id, Double](new (Id *=> λ[S => (S, Double)]) {
    override def apply[S](s: S)(implicit rng: Generator[S]): (S, Double) =
      rng.nextDouble.run(s)
  })

  def doubleF[F[_]](implicit F: Applicative[F]): DistT[F, Double] =
    DistT.fromDist(double)

  private def nonNegativeInt(n: Int): Dist[Int] =
    if ((n & -n) == n)
      int.map(_ & (n - 1))
    else
      Monad[Dist]
        .iterateWhile(int.map { x =>
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

  private def intRange(from: Int, to: Int): Dist[Int] = {
    import lang.Integer._
    val width = to - from + 1
    if (width == 0)
      int
    else {
      val cap =
        if (compareUnsigned(width, Int.MinValue) > 0)
          width
        else
          retryCap(width)
      if (cap == 0)
        int.map(x => from + remainderUnsigned(x, width))
      else
        Monad[Dist]
          .iterateUntil(int)(x => compareUnsigned(x, cap) <= 0)
          .map(x => remainderUnsigned(x, width) + from)
    }
  }

  private def nonNegativeLong(n: Long): Dist[Long] =
    if ((n & -n) == n)
      long.map(_ & (n - 1))
    else
      long
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

  private def longRange(from: Long, to: Long): Dist[Long] = {
    import lang.Long._
    val width = to - from + 1
    if (width == 0)
      long
    else {
      val cap =
        if (compareUnsigned(width, Long.MinValue) > 0)
          width
        else
          retryCap(width)
      if (cap == 0)
        long.map(x => from + remainderUnsigned(x, width))
      else
        Monad[Dist]
          .iterateUntil(long)(x => compareUnsigned(x, cap) <= 0)
          .map(x => remainderUnsigned(x, width) + from)
    }
  }

  def apply(range: Range): Dist[Int] = {
    require(range.nonEmpty)
    if (range.start == 0 & range.step == 1)
      if (range.last == Int.MaxValue)
        int.map(_ & Int.MaxValue)
      else
        nonNegativeInt(range.last + 1)
    else
      (range.step: @switch) match {
        case 1 =>
          intRange(range.start, range.last)
        case 2 =>
          val mask = -2 & (range.start % 2).abs
          intRange(range.start, range.last).map(_ & mask)
          ??? // TODO Broken
        case _ =>
          apply(range.indices).map(range)
      }
  }

  def applyF[F[_]: Applicative, A](range: Range): DistT[F, Int] =
    DistT.fromDist(apply(range))

  def apply(range: NumericRange[Long]): Dist[Long] = {
    require(range.nonEmpty)
    if (range.start == 0 & range.step == 1)
      if (range.last == Long.MaxValue)
        long.map(_ & Long.MaxValue)
      else
        nonNegativeLong(range.last + 1)
    else
      (range.step: @switch) match {
        case 1 =>
          longRange(range.start, range.last)
        case _ =>
          ??? // TODO
      }
  }

  def applyF[F[_]: Applicative, A](range: NumericRange[Long]): DistT[F, Long] =
    DistT.fromDist(apply(range))

  def apply[A](seq: Seq[A]): Dist[A] =
    apply(seq.indices).map(seq)

  def applyF[F[_]: Applicative, A](seq: Seq[A]): DistT[F, A] =
    DistT.fromDist(apply(seq))

}
