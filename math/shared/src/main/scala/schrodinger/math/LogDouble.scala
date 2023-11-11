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

package schrodinger.math

import algebra.ring.CommutativeSemifield
import cats.kernel.Hash
import cats.kernel.Order

import scala.annotation.nowarn

opaque type LogDouble = Double

object LogDouble:
  final val Zero: LogDouble = Double.NegativeInfinity
  final val One: LogDouble = 0.0
  final val Two: LogDouble = 0.6931471805599453
  final val MinPositiveValue: LogDouble = -Double.MaxValue
  final val MaxValue: LogDouble = Double.MaxValue
  final val PositiveInfinity: LogDouble = Double.PositiveInfinity
  final val NaN: LogDouble = Double.NaN

  inline def apply(x: Double): LogDouble = Math.log(x)
  inline def `1+`(x: Double): LogDouble = Math.log1p(x)
  inline def exp(x: Double): LogDouble = x

  def sum(xs: IArray[LogDouble]): LogDouble =
    var max = LogDouble.Zero
    var i = 0
    while i < xs.length do
      max = LogDouble.max(max)(xs(i))
      i += 1
    var sum = 0.0
    i = 0
    while i < xs.length do
      sum += LogDouble./(xs(i))(max).real
      i += 1
    LogDouble.*(LogDouble(sum))(max)

  def sum(xs: Iterable[LogDouble]): LogDouble =
    val max = xs.fold(LogDouble.Zero)(LogDouble.max(_)(_))
    val sum = xs.fold(0.0)(_ + LogDouble./(_)(max).real)
    LogDouble.*(LogDouble(sum))(max)

  extension (x: LogDouble)
    inline def real: Double = Math.exp(x)
    inline def log: Double = x
    inline def +(y: LogDouble): LogDouble =
      if x > y then Math.log1p(Math.exp(y - x)) + x
      else if y > x then Math.log1p(Math.exp(x - y)) + y
      else LogDouble.*(Two)(x)
    inline def -(y: LogDouble): LogDouble = Math.log1p(-Math.exp(y - x)) + x
    inline def *(y: LogDouble): LogDouble = (x: Double) + (y: Double)
    inline def **(y: Double): LogDouble = (x: Double) * y
    inline def /(y: LogDouble): LogDouble = x - y
    inline def reciprocal: LogDouble = -x
    inline def isNaN: Boolean = java.lang.Double.isNaN(x)
    inline infix def max(y: LogDouble): LogDouble = Math.max(x, y)
    inline infix def min(y: LogDouble): LogDouble = Math.min(x, y)
    inline infix def compare(y: LogDouble): Int = java.lang.Double.compare(x, y)
    inline def <(y: LogDouble): Boolean = (x: Double) < (y: Double)
    inline def <=(y: LogDouble): Boolean = (x: Double) <= (y: Double)
    inline def >(y: LogDouble): Boolean = (x: Double) > (y: Double)
    inline def >=(y: LogDouble): Boolean = (x: Double) >= (y: Double)

  given CommutativeSemifield[LogDouble]
    with Monus[LogDouble]
    with Order[LogDouble]
    with Hash[LogDouble]
    with
    override def zero: LogDouble = LogDouble.Zero
    override def one: LogDouble = LogDouble.One
    override def plus(x: LogDouble, y: LogDouble) = LogDouble.+(x)(y)
    override def monus(x: LogDouble, y: LogDouble) =
      if gt(x, y) then LogDouble.-(x)(y) else zero
    override def positiveSumN(x: LogDouble, n: Int): LogDouble = LogDouble.*(x)(LogDouble(n))
    override def sum(as: TraversableOnce[LogDouble] @nowarn("cat=deprecation")): LogDouble =
      as.asInstanceOf[Matchable] match
        case iterable: Iterable[LogDouble] @unchecked => LogDouble.sum(iterable)
        case _ => super.sum(as)
    override def times(x: LogDouble, y: LogDouble) = LogDouble.*(x)(y)
    override def div(x: LogDouble, y: LogDouble) = LogDouble./(x)(y)
    override def reciprocal(x: LogDouble) = x.reciprocal
    override def compare(x: LogDouble, y: LogDouble) = LogDouble.compare(x)(y)
    override def hash(x: LogDouble): Int = x.hashCode
    override def additiveCommutativeMonoid = this
    override def naturalOrder = this
