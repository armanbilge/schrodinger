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

import algebra.ring.CommutativeRig
import algebra.ring.Field
import algebra.ring.MultiplicativeCommutativeGroup
import cats.kernel.Hash
import cats.kernel.Order

opaque type LogDouble = Double

object LogDouble:
  val Zero: LogDouble = Double.NegativeInfinity
  val One: LogDouble = 0.0
  val Two: LogDouble = 0.6931471805599453
  val NaN: LogDouble = Double.NaN

  def apply(x: Double): LogDouble = math.log(x)
  def exp(x: Double): LogDouble = x

  def sum(xs: IArray[LogDouble]): LogDouble =
    var max = LogDouble.Zero.log
    var i = 0
    while i < xs.length do
      max = max.max(xs(i))
      i += 1
    var sum = 0.0
    i = 0
    while i < xs.length do
      sum += (xs(i) / max).real
      i += 1
    LogDouble(sum) * max

  extension (x: LogDouble)
    def real: Double = math.exp(x)
    def log: Double = x
    def +(y: LogDouble): LogDouble =
      if x > y then math.log1p(math.exp(y - x)) + x
      else if y > x then math.log1p(math.exp(x - y)) + y
      else Two * x
    def *(y: LogDouble): LogDouble = (x: Double) + (y: Double)
    def /(y: LogDouble): LogDouble = x - y
    def reciprocal: LogDouble = -x
    infix def max(y: LogDouble): LogDouble = math.max(x, y)
    infix def min(y: LogDouble): LogDouble = math.min(x, y)
    infix def compare(y: LogDouble): Int = java.lang.Double.compare(x, y)
    def <(y: LogDouble): Boolean = (x: Double) < (y: Double)
    def <=(y: LogDouble): Boolean = (x: Double) <= (y: Double)
    def >(y: LogDouble): Boolean = (x: Double) > (y: Double)
    def >=(y: LogDouble): Boolean = (x: Double) >= (y: Double)

  abstract class LogDoubleAlgebra private[LogDouble] ()
      extends CommutativeRig[LogDouble],
        MultiplicativeCommutativeGroup[LogDouble],
        Order[LogDouble],
        Hash[LogDouble]

  given schrodingerAlgebraForLogDouble: LogDoubleAlgebra with
    override def zero: LogDouble = LogDouble.Zero
    override def one: LogDouble = LogDouble.One
    override def plus(x: LogDouble, y: LogDouble) = LogDouble.+(x)(y)
    override def times(x: LogDouble, y: LogDouble) = LogDouble.*(x)(y)
    override def div(x: LogDouble, y: LogDouble) = LogDouble./(x)(y)
    override def reciprocal(x: LogDouble) = x.reciprocal
    override def compare(x: LogDouble, y: LogDouble) = LogDouble.compare(x)(y)
    override def hash(x: LogDouble): Int = x.hashCode
