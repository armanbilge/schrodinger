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

package schrodinger.kernel

import scala.annotation.targetName

opaque type LogDouble = Double

object LogDouble:
  def Zero: LogDouble = Double.NegativeInfinity
  def One: LogDouble = 0.0
  def NaN: LogDouble = Double.NaN

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
    def *(y: LogDouble): LogDouble = x + y
    def /(y: LogDouble): LogDouble = x - y
    def reciprocal: LogDouble = -x
    infix def max(y: LogDouble): LogDouble = math.max(x, y)
    infix def min(y: LogDouble): LogDouble = math.min(x, y)
    def <(y: LogDouble): Boolean = (x: Double) < (y: Double)
    def <=(y: LogDouble): Boolean = (x: Double) <= (y: Double)
    def >(y: LogDouble): Boolean = (x: Double) > (y: Double)
    def >=(y: LogDouble): Boolean = (x: Double) >= (y: Double)
