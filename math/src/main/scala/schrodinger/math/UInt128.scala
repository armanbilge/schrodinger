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

import java.lang.Long.*
import scala.annotation.tailrec

final case class UInt128(hi: Long, lo: Long):

  def +(that: UInt128): UInt128 =
    val lo = this.lo + that.lo
    val carry = if compareUnsigned(lo, this.lo) < 0 then 1 else 0
    val hi = this.hi + that.hi + carry
    UInt128(hi, lo)

  def *(that: UInt128): UInt128 =
    val lo = this.lo * that.lo
    val hi = multiplyHighUnsigned(this.lo, that.lo) +
      this.lo * that.hi + this.hi * that.lo
    UInt128(hi, lo)

  private def multiplyHighUnsigned(x: Long, y: Long) =
    val x0 = x & 0xffffffffL
    val x1 = x >>> 32

    val y0 = y & 0xffffffffL
    val y1 = y >>> 32

    val z0 = x0 * y0
    val t = x1 * y0 + (z0 >>> 32)
    var z1 = t & 0xffffffffL
    val z2 = t >>> 32
    z1 += x0 * y1

    x1 * y1 + z2 + (z1 >>> 32)

  def |(that: UInt128) =
    val lo = this.lo | that.lo
    val hi = this.hi | that.hi
    UInt128(hi, lo)

  def &(that: UInt128) =
    val lo = this.lo & that.lo
    val hi = this.hi & that.hi
    UInt128(hi, lo)

  def ^(that: UInt128) =
    val lo = this.lo ^ that.lo
    val hi = this.hi ^ that.hi
    UInt128(hi, lo)

  @tailrec
  def <<(n: Int): UInt128 =
    if n >= 128 then this << (n % 128)
    else if n >= 64 then UInt128(lo << (n - 64), 0)
    else if n > 0 then UInt128((hi << n) | (lo >>> (64 - n)), lo << n)
    else this

  @tailrec
  def >>(n: Int): UInt128 =
    if n >= 128 then this >> (n % 128)
    else if n >= 64 then UInt128(0, hi >>> (n - 64))
    else if n > 0 then UInt128(hi >>> n, (hi << (64 - n)) | (lo >>> n))
    else this

  def toBigInt =
    BigInt(toUnsignedString(hi, 16) + "0000000000000000", 16) + BigInt(
      toUnsignedString(lo, 16),
      16)

object UInt128:

  def apply(x: BigInt): UInt128 =
    val s = x.toString(16)
    val lo = parseUnsignedLong(s.takeRight(16), 16)
    val hi = parseUnsignedLong("0" + s.dropRight(16), 16)
    UInt128(hi, lo)
