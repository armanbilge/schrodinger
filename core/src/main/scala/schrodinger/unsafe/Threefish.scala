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

/*
 * Copyright (c) 2012-2013, Michał Pałka
 * Licensed under the BSD-3 license
 */

/*
 * Copyright (c) 2000-2021 The Legion of the Bouncy Castle Inc. (https://www.bouncycastle.org)
 * Licensed under the MIT license
 */

package schrodinger.unsafe

import cats.Apply
import cats.effect.std.SecureRandom
import cats.syntax.all.*

import Threefish.*

final class Threefish private (
    // state
    private var s0: Long,
    private var s1: Long,
    private var s2: Long,
    private var s3: Long,
    // output
    private var b0: Long,
    private var b1: Long,
    private var b2: Long,
    private var b3: Long,
    // path
    private var bseq0: Long,
    private var bseq1: Long,
    private var ctr: Int,
    // indices
    private var bIndex: Byte,
    private var bseqLength: Byte,
) extends Serializable:

  private[unsafe] def state(): (Long, Long, Long, Long) = (s0, s1, s2, s3)

  private[unsafe] def debug(): String =
    def toBinString(l: Long) =
      val s = l.toBinaryString
      "0" * (64 - s.length) + s

    s"""|Threefish(
        |  s = ${(s0, s1, s2, s3)},
        |  b = ${(b0, b1, b2, b3)},
        |  bIndex = $bIndex,
        |  bseq = ${toBinString(bseq0)}${toBinString(bseq1)},
        |  bseqLength = $bseqLength,
        |)""".stripMargin

  private[unsafe] def copy() =
    new Threefish(s0, s1, s2, s3, b0, b1, b2, b3, bseq0, bseq1, ctr, bIndex, bseqLength)

  private[unsafe] def nextInt() =
    if bIndex >= 8 then incrementCtr()

    val bi = Array(b0, b1, b2, b3)(bIndex / 2)
    val shift = 32 * (bIndex % 2)
    val rtn = Int.MaxValue & (bi >>> shift)

    bIndex = (bIndex + 1).toByte

    rtn.toInt

  private[unsafe] def nextLong() =
    if bIndex >= 7 then incrementCtr()

    bIndex = (bIndex + (bIndex % 2)).toByte
    val rtn = Array(b0, b1, b2, b3)(bIndex / 2)

    bIndex = (bIndex + 2).toByte

    rtn

  private[unsafe] def split() =
    if bseqLength == -128 then // overflow
      reseed()

    val i = bseqLength
    bseqLength = (bseqLength + 1).toByte

    val left = copy()
    left.rehash()

    // go right
    setBseq(i)
    rehash()

    left

  private inline def setBseq(i: Int): Unit =
    if i < 64 then bseq0 |= 1L << i
    else bseq1 |= 1L << (i - 64)

  private def incrementCtr(): Unit =
    if ctr == -1 then // max uint
      reseed()
      rehash()
    else
      ctr += 1
      rehash()

  private def rehash(): Unit =

    val key = Array(s0, s1, s2, s3)
    val block = Array(bseq0, bseq1, ctr, bseqLength)
    val out = new Array[Long](4)
    processBlock(key, block, out)
    b0 = out(0)
    b1 = out(1)
    b2 = out(2)
    b3 = out(3)
    bIndex = 0

  private def reseed(): Unit =
    bseqLength = -1 // never happens naturally, so guarantees prefix-free

    rehash()

    s0 = b0
    s1 = b1
    s2 = b2
    s3 = b3

    bseq0 = 0
    bseq1 = 0
    ctr = 0
    bseqLength = 0

object Threefish:

  def apply(s0: Long, s1: Long, s2: Long, s3: Long): Threefish =
    val tf = new Threefish(s0, s1, s2, s3, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    tf.rehash()
    tf

  def fromSecureRandom[F[_]: Apply](random: SecureRandom[F]): F[Threefish] =
    (random.nextLong, random.nextLong, random.nextLong, random.nextLong)
      .mapN(Threefish(_, _, _, _))

  given SplittableRng[Threefish] with
    extension (tf: Threefish)
      def copy() = tf.copy()
      def nextInt() = tf.nextInt()
      def nextLong() = tf.nextLong()
      def split() = tf.split()

  private final val C240 = 0x1bd11bdaa9fc1a22L

  private final val Rounds = 72

  private final val R00 = 14; private final val R01 = 16
  private final val R10 = 52; private final val R11 = 57
  private final val R20 = 23; private final val R21 = 40
  private final val R30 = 5; private final val R31 = 37

  private final val R40 = 25; private final val R41 = 33
  private final val R50 = 46; private final val R51 = 12
  private final val R60 = 58; private final val R61 = 22
  private final val R70 = 32; private final val R71 = 32

  private inline def rotlXor(x: Long, n: Int, xor: Long): Long =
    ((x << n) | (x >>> -n)) ^ xor

  private[unsafe] def processBlock(
      key: Array[Long],
      block: Array[Long],
      out: Array[Long],
  ): Unit =

    val kw = new Array[Long](9)
    kw(0) = key(0); kw(5 + 0) = kw(0)
    kw(1) = key(1); kw(5 + 1) = kw(1)
    kw(2) = key(2); kw(5 + 2) = kw(2)
    kw(3) = key(3); kw(5 + 3) = kw(3)
    kw(4) = C240 ^ kw(0) ^ kw(1) ^ kw(2) ^ kw(3)

    var b0 = block(0)
    var b1 = block(1)
    var b2 = block(2)
    var b3 = block(3)

    b0 += kw(0)
    b1 += kw(1)
    b2 += kw(2)
    b3 += kw(3)

    var d = 1
    var dm5 = 1
    while d < Rounds / 4 do

      b0 += b1
      b1 = rotlXor(b1, R00, b0)
      b2 += b3
      b3 = rotlXor(b3, R01, b2)

      b0 += b3
      b3 = rotlXor(b3, R10, b0)
      b2 += b1
      b1 = rotlXor(b1, R11, b2)

      b0 += b1
      b1 = rotlXor(b1, R20, b0)
      b2 += b3
      b3 = rotlXor(b3, R21, b2)

      b0 += b3
      b3 = rotlXor(b3, R30, b0)
      b2 += b1
      b1 = rotlXor(b1, R31, b2)

      b0 += kw(dm5)
      b1 += kw(dm5 + 1)
      b2 += kw(dm5 + 2)
      b3 += kw(dm5 + 3) + d

      b0 += b1
      b1 = rotlXor(b1, R40, b0)
      b2 += b3
      b3 = rotlXor(b3, R41, b2)

      b0 += b3
      b3 = rotlXor(b3, R50, b0)
      b2 += b1
      b1 = rotlXor(b1, R51, b2)

      b0 += b1
      b1 = rotlXor(b1, R60, b0)
      b2 += b3
      b3 = rotlXor(b3, R61, b2)

      b0 += b3
      b3 = rotlXor(b3, R70, b0)
      b2 += b1
      b1 = rotlXor(b1, R71, b2)

      b0 += kw(dm5 + 1)
      b1 += kw(dm5 + 2)
      b2 += kw(dm5 + 3)
      b3 += kw(dm5 + 4) + d + 1

      d += 2
      dm5 = (dm5 + 2) % 5

    out(0) = b0
    out(1) = b1
    out(2) = b2
    out(3) = b3
