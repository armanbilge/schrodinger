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
    private var bseq: Long,
    private var ctr: Long,
    // indices
    private var bIndex: Byte,
    private var bseqIndex: Byte,
) extends Serializable

object Threefish:

  given SplittableRng[Threefish] with
    extension (tf: Threefish)
      def copy() =
        import tf.*
        Threefish(s0, s1, s2, s3, b0, b1, b2, b3, bseq, ctr, bIndex, bseqIndex)

      def nextInt() =
        ???

      def nextLong() =
        ???

      def split() =
        val right = copy()
        goLeft()
        right.goRight()
        right

      private def goLeft(): Unit =
        ???

      private def goRight(): Unit =
        ???

      private def incrementCtr(): Unit =
        if tf.ctr == Long.MaxValue then goLeft()
        else
          tf.ctr += 1
          rehash()

      private def rehash(): Unit =
        import tf.*

        val key = Array(s0, s1, s2, s3)
        val block = Array(bseq, ctr, 0, 0)
        val out = new Array[Long](4)
        processBlock(key, block, out)
        b0 = out(0)
        b1 = out(1)
        b2 = out(2)
        b3 = out(3)

        bseq = 0
        ctr = 0
        bIndex = 0
        bseqIndex = 0

      private def reseed(): Unit =
        import tf.*
        rehash()
        s0 = b0
        s1 = b1
        s2 = b2
        s3 = b3
        rehash()

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
