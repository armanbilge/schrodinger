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
 * Copyright (c) 2000-2021 The Legion of the Bouncy Castle Inc. (https://www.bouncycastle.org)
 */

package schrodinger.unsafe

private object Threefish:

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

  def processBlock(
      key: Array[Long],
      block: Array[Long],
      out: Array[Long],
  ): Unit =

    val kw = new Array[Long](9)
    kw(0) = key(0); kw(5 + 0) = kw(0)
    kw(1) = key(1); kw(5 + 1) = kw(1)
    kw(2) = key(2); kw(5 + 2) = kw(2)
    kw(3) = key(3); kw(5 + 3) = kw(3)
    kw(5) = C240 ^ kw(0) ^ kw(1) ^ kw(2) ^ kw(3)

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
      b3 += kw(dm5 + 3)

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
