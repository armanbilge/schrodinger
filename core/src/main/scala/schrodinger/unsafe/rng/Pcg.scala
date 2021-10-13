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

package schrodinger.unsafe.rng
import schrodinger.math.UInt128

import java.lang

final case class Pcg32 private (private[rng] var state: Long, private[rng] var inc: Long)

object Pcg32:
  def apply(state: Long, inc: Long): Pcg32 =
    new Pcg32(state, inc | 1)

  sealed abstract class Pcg32Rng extends SplittableRng[Pcg32]:
    extension (s: Pcg32)
      final override def copy(): Pcg32 =
        Pcg32(s.state, s.inc)

      final override def nextInt(): Int =
        val x = output(s.state)
        s.state = s.state * 6364136223846793005L + s.inc
        x

      final override def nextLong(): Long =
        (s.nextInt().toLong << 32) | s.nextInt().toLong

      final override def split(): Pcg32 =
        val inc = s.nextLong()
        Pcg32(s.state, inc)

    protected def output(state: Long): Int

  object Pcg32XshRr extends Pcg32Rng:
    override protected def output(state: Long): Int =
      lang.Integer.rotateRight((((state >>> 18) ^ state) >>> 27).toInt, (state >>> 59).toInt)

  given schrodingerRngSplittableRngForPcg32: SplittableRng[Pcg32] = Pcg32XshRr

final case class Pcg64 private (
    private var stateHi: Long,
    private var stateLo: Long,
    private var incHi: Long,
    private var incLo: Long)

object Pcg64:
  def apply(stateHi: Long, stateLo: Long, incHi: Long, incLo: Long): Pcg64 =
    new Pcg64(stateHi, stateLo, incHi, incLo | 1)

  sealed abstract class Pcg64Rng extends SplittableRng[Pcg64]:
    extension (s: Pcg64)
      final override def copy(): Pcg64 =
        Pcg64(s.stateHi, s.stateLo, s.incHi, s.incLo)

      final override def nextInt(): Int =
        (s.nextLong() >>> 32).toInt

      final override def nextLong(): Long =
        import s.*
        val x = output(stateHi, stateLo)
        val state = UInt128(stateHi, stateLo) *
          UInt128(0x2360ed051fc65da4L, 0x4385df649fccf645L) +
          UInt128(incHi, incLo)
        stateHi = state.hi
        stateLo = state.lo
        x

      final override def split(): Pcg64 =
        val incHi = s.nextLong()
        val incLo = s.nextLong()
        Pcg64(s.stateHi, s.stateLo, incHi, incLo)

    protected def output(hi: Long, lo: Long): Long

  object Pcg64XslRr extends Pcg64Rng:
    override protected def output(hi: Long, lo: Long): Long =
      lang.Long.rotateRight(hi ^ lo, (hi >> 58).toInt)

  given schrodingerRngSplittableRngForPcg64: SplittableRng[Pcg64] = Pcg64XslRr
