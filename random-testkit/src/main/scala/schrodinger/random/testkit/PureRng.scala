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

package schrodinger.random.testkit

trait PureRng[S]:
  def nextInt(s: S): (S, Int)
  def nextLong(s: S): (S, Long)

object PureRng:
  def apply[S](using rng: PureRng[S]): rng.type = rng

trait PureRng32[S] extends PureRng[S]:
  def nextInt(s: S): (S, Int)

  final def nextLong(s0: S): (S, Long) =
    val (s1, i0) = nextInt(s0)
    val (s2, i1) = nextInt(s1)
    (s2, (i0.toLong << 32) | i1.toLong)

opaque type Pcg32 = Long
object Pcg32:
  def apply(state: Long): Pcg32 = state

  given PureRng[Pcg32] with PureRng32[Pcg32] with
    def nextInt(s0: Long): (Long, Int) =
      import Integer.*
      val s1 = s0 * 6364136223846793005L + 1442695040888963407L
      (s1, rotateRight(((s0 ^ (s0 >>> 18)) >>> 27).toInt, (s0 >>> 59).toInt))
