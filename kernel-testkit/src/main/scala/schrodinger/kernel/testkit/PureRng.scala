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

package schrodinger.kernel.testkit

import org.scalacheck.Arbitrary

trait PureRng[S]:
  def nextInt(s: S): (S, Int)
  def nextLong(s: S): (S, Long)

object PureRng:
  def apply[S](using rng: PureRng[S]): rng.type = rng

trait PureRng64[S] extends PureRng[S]:
  final def nextInt(s0: S): (S, Int) =
    val (s1, x) = nextLong(s0)
    (s1, (x >>> 32).toInt)

  def nextLong(s: S): (S, Long)

opaque type SplitMix64 = Long
object SplitMix64:
  def apply(state: Long): SplitMix64 = state

  given PureRng[SplitMix64] with PureRng64[SplitMix64] with
    def nextLong(s0: Long): (Long, Long) =
      val s1 = s0 + 0x9e3779b97f4a7c15L
      var z = s1
      z = (z ^ z >>> 30) * 0xbf58476d1ce4e5b9L
      z = (z ^ z >>> 27) * 0x94d049bb133111ebL
      (s1, z ^ z >>> 31)

  given Arbitrary[SplitMix64] = Arbitrary(Arbitrary.arbLong.arbitrary.map(SplitMix64(_)))
