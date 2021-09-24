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

final case class Interval[+L <: Bound, +U <: Bound](lower: L, upper: U)
sealed abstract class Bound
object Bound:
  final case class Open[+A](a: A) extends Bound
  final case class Closed[+A](a: A) extends Bound

object Interval:
  import Bound.*

  type <=@<[+A, +B] = Interval[Closed[A], Open[B]]
  type <=@<=[+A, +B] = Interval[Closed[A], Closed[B]]
  type <@<=[+A, +B] = Interval[Open[A], Closed[B]]
  type <@<[+A, +B] = Interval[Open[A], Open[B]]

  extension [A <: Singleton](inline a: A)
    inline def <=@<[B <: Singleton](inline b: B): A <=@< B = Interval(Closed(a), Open(b))
    inline def <=@<=[B <: Singleton](inline b: B): A <=@<= B = Interval(Closed(a), Closed(b))
    inline def <@<=[B <: Singleton](inline b: B): A <@<= B = Interval(Open(a), Closed(b))
    inline def <@<[B <: Singleton](inline b: B): A <@< B = Interval(Open(a), Open(b))
