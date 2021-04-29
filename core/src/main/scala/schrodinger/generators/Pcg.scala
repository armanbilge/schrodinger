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

package schrodinger.generators
import cats.Id
import cats.data.StateT
import schrodinger.generators.Pcg.{State, State128, State64}
import schrodinger.util.UInt128

import java.lang

abstract class Pcg[S <: State] extends Generator[S] with BoxMullerGaussian[S]

abstract class Pcg32 extends Pcg[State64] with IntBasedGenerator[State64] {

  private val getState = StateT.inspect[Id, State64, Long](_.state)

  final override val nextInt: StateT[Id, State64, Int] = StateT[Id, State64, Int] { state =>
    (
      state.copy(state = state.state * 6364136223846793005L + (state.inc | 1)),
      output(state.state))
  }

  final override val split: StateT[Id, State64, State64] = for {
    inc <- nextLong
    state <- getState
  } yield State64(state, inc | 1)

  protected def output(state: Long): Int

  override protected val getExtraGaussian: StateT[Id, State64, Double] =
    StateT.inspect(_.extra)

  override protected def setExtraGaussian(x: Double): StateT[Id, State64, Unit] =
    StateT.modify(_.copy(extra = x))

}

class Pcg32XshRr extends Pcg32 {
  final override protected def output(state: Long): Int =
    lang.Integer.rotateRight((((state >>> 18) ^ state) >>> 27).toInt, (state >>> 59).toInt)
}

abstract class Pcg64 extends Pcg[State128] with LongBasedGenerator[State128] {

  private val getState =
    StateT.inspect[Id, State128, UInt128](s => UInt128(s.stateHi, s.stateLo))

  final override val nextLong: StateT[Id, State128, Long] = StateT[Id, State128, Long] {
    state =>
      import state._
      val s = UInt128(stateHi, stateLo) * UInt128(
        2549297995355413924L,
        4865540595714422341L) + UInt128(incHi, incLo | 1)
      (state.copy(stateHi = s.hi, stateLo = s.lo), output(stateHi, stateLo))
  }

  final override val split: StateT[Id, State128, State128] = for {
    incHi <- nextLong
    incLo <- nextLong
    state <- getState
  } yield State128(state.hi, state.lo, incHi, incLo | 1)

  protected def output(hi: Long, lo: Long): Long

  override protected val getExtraGaussian: StateT[Id, State128, Double] =
    StateT.inspect(_.extra)

  override protected def setExtraGaussian(x: Double): StateT[Id, State128, Unit] =
    StateT.modify(_.copy(extra = x))

}

class Pcg64XslRr extends Pcg64 {
  final override protected def output(hi: Long, lo: Long): Long =
    lang.Long.rotateRight(hi ^ lo, (hi >> 58).toInt)
}

object Pcg {

  implicit val DefaultInstance32: Pcg32 = new Pcg32XshRr
  implicit val DefaultInstance64: Pcg64 = new Pcg64XslRr

  sealed abstract class State
  final case class State64(state: Long, inc: Long, extra: Double = Double.NaN) extends State
  final case class State128(
      stateHi: Long,
      stateLo: Long,
      incHi: Long,
      incLo: Long,
      extra: Double = Double.NaN)
      extends State

}
