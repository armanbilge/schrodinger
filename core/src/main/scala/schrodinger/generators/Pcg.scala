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
import cats.{Applicative, Monad}
import cats.data.StateT
import schrodinger.RandomT
import schrodinger.random.{GeneratorOverride, Uniform, UniformImpl}
import schrodinger.util.UInt128

import java.lang

final case class Pcg32(state: Long, inc: Long, extra: Double = Double.NaN)

object Pcg32XshRr extends Pcg32Instances {
  final override protected def output(state: Long): Int =
    lang.Integer.rotateRight((((state >>> 18) ^ state) >>> 27).toInt, (state >>> 59).toInt)
}

private[generators] abstract class Pcg32Instances {
  implicit def shrodingerGeneratorsSplitForPcg32[F[_]: Monad](
      implicit
      nextLong: Uniform[F, Pcg32, Unit, Long]): Split[F, Pcg32] with GeneratorOverride =
    new Split[F, Pcg32] with GeneratorOverride {
      override def split: RandomT[F, Pcg32, Pcg32] = for {
        inc <- nextLong(())
        state <- RandomT(StateT.inspect[F, Pcg32, Long](_.state))
      } yield Pcg32(state, inc | 1)
    }

  implicit def schrodingerGeneratorsUniformIntForPcg32[F[_]](
      implicit F: Applicative[F]): UniformImpl[F, Pcg32, Unit, Int] with GeneratorOverride =
    new UniformImpl[F, Pcg32, Unit, Int] with GeneratorOverride {
      override def apply(args: Unit): RandomT[F, Pcg32, Int] = RandomT(
        StateT[F, Pcg32, Int] { state =>
          F.pure(
            (
              state.copy(state = state.state * 6364136223846793005L + (state.inc | 1)),
              output(state.state)))
        }
      )
    }

  protected def output(state: Long): Int
}

final case class Pcg64(
    stateHi: Long,
    stateLo: Long,
    incHi: Long,
    incLo: Long,
    extra: Double = Double.NaN)

object Pcg64XslRr extends Pcg64Instances {
  override protected def output(hi: Long, lo: Long): Long =
    lang.Long.rotateRight(hi ^ lo, (hi >> 58).toInt)
}

private[generators] sealed abstract class Pcg64Instances {
  implicit def shrodingerGeneratorsSplitForPcg64[F[_]: Monad](
      implicit
      nextLong: Uniform[F, Pcg64, Unit, Long]): Split[F, Pcg64] with GeneratorOverride =
    new Split[F, Pcg64] with GeneratorOverride {
      override def split: RandomT[F, Pcg64, Pcg64] = for {
        incHi <- nextLong(())
        incLo <- nextLong(())
        state <- RandomT(StateT.get[F, Pcg64])
      } yield Pcg64(state.stateHi, state.stateLo, incHi, incLo | 1)
    }

  implicit def schrodingerGeneratorsUniformLongForPcg64[F[_]](
      implicit F: Applicative[F]): UniformImpl[F, Pcg64, Unit, Long] with GeneratorOverride =
    new UniformImpl[F, Pcg64, Unit, Long] with GeneratorOverride {
      override def apply(args: Unit): RandomT[F, Pcg64, Long] = RandomT(
        StateT[F, Pcg64, Long] { state =>
          import state._
          val s = UInt128(stateHi, stateLo) * UInt128(
            2549297995355413924L,
            4865540595714422341L) + UInt128(incHi, incLo | 1)
          F.pure((state.copy(stateHi = s.hi, stateLo = s.lo), output(stateHi, stateLo)))
        }
      )
    }

  protected def output(hi: Long, lo: Long): Long
}
