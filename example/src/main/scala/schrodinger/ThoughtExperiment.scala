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

package schrodinger

import cats.Monad
import cats.effect.std.CountDownLatch
import cats.effect.syntax.all._
import cats.effect.{Async, IO, IOApp, Ref}
import cats.syntax.all._
import schrodinger.random.{Exponential, ExponentialDouble}
import schrodinger.effect.instances.SplitMonadCancel
import schrodinger.effect.instances.random._
import schrodinger.generators.SplitMix

import scala.concurrent.duration._

object ThoughtExperiment extends IOApp.Simple {

  sealed trait Cat
  case object LiveCat extends Cat
  case object DeadCat extends Cat

  val decayRate = math.log(2)

  def decayingAtom[F[_]: Async, S: SplitMonadCancel](
      geigerCounter: CountDownLatch[RandomT[F, S, *]])(implicit E: ExponentialDouble[F, S]) =
    for {
      decayAfter <- Exponential[F, S](decayRate)
      _ <- Async[RandomT[F, S, *]].sleep(decayAfter.seconds)
      _ <- geigerCounter.release
    } yield ()

  def poisonRelay[F[_]: Monad](geigerCounter: CountDownLatch[F], cat: Ref[F, Cat]) =
    for {
      _ <- geigerCounter.await
      _ <- cat.set(DeadCat)
    } yield ()

  def experiment[F[_]: Async, S: SplitMonadCancel](implicit E: ExponentialDouble[F, S]) =
    for {
      cat <- Ref.of[RandomT[F, S, *], Cat](LiveCat)
      geigerCounter <- CountDownLatch[RandomT[F, S, *]](1)
      // spawning fibers splits the RNG deterministically
      _ <- poisonRelay[RandomT[F, S, *]](geigerCounter, cat).start
      _ <- decayingAtom[F, S](geigerCounter).start
      _ <- Async[RandomT[F, S, *]].sleep(1.second)
      // 50% probability that we will observe a live cat
      observation <- cat.get
    } yield observation

  val seed1 = SplitMix.initialState(0x2b992ddfa23249d6L, 0x4034650f1c98bd69L)
  val seed2 = SplitMix.initialState(0x86d98163ff1fe751L, 0x8316a8fe31a2228eL)

  override def run: IO[Unit] = for {
    observation1 <- experiment[IO, SplitMix].simulate(seed1)
    _ <- IO.println(s"Experiment 1: observing a $observation1")
    observation2 <- experiment[IO, SplitMix].simulate(seed2)
    _ <- IO.println(s"Experiment 2: observing a $observation2")
    _ <- IO.println("No cats were harmed in the thinking of this experiment :)")
  } yield ()
}
