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
import cats.effect.Async
import cats.effect.std.CountDownLatch
import cats.effect.syntax.all._
import cats.effect.{IO, IOApp, Ref}
import cats.syntax.all._
import schrodinger.distributions.Exponential
import schrodinger.effect.instances.dist._
import schrodinger.generators.SplitMix

import scala.concurrent.duration._

object ThoughtExperiment extends IOApp.Simple {

  sealed trait Cat
  case object LiveCat extends Cat
  case object DeadCat extends Cat

  val decayRate = math.log(2)

  def decayingAtom[F[_]: Async](geigerCounter: CountDownLatch[DistT[F, *]]): DistT[F, Unit] =
    for {
      decayAfter <- Exponential.applyF[F](decayRate)
      _ <- Async[DistT[F, *]].sleep(decayAfter.seconds)
      _ <- geigerCounter.release
    } yield ()

  def poisonRelay[F[_]: Monad](geigerCounter: CountDownLatch[F], cat: Ref[F, Cat]): F[Unit] =
    for {
      _ <- geigerCounter.await
      _ <- cat.set(DeadCat)
    } yield ()

  type DistIO[A] = DistT[IO, A]
  val experiment = for {
    cat <- Ref.of[DistIO, Cat](LiveCat)
    geigerCounter <- CountDownLatch[DistIO](1)
    // spawning fibers splits the RNG deterministically
    _ <- poisonRelay[DistIO](geigerCounter, cat).start
    _ <- decayingAtom[IO](geigerCounter).start
    _ <- Async[DistIO].sleep(1.second)
    // 50% probability that we will observe a live cat
    observation <- cat.get
    _ <- DistT.liftF(IO.println(s"observing a ${observation}"))
  } yield ()

  val seed1 = SplitMix.initialState(0x2b992ddfa23249d6L, 0x4034650f1c98bd69L)
  val seed2 = SplitMix.initialState(0x86d98163ff1fe751L, 0x8316a8fe31a2228eL)

  override def run: IO[Unit] =
    IO.print("Experiment 1: ") >> experiment.sample(seed1) >>
      IO.print("Experiment 2: ") >> experiment.sample(seed2) >>
      IO.println("No cats were harmed in the thinking of this experiment :)")
}
