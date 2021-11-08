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

import cats.Align
import cats.Parallel
import cats.SemigroupK
import cats.Show
import cats.effect.IO
import cats.effect.IOLocal
import cats.effect.instances.spawn
import cats.effect.kernel.Async
import cats.effect.kernel.Cont
import cats.effect.kernel.Deferred
import cats.effect.kernel.Fiber
import cats.effect.kernel.Poll
import cats.effect.kernel.Ref
import cats.effect.kernel.Sync
import cats.effect.std.Console
import cats.kernel.Monoid
import cats.kernel.Semigroup
import schrodinger.kernel.PseudoRandom
import schrodinger.random.GaussianCache
import schrodinger.unsafe.rng.SplittableRng

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.FiniteDuration
import scala.util.NotGiven

opaque type RVIO[S, +A] = IO[A]

object RVIO:

  given [S0: SplittableRng]: Async[RVIO[S0, _]]
    with PseudoRandom[RVIO[S0, _]]
    with RngDispatcher[RVIO[S0, _]]
    with GaussianCache[RVIO[S0, _], Double]
    with
    type G[A] = IO[A]
    type S = S0

    def rng = summon[SplittableRng[S0]]

    final class State(
        val rng: S = null.asInstanceOf[S],
        var cachedGaussian: Double = Double.NaN)
    private val state = IOLocal[State](null).syncStep.unsafeRunSync().toOption.get

    def pure[A](x: A): RVIO[S, A] = IO.pure(x)

    def handleErrorWith[A](fa: RVIO[S, A])(f: Throwable => RVIO[S, A]): RVIO[S, A] =
      fa.handleErrorWith(f)

    def raiseError[A](e: Throwable): RVIO[S, A] = IO.raiseError(e)

    def cont[K, R](body: Cont[RVIO[S, _], K, R]): RVIO[S0, R] = IO.cont(body)

    def evalOn[A](fa: RVIO[S, A], ec: ExecutionContext): RVIO[S, A] = fa.evalOn(ec)

    def executionContext: RVIO[S, ExecutionContext] = IO.executionContext

    def monotonic: RVIO[S, FiniteDuration] = IO.monotonic

    def realTime: RVIO[S, FiniteDuration] = IO.realTime

    def flatMap[A, B](fa: RVIO[S, A])(f: A => RVIO[S0, B]): RVIO[S, B] =
      fa.flatMap(f)

    def tailRecM[A, B](a: A)(f: A => RVIO[S, Either[A, B]]): RVIO[S, B] =
      IO.asyncForIO.tailRecM(a)(f)

    def deferred[A]: RVIO[S, Deferred[RVIO[S, _], A]] = IO.deferred

    def ref[A](a: A): RVIO[S, Ref[RVIO[S, _], A]] = IO.ref(a)

    def cede: RVIO[S, Unit] = IO.cede

    def start[A](fa: RVIO[S, A]): RVIO[S, Fiber[RVIO[S, _], Throwable, A]] =
      for
        rng <- dispatch
        f <- (state.set(State(rng)) *> fa).start
      yield f

    def sleep(time: FiniteDuration): RVIO[S, Unit] = IO.sleep(time)

    def canceled: RVIO[S, Unit] = IO.canceled

    def forceR[A, B](fa: RVIO[S, A])(fb: RVIO[S, B]): RVIO[S, B] = fa.forceR(fb)

    def onCancel[A](fa: RVIO[S, A], fin: RVIO[S, Unit]): RVIO[S, A] = fa.onCancel(fin)

    def uncancelable[A](body: Poll[RVIO[S, _]] => RVIO[S, A]): RVIO[S, A] =
      IO.uncancelable(body)

    extension [A](fa: RVIO[S, A])
      def simulate(seed: S): IO[A] = IO(State(seed.copy())).flatMap(state.set) *> fa

    def int: RVIO[S, Int] = state.get.flatMap(s => IO(s.rng.nextInt()))

    def long: RVIO[S, Long] = state.get.flatMap(s => IO(s.rng.nextLong()))

    def dispatch: RVIO[S, S] =
      for
        s <- state.get
        rng <- IO(s.rng.copy())
      yield rng

    def get: RVIO[S, Double] = state.get.map(_.cachedGaussian)

    def set(x: Double): RVIO[S, Unit] = state.get.flatMap(s => IO(s.cachedGaussian = x))

    def suspend[A](hint: Sync.Type)(thunk: => A): RVIO[S, A] = IO.suspend(hint)(thunk)

  given [S, A](using show: Show[IO[A]]): Show[RVIO[S, A]] with
    def show(rv: RVIO[S, A]): String = s"RV${show.show(rv)}"

  given [S, A](using m: Monoid[IO[A]]): Monoid[RVIO[S, A]] = m
  given [S, A](
      using s: Semigroup[IO[A]],
      not: NotGiven[Monoid[RVIO[S, A]]]): Semigroup[RVIO[S, A]] = s
  given [S, A]: SemigroupK[RVIO[S, _]] = IO.semigroupKForIO
  given [S, A]: Align[RVIO[S, _]] = IO.alignForIO
  given [S, A]: Parallel[RVIO[S, _]] = spawn.parallelForGenSpawn
  given [S, A]: Console[RVIO[S, _]] = IO.consoleForIO
