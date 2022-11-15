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
import cats.effect.kernel.Outcome
import cats.effect.kernel.Par.ParallelF
import cats.effect.kernel.Poll
import cats.effect.kernel.Ref
import cats.effect.kernel.Spawn
import cats.effect.kernel.Sync
import cats.effect.std.Console
import cats.kernel.Monoid
import cats.kernel.Semigroup
import cats.syntax.all.*
import cats.~>
import schrodinger.kernel.PseudoRandom
import schrodinger.kernel.GaussianCache
import schrodinger.unsafe.rng.SplittableRng

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.FiniteDuration
import scala.util.NotGiven

opaque type RVIO[S, +A] = IO[A]

object RVIO:
  extension [S, A](rvioa: RVIO[S, A])
    inline def evalMap[B](f: A => IO[B]): RVIO[S, B] =
      rvioa.flatMap(a => eval(f(a)))

  type Par[S, A] = ParallelF[RVIO[S, _], A]

  inline def eval[S, A](ioa: IO[A]): RVIO[S, A] = ioa

  def evalK[S]: IO ~> RVIO[S, _] =
    new:
      def apply[A](ioa: IO[A]): RVIO[S, A] = ioa

  def algebra[S: SplittableRng]: IO[Algebra[S]] = IOLocal[State[S]](null).map(Algebra(_))

  final private class State[S](
      val rng: S = null.asInstanceOf[S],
      var cachedGaussian: Double = Double.NaN,
  )

  final class Algebra[S0: SplittableRng] private[RVIO] (state: IOLocal[State[S0]])
      extends Async[RVIO[S0, _]],
        PseudoRandom[RVIO[S0, _]],
        RngDispatcher[RVIO[S0, _]],
        GaussianCache[RVIO[S0, _], Double]:

    type G[A] = IO[A]
    type S = S0

    def rng = summon[SplittableRng[S]]

    def pure[A](x: A): RVIO[S, A] = IO.pure(x)

    def handleErrorWith[A](fa: RVIO[S, A])(f: Throwable => RVIO[S, A]): RVIO[S, A] =
      fa.handleErrorWith(f)

    def raiseError[A](e: Throwable): RVIO[S, A] = IO.raiseError(e)

    override def attempt[A](fa: RVIO[S, A]): RVIO[S, Either[Throwable, A]] = fa.attempt

    def cont[K, R](body: Cont[RVIO[S, _], K, R]): RVIO[S0, R] = IO.cont(body)

    def evalOn[A](fa: RVIO[S, A], ec: ExecutionContext): RVIO[S, A] = fa.evalOn(ec)

    def executionContext: RVIO[S, ExecutionContext] = IO.executionContext

    def monotonic: RVIO[S, FiniteDuration] = IO.monotonic

    def realTime: RVIO[S, FiniteDuration] = IO.realTime

    override def map[A, B](fa: RVIO[S, A])(f: A => B): RVIO[S, B] = fa.map(f)

    def flatMap[A, B](fa: RVIO[S, A])(f: A => RVIO[S0, B]): RVIO[S, B] =
      fa.flatMap(f)

    def tailRecM[A, B](a: A)(f: A => RVIO[S, Either[A, B]]): RVIO[S, B] =
      IO.asyncForIO.tailRecM(a)(f)

    def deferred[A]: RVIO[S, Deferred[RVIO[S, _], A]] = IO.deferred

    def ref[A](a: A): RVIO[S, Ref[RVIO[S, _], A]] = IO.ref(a)

    def cede: RVIO[S, Unit] = IO.cede

    def start[A](fa: RVIO[S, A]): RVIO[S, Fiber[RVIO[S, _], Throwable, A]] =
      dispatch.flatMap(rng => (state.set(State(rng)) *> fa).start)

    override def racePair[A, B](fa: RVIO[S, A], fb: RVIO[S, B]): RVIO[S, Either[
      (Outcome[RVIO[S, _], Throwable, A], Fiber[RVIO[S, _], Throwable, B]),
      (Fiber[RVIO[S, _], Throwable, A], Outcome[RVIO[S, _], Throwable, B]),
    ]] =
      (dispatch, dispatch).flatMapN { (rng1, rng2) =>
        IO.racePair(
          state.set(State(rng1)) *> fa,
          state.set(State(rng2)) *> fb,
        )
      }

    def sleep(time: FiniteDuration): RVIO[S, Unit] = IO.sleep(time)

    def canceled: RVIO[S, Unit] = IO.canceled

    def forceR[A, B](fa: RVIO[S, A])(fb: RVIO[S, B]): RVIO[S, B] = fa.forceR(fb)

    def onCancel[A](fa: RVIO[S, A], fin: RVIO[S, Unit]): RVIO[S, A] = fa.onCancel(fin)

    def uncancelable[A](body: Poll[RVIO[S, _]] => RVIO[S, A]): RVIO[S, A] =
      IO.uncancelable(body)

    override def bracketFull[A, B](acquire: Poll[RVIO[S, _]] => RVIO[S, A])(
        use: A => RVIO[S, B],
    )(release: (A, Outcome[RVIO[S, _], Throwable, B]) => RVIO[S, Unit]): RVIO[S, B] =
      IO.bracketFull(acquire)(use)(release)

    override def guarantee[A](fa: RVIO[S, A])(finalizer: RVIO[S, Unit]): RVIO[S, A] =
      fa.guarantee(finalizer)

    override def guaranteeCase[A](fa: RVIO[S, A])(
        finalizer: Outcome[RVIO[S, _], Throwable, A] => RVIO[S, Unit],
    ): RVIO[S, A] =
      fa.guaranteeCase(finalizer)

    extension [A](fa: RVIO[S, A])
      def simulate(seed: S): IO[A] = IO(State(seed.copy())).flatMap(state.set) *> fa

    def int: RVIO[S, Int] = state.get.flatMap(s => IO(s.rng.nextInt()))

    def long: RVIO[S, Long] = state.get.flatMap(s => IO(s.rng.nextLong()))

    def dispatch: RVIO[S, S] =
      for
        s <- state.get
        rng <- IO(s.rng.split())
      yield rng

    def getAndClear: RVIO[S, Double] =
      state.get.flatMap { s =>
        IO.pure(s.cachedGaussian) <* IO(s.cachedGaussian = Double.NaN)
      }

    def set(x: Double): RVIO[S, Unit] = state.get.flatMap(s => IO(s.cachedGaussian = x))

    def suspend[A](hint: Sync.Type)(thunk: => A): RVIO[S, A] = IO.suspend(hint)(thunk)

  given [S, A](using show: Show[IO[A]]): Show[RVIO[S, A]] with
    def show(rv: RVIO[S, A]): String = s"RV${show.show(rv)}"

  given [S, A](using m: Monoid[IO[A]]): Monoid[RVIO[S, A]] = m
  given [S, A](using
      s: Semigroup[IO[A]],
      not: NotGiven[Monoid[RVIO[S, A]]],
  ): Semigroup[RVIO[S, A]] = s
  given [S]: SemigroupK[RVIO[S, _]] = IO.semigroupKForIO
  given [S]: Align[RVIO[S, _]] = IO.alignForIO
  given [S](using Spawn[RVIO[S, _]]): Parallel[RVIO[S, _]] = spawn.parallelForGenSpawn
  given [S]: Console[RVIO[S, _]] = IO.consoleForIO
