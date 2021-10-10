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

import cats.Applicative
import cats.Eval
import cats.FlatMap
import cats.Monad
import cats.data.StateT
import org.typelevel.vault.InsertKey
import org.typelevel.vault.LookupKey
import org.typelevel.vault.Vault
import schrodinger.kernel.PseudoRandom

type PureRV[S, A] = PureRVT[Eval, S, A]

opaque type PureRVT[F[_], S, A] = StateT[F, (S, Vault), A]

object PureRVT:
  extension [F[_], S, A](rv: PureRVT[F, S, A])
    def simulate(seed: S)(using FlatMap[F]): F[A] = rv.runA((seed, Vault.empty))

  def getExtra[F[_]: Applicative, S, A](key: LookupKey[A]): PureRVT[F, S, Option[A]] =
    StateT.inspect(_._2.lookup(key))

  def setExtra[F[_]: Applicative, S, A](key: InsertKey[A])(a: A): PureRVT[F, S, Unit] =
    StateT.modify { (state, extras) => (state, extras.insert(key, a)) }

  given [F[_]: Monad, S]: Monad[PureRVT[F, S, _]] with
    def pure[A](a: A): PureRVT[F, S, A] = StateT.pure(a)
    def flatMap[A, B](fa: PureRVT[F, S, A])(f: A => PureRVT[F, S, B]): PureRVT[F, S, B] =
      fa.flatMap(f)
    def tailRecM[A, B](a: A)(f: A => PureRVT[F, S, Either[A, B]]): PureRVT[F, S, B] =
      Monad[StateT[F, (S, Vault), _]].tailRecM(a)(f)

  given [F[_]: Monad, S0](using rng: PureRng[S0]): PseudoRandom[PureRVT[F, S0, _]] with
    type G[A] = F[A]
    type S = S0

    def int: PureRVT[F, S, Int] =
      for
        (state0, extras) <- StateT.get[F, (S, Vault)]
        (state1, x) = rng.nextInt(state0)
        _ <- StateT.set((state1, extras))
      yield x

    def long: PureRVT[F, S, Long] =
      for
        (state0, extras) <- StateT.get[F, (S, Vault)]
        (state1, x) = rng.nextLong(state0)
        _ <- StateT.set((state1, extras))
      yield x

    extension [A](fa: PureRVT[F, S, A])
      def simulate(seed: S): G[A] =
        PureRVT.simulate(fa)(seed)
