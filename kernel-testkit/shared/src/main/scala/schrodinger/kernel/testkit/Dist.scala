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

package schrodinger.kernel
package testkit

import algebra.ring.AdditiveMonoid
import algebra.ring.CommutativeRig
import algebra.ring.MultiplicativeMonoid
import algebra.ring.Rig
import algebra.ring.Semiring
import cats.CommutativeMonad
import cats.Eq
import cats.Foldable
import cats.Id
import cats.Monad
import cats.Reducible
import cats.kernel.Hash
import cats.kernel.Monoid
import cats.kernel.Semigroup
import cats.syntax.all.*
import schrodinger.math.syntax.*
import schrodinger.stats.Density

import scala.util.NotGiven

/** @note
  *   The implementation relies on universal equals and hashCode of `A` for performance. Results
  *   will be incorrect otherwise.
  */
final case class Dist[P, A](support: Map[A, P]):

  def map[B](f: A => B)(using P: AdditiveMonoid[P]): Dist[P, B] =
    Dist(support.groupMapReduce(kv => f(kv._1))(_._2)(P.plus(_, _)))

  def flatMap[B](f: A => Dist[P, B])(using P: Rig[P], eq: Eq[P]): Dist[P, B] =
    given Monoid[P] = P.additive
    Dist(
      support.view
        .map { (a, p) =>
          f(a).support.view.filterNot(_._2 === P.zero).mapValues(P.times(p, _)).toMap
        }
        .toList
        .combineAll,
    )

  def expect(f: A => P)(using P: Rig[P]): P =
    P.sum(support.map(f(_) * _))

  def mean(using ev: A <:< P, P: Rig[P]): P = expect(ev)

object Dist:
  def pure[P, A](a: A)(using P: MultiplicativeMonoid[P]): Dist[P, A] =
    Dist(Map(a -> P.one))

  given [P: Rig: Eq](using NotGiven[CommutativeRig[P]]): Monad[Dist[P, *]] = DistMonad[P]

  given [P: CommutativeRig: Eq]: CommutativeMonad[Dist[P, *]] =
    new DistMonad[P] with CommutativeMonad[Dist[P, *]]

  given [P: Eq, A: Eq](using P: Semiring[P]): Eq[Dist[P, A]] =
    Eq.by(_.support.filterNot { case (_, p) => P.isZero(p) })

  private class DistMonad[P: Eq](using P: Rig[P]) extends Monad[Dist[P, *]]:
    def pure[A](a: A): Dist[P, A] = Dist.pure(a)(using P)

    override def map[A, B](da: Dist[P, A])(f: A => B): Dist[P, B] = da.map(f)

    def flatMap[A, B](da: Dist[P, A])(f: A => Dist[P, B]): Dist[P, B] = da.flatMap(f)

    def tailRecM[A, B](a: A)(f: A => Dist[P, Either[A, B]]): Dist[P, B] =
      given Semigroup[P] = P.additive

      def go(da: Dist[P, Either[A, B]], db: Map[B, P]): Dist[P, B] =
        val as = da.support.collect { case (Left(a), p) => a -> p }
        val bs = da.support.collect { case (Right(b), p) => b -> p }
        val dbp = db |+| bs
        if P.isZero(P.sum(da.support.view.filterKeys(_.isLeft).values)) then Dist(dbp)
        else go(Dist(as).flatMap(f), dbp)

      go(f(a), Map.empty)

  given [P](using FairBernoulli[Density[Id, P, _], Boolean]): FairBernoulli[Dist[P, _], Boolean]
  with
    def fairBernoulli =
      val f = Bernoulli.fair
      Dist(List(false, true).fproduct(f).toMap)

  given [P](using Bernoulli[Density[Id, P, _], P, Boolean]): Bernoulli[Dist[P, _], P, Boolean]
  with
    def bernoulli(successProbability: P) =
      val f = Bernoulli(successProbability)
      Dist(List(false, true).fproduct(f).toMap)

  given [P](using
      DiscreteUniform[Density[Id, P, _], Long],
  ): DiscreteUniform[Dist[P, _], Long] with
    def discreteUniform(n: Long) =
      val f = DiscreteUniform(n)
      Dist((0L until n).toList.fproduct(f).toMap)

  given [G[_]: Reducible, P](using
      Categorical[Density[Id, P, _], G[P], Long],
  ): Categorical[Dist[P, _], G[P], Long] with
    def categorical(support: G[P]) =
      val f = Categorical(support)
      Dist((0L until support.size).toList.fproduct(f).toMap)
