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
import cats.CommutativeMonad
import cats.Eq
import cats.Foldable
import cats.Id
import cats.Monad
import cats.instances.*
import cats.kernel.Hash
import cats.kernel.Semigroup
import cats.kernel.instances.MapMonoid
import cats.syntax.all.*
import schrodinger.math.syntax.*

import scala.util.NotGiven
import algebra.ring.Semiring

/**
 * @note
 *   The implementation relies on universal equals and hashCode of `A` for performance. Results
 *   will be incorrect otherwise.
 */
final case class Dist[P, A](support: Map[A, P]):

  def map[B](f: A => B)(using P: AdditiveMonoid[P]): Dist[P, B] =
    Dist(support.groupMapReduce(kv => f(kv._1))(_._2)(P.plus(_, _)))

  def flatMap[B](f: A => Dist[P, B])(using P: Rig[P]): Dist[P, B] =
    Dist {
      MapMonoid[B, P](using P.additive).combineAll {
        support.view.map((a, p) => f(a).support.view.mapValues(P.times(p, _)).toMap)
      }
    }

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

  given [P](
      using density: Bernoulli[P, Boolean][Density[Id, P]]): Bernoulli[P, Boolean][Dist[P, *]] =
    params =>
      val f = density(params)
      Dist(Map(false -> f(false), true -> f(true)))

  given [P](using density: UniformRange[Density[Id, P]]): UniformRange[Dist[P, *]] =
    params =>
      val f = density(params)
      Dist(params.support.map(i => i -> f(i)).toMap)

  given [G[_]: Foldable, P](
      using
      density: Categorical[G[P], Int][Density[Id, P]]): Categorical[G[P], Int][Dist[P, *]] =
    case params @ Categorical.Params(support) =>
      val f = density(params)
      Dist((0 until support.size.toInt).map(i => i -> f(i)).toMap)

  given [A: Hash, P](using density: Categorical[Map[A, P], A][Density[Id, P]])
      : Categorical[Map[A, P], A][Dist[P, *]] =
    case params @ Categorical.Params(support) =>
      val f = density(params)
      Dist(support.map((a, _) => a -> f(a)))

  given [G[_]: Foldable, A: Hash, P](
      using Categorical[Map[A, P], A][Dist[P, *]]): Categorical[G[(A, P)], A][Dist[P, *]] =
    case Categorical.Params(support) =>
      Categorical(support.toIterable.toMap)
