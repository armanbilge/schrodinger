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
import cats.kernel.Semigroup
import cats.kernel.instances.MapMonoid
import cats.syntax.all.*
import schrodinger.math.syntax.*

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

  def monad[P: Rig](n: Int): Monad[Dist[P, *]] = DistMonad(n)

  def commutativeMonad[P: CommutativeRig](n: Int): CommutativeMonad[Dist[P, *]] =
    new DistMonad(n) with CommutativeMonad[Dist[P, *]]

  given [P: Eq, A: Eq]: Eq[Dist[P, A]] = Eq.by(_.support)

  private class DistMonad[P](n: Int)(using P: Rig[P]) extends Monad[Dist[P, *]]:
    private given Semigroup[P] = P.additive

    def pure[A](a: A): Dist[P, A] = Dist.pure(a)(using P)

    override def map[A, B](da: Dist[P, A])(f: A => B): Dist[P, B] = da.map(f)

    def flatMap[A, B](da: Dist[P, A])(f: A => Dist[P, B]): Dist[P, B] = da.flatMap(f)

    def tailRecM[A, B](a: A)(f: A => Dist[P, Either[A, B]]): Dist[P, B] =
      var fa = f(a)
      var db = Map.empty[B, P]
      var i = 0
      while i < n do
        val as = fa.support.collect { case (Left(a), p) => a -> p }
        val bs = fa.support.collect { case (Right(b), p) => b -> p }
        db = db |+| bs
        if db.nonEmpty then i += 1 // don't start counter until we've hit some b
        if as.nonEmpty then fa = Dist(as).flatMap(f)
        else i = n
      Dist(db)

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
