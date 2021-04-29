# Schrodinger

[![Latest version](https://index.scala-lang.org/armanbilge/schrodinger/schrodinger/latest.svg?color=orange)](https://index.scala-lang.org/armanbilge/schrodinger/schrodinger)

Schrodinger is an (early-stage) project for probabilistic programming built on the [Cats](https://github.com/typelevel/cats) ecosystem.
At its heart is the `DistT` monad which enables you to safely inject randomness into your program by describing it as a probability distribution while also interleaving with other effects, such as [Cats Effect](https://github.com/typelevel/cats-effect)'s `IO` monad.
The goal is that this construct will facilitate the implementation of high-performance, concurrent Monte Carlo algorithms for simulation and inference.
Notably, via the use of "splittable" RNGs (versus a synchronized global or thread-locals), every random number generated can be deterministically traced back to the initial seed irrespective of execution order or thread assignment at runtime.
This substantially improves the reproducibility of concurrent programs.

## Usage

```scala
libraryDependencies += "com.armanbilge" %% "schrodinger" % "0.1.0"
// (optional) provides additional instances for cats-effect typeclasses
libraryDependencies += "com.armanbilge" %% "schrodinger-effect" % "0.1.0"
```

## Example: Schrödinger's cat

In the [example project](https://github.com/armanbilge/schrodinger/blob/main/example/src/main/scala/schrodinger/ThoughtExperiment.scala) we reimagine the famous [thought experiment](https://en.wikipedia.org/wiki/Schr%C3%B6dinger%27s_cat) with the help of Cats Effect.
The experiment is modeled with three concurrent components:

* an atom that decays after a random exponentially-distributed time and is sensed by the Geiger counter
* a relay that is triggered by the Geiger counter to release poison and kill the cat :(
* an observer who starts the experiment, waits 1 second, and then checks on the cat

Although the only randomness in the program is the atom's decay, we build the entire program as a `DistT[IO, *]` and defer sampling from this distribution until "right before the end of the world" at which point we provide the initial seed for the RNG.

```scala
sealed trait Cat
case object LiveCat extends Cat
case object DeadCat extends Cat

val decayRate = math.log(2)

def decayingAtom[F[_]: Async](geigerCounter: CountDownLatch[DistT[F, *]]): DistT[F, Unit] = for {
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
```

In spite of its concurrency, this program is virtually guaranteed to produce:

```
Experiment 1: observing a LiveCat
Experiment 2: observing a DeadCat
No cats were harmed in the thinking of this experiment :)
```
