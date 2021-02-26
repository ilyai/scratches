import java.util.concurrent.ExecutorService

import scala.concurrent.ExecutionContext.Implicits.global
//import scala.concurrent.Future
import scala.concurrent.duration.TimeUnit
import java.util.concurrent._
import language.implicitConversions

def sum(as: IndexedSeq[Int]): Int =
  if (as.size <= 1) as.headOption.getOrElse(0)
  else {
    val (l,r) = as.splitAt(as.length/2)
    sum(l) + sum(r)
  }

sum(Vector(1,2,3))

//trait Par[A] {
//
//}
//
//object Par {
//  def unit[A](a: => A): Par[A] = ???
//  def get[A](a: Par[A]): A = ???
//}
//
//def sumPar(as: IndexedSeq[Int]): Int =
//  if (as.size <= 1) as.headOption.getOrElse(0)
//  else {
//    val (l,r) = as.splitAt(as.length/2)
//    ???
//  }

object Par {
  type Par[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = _ => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit): A  = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean) = false
  }

  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get,bf.get))
    }

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(() => a(es).get)

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def asyncF[A,B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def map[A,B](fa: Par[A])(f: A => B): Par[B] =
    map2(fa, unit(()))((a,_) => f(a))

  def sortPar(l: Par[List[Int]]): Par[List[Int]] =
    map(l)(_.sorted) // map2(l, unit(()))((a,_) => a.sorted)

  def sequenceSimple[A](l: List[Par[A]]): Par[List[A]] =
    l.foldRight[Par[List[A]]](unit(List()))((h,t) => map2(h,t)(_ :: _))

  def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
    if (as.isEmpty) unit(Vector())
    else if (as.length == 1) map(as.head)(a => Vector(a))
    else {
      val (l,r) = as.splitAt(as.length/2)
      map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
    }
  }

  def sequence[A](l: List[Par[A]]): Par[List[A]] =
    map(sequenceBalanced(l.toIndexedSeq))(_.toList)

  def parFilter[A](l: List[A])(f: A => Boolean): Par[List[A]] = {
    val pars: List[Par[List[A]]] = l.map(asyncF((a:A) => if (f(a)) List(a) else List()))
    map(sequence(pars))(_.flatten)
  }

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
    p(e).get == p2(e).get

  def delay[A](fa: => Par[A]): Par[A] = es => fa(es)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es => if (run(es)(cond).get) t(es) else f(es)

  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    es => {
      val ind = run(es)(n).get // Full source files
      run(es)(choices(ind))
    }

  def choiceViaChoiceN[A](a: Par[Boolean])(ifTrue: Par[A], ifFalse: Par[A]): Par[A] =
    choiceN(map(a)(b => if (b) 0 else 1))(List(ifTrue, ifFalse))

  def choiceMap[K,V](key: Par[K])(choices: Map[K,Par[V]]): Par[V] =
    es => {
      val k = run(es)(key).get
      run(es)(choices(k))
    }

  def chooser[A,B](p: Par[A])(choices: A => Par[B]): Par[B] =
    es => {
      val k = run(es)(p).get
      run(es)(choices(k))
    }

  /* `chooser` is usually called `flatMap` or `bind`. */
  def flatMap[A,B](p: Par[A])(choices: A => Par[B]): Par[B] =
    es => {
      val k = run(es)(p).get
      run(es)(choices(k))
    }

  def choiceViaFlatMap[A](p: Par[Boolean])(f: Par[A], t: Par[A]): Par[A] =
    flatMap(p)(b => if (b) t else f)

  def choiceNViaFlatMap[A](p: Par[Int])(choices: List[Par[A]]): Par[A] =
    flatMap(p)(i => choices(i))

  // see nonblocking implementation in `Nonblocking.scala`
  def join[A](a: Par[Par[A]]): Par[A] =
    es => run(es)(run(es)(a).get())

  def joinViaFlatMap[A](a: Par[Par[A]]): Par[A] =
    flatMap(a)(x => x)

  def flatMapViaJoin[A,B](p: Par[A])(f: A => Par[B]): Par[B] =
    join(map(p)(f))
  /* Gives us infix syntax for `Par`. */
  implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

  class ParOps[A](p: Par[A]) {

  }
}

object Examples {
  import Par._

  def sum(ints: IndexedSeq[Int]): Int =
    if (ints.size <= 1)
      ints.headOption.getOrElse(0)
    else {
      val (l,r) = ints.splitAt(ints.length/2)
      sum(l) + sum(r)
    }
}

Examples.sum(Vector(1,2,3))