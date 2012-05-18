package org.casualmiracles.finance.contracts

object Observable extends Zip {
  
  import scalaz._
  import Scalaz._
  
  implicit def ObservableFunctor: Functor[Observable] = new Functor[Observable] {
    def fmap[A, B](r: Observable[A], f: A => B) = Observable((t: Date) ⇒ PR(r.f(t).unPr.map(_.map(f(_)))))
  } 
  
  def lift2[A, B, C](f: (A, B) ⇒ C, obsA: Observable[A], obsB: Observable[B]): Observable[C] = {
    val rvF = (rvA: RV[A], rvB: RV[B]) ⇒ zipWith(rvA, rvB)(f(_, _))
    Observable((t: Date) ⇒ PR(zipWith(obsA.f(t).unPr, obsB.f(t).unPr)(rvF)))
  }

  implicit def ObservableOps[T <% Double](obs: Observable[T]) = new {
    def %*(a: Observable[T]) = lift2((_: T) * (_: T), obs, a)
    def %/(a: Observable[T]) = lift2((_: T) / (_: T), obs, a)
    def %+(a: Observable[T]) = lift2((_: T) + (_: T), obs, a)
    def %-(a: Observable[T]) = lift2((_: T) - (_: T), obs, a)
    def %%(a: Observable[T]) = lift2((_: T) % (_: T), obs, a)
  }

  implicit def ObservableRelations[T <% Ordered[T]](obs: Observable[T]) = new {
    def %<(a: Observable[T]) = lift2((_: T) < (_: T), obs, a)
    def %<=(a: Observable[T]) = lift2((_: T) <= (_: T), obs, a)
    def %>(a: Observable[T]) = lift2((_: T) > (_: T), obs, a)
    def %>=(a: Observable[T]) = lift2((_: T) >= (_: T), obs, a)
    def %==(a: Observable[T]) = lift2((_: T) == (_: T), obs, a)
  }

  implicit def ObservableBooleans(obs: Observable[Boolean]) = new {
    def %&&(a: Observable[Boolean]) = lift2((_: Boolean) && (_: Boolean), obs, a)
    def %||(a: Observable[Boolean]) = lift2((_: Boolean) || (_: Boolean), obs, a)
  }

  implicit def ObservableDateOps(obs: Observable[Date]) = new {
    def %-(a: Observable[Date]) = lift2((_: Date).t - (_: Date).t, obs, a)
    def %+(a: Observable[Date]) = lift2((_: Date).t + (_: Date).t, obs, a)
  }

}

case class Observable[T](f: Date ⇒ PR[T])