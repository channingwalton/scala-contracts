package org.casualmiracles.finance.contracts

object Observable extends Zip {
  
  def lift2[A, B, C](f: (A, B) ⇒ C, obsA: Observable[A], obsB: Observable[B]): Observable[C] = {
    val rvF = (rvA: RV[A], rvB: RV[B]) ⇒ zipWith(rvA, rvB)(f(_, _))
    Observable((t: Date) ⇒ PR(zipWith(obsA.f(t).unPr, obsB.f(t).unPr)(rvF)))
  }

  implicit class ObservableOps[T <% Double](obs: Observable[T]) {
    def %*(a: Observable[T]) = lift2((_: T) * (_: T), obs, a)
    def %/(a: Observable[T]) = lift2((_: T) / (_: T), obs, a)
    def %+(a: Observable[T]) = lift2((_: T) + (_: T), obs, a)
    def %-(a: Observable[T]) = lift2((_: T) - (_: T), obs, a)
    def %%(a: Observable[T]) = lift2((_: T) % (_: T), obs, a)
  }

  implicit class ObservableRelations[T <% Ordered[T]](obs: Observable[T]) {
    def %<(a: Observable[T]) = lift2((_: T) < (_: T), obs, a)
    def %<=(a: Observable[T]) = lift2((_: T) <= (_: T), obs, a)
    def %>(a: Observable[T]) = lift2((_: T) > (_: T), obs, a)
    def %>=(a: Observable[T]) = lift2((_: T) >= (_: T), obs, a)
    def %==(a: Observable[T]) = lift2((_: T) == (_: T), obs, a)
  }

  implicit class ObservableBooleans(obs: Observable[Boolean]) {
    def %&&(a: Observable[Boolean]) = lift2((_: Boolean) && (_: Boolean), obs, a)
    def %||(a: Observable[Boolean]) = lift2((_: Boolean) || (_: Boolean), obs, a)
  }

  implicit class ObservableDateOps(obs: Observable[Date]) {
    def %-(a: Observable[Date]) = lift2((_: Date).t - (_: Date).t, obs, a)
    def %+(a: Observable[Date]) = lift2((_: Date).t + (_: Date).t, obs, a)
  }

}

case class Observable[T](f: Date ⇒ PR[T])