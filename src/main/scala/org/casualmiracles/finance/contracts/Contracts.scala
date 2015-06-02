package org.casualmiracles.finance.contracts

import Stream.Empty

object Contracts extends PRs with Zip {
  
  def one = One.apply _
  def when = (When.apply _).curried
  def anytime = (Anytime.apply _).curried
  def until = (Until.apply _).curried
  def scale = (Scale.apply _).curried
  def cond = (Cond.apply _).curried
  
  implicit def toConstant[T](x: T) = constant(x)

  def constant[T](k: T): Observable[T] = Observable((d: Date) ⇒ bigK(k))

  def date: Observable[Date] = Observable((t: Date) ⇒ PR(timeSlices(Stream(t))))

  def bigK[T](x: T): PR[T] = PR(konstSlices(x))

  def konstSlices[T](x: T): Stream[RV[T]] = {
    def nextSlice(sl: Stream[T]): Stream[RV[T]] = sl #:: nextSlice(x #:: sl)
    nextSlice(Stream(x))
  }

  // Why does this work this way?
  // Isn't PR supposed to consist of a set of random values, not lists of values containing possible duplicates?
  def timeSlices(sl: RV[Date]): Stream[RV[Date]] = {
    val (Date(s, t) #:: _) = sl
    val nextSlice = Stream.fill(t + 2)(Date(s, t + 1))
    sl #:: timeSlices(nextSlice)
  }

  def between(d1: Date, d2: Date): Observable[Boolean] = (date %>= d1) %&& (date %<= d2)

  def at(d: Date): Observable[Boolean] = date %== d

  def mkDate(t: TimeStep): Date = Date((), t)

  def time0: Date = mkDate(0)

}