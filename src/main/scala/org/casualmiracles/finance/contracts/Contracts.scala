package org.casualmiracles.finance.contracts

object Contracts {

  trait Currency
  case object USD extends Currency
  case object GBP extends Currency
  case object EUR extends Currency
  case object ZAR extends Currency
  case object KYD extends Currency
  case object CHF extends Currency

  type TimeStep = Int
  type CalendarTime = Unit
  case class Date(c: CalendarTime, t: TimeStep) extends Ordered[Date] {
    def compare(that: Date) = t.compare(that.t)
  }

  trait Contract

  type RV[A] = Stream[A]

  case class PR[A](unPr: Stream[RV[A]])

  case class Observable[T](f: Date ⇒ PR[T]) {
    override def toString = "Observable " + f(time0) + ")"
  }

  def konst[T](k: T) = Observable((d: Date) ⇒ bigK(k))
  def date = Observable((t: Date) ⇒ PR(timeSlices(Stream(t))))
  
  def bigK[T](x: T) = PR(konstSlices(x))
  def konstSlices[T](x: T): Stream[Stream[T]] = {
    def nextSlice(sl: Stream[T]): Stream[Stream[T]] = sl #:: nextSlice(x #:: sl)
    nextSlice(Stream(x))
  }
  
  def mkDate(t: TimeStep): Date = Date((), t)
  def time0 = mkDate(0)
  //  timeSlices sl@((s,t):_) = sl : timeSlices [(s,t+1) | _ <- [0..t+1]]
  def timeSlices(sl: RV[Date]): Stream[RV[Date]] = {
    val (Date(s, t) #:: _) = sl
    val nextSlice = Stream.fill(t + 2)(Date(s, t + 1))
    sl #:: timeSlices(nextSlice)
  }

  implicit def withEnrichment(c: Contract) = new {
    def and(c2: Contract) = And(c, c2)
    def andGive(c2: Contract) = And(c, Give(c2))
    def or(c2: Contract) = Or(c, c2)
  }

  implicit def toKonstD(x: Double) = konst(x)
  implicit def toKonstI(x: Int) = konst(x.toDouble)

  case object Zero extends Contract
  case class One(currency: Currency) extends Contract
  case class Scale(obs: Observable[Double], contract: Contract) extends Contract
  case class When(obs: Observable[Boolean], c: Contract) extends Contract
  case class Anytime(obs: Observable[Boolean], c: Contract) extends Contract
  case class Until(obs: Observable[Boolean], c: Contract) extends Contract
  case class Cond(obs: Observable[Boolean], c1: Contract, c2: Contract) extends Contract
  case class Or(c1: Contract, c2: Contract) extends Contract
  case class And(c1: Contract, c2: Contract) extends Contract
  case class Give(contract: Contract) extends Contract

  def one = One.apply _
  def when = (When.apply _).curried
  def anytime = (Anytime.apply _).curried
  def until = (Until.apply _).curried
  def scale = (Scale.apply _).curried
  def cond = (Cond.apply _).curried
  def at(d: Date): Observable[Boolean] = lift2((a: Date, b: Date) ⇒ a == b, date, konst(d))

  def zipWith[A, B, C](sA: Stream[A], sB: Stream[B])(f: (A, B) ⇒ C): Stream[C] = sA.zip(sB).map(x ⇒ f(x._1, x._2))

  def zipWith3[A, B, C, D](sA: Stream[A], sB: Stream[B], sC: Stream[C])(f: (A, B, C) ⇒ D): Stream[D] = sA.zip(sB.zip(sC)).map(x ⇒ f(x._1, x._2._1, x._2._2))

  //lift f (Obs o) = Obs (\t -> PR $ map (map f) (unPr $ o t))
  def lift[A, B](f: A ⇒ B, obs: Observable[A]) = Observable((t: Date) ⇒ PR(obs.f(t).unPr.map(_.map(f(_)))))

  //lift2 f (Obs o1) (Obs o2) = Obs (\t -> PR $ zipWith (zipWith f) (unPr $ o1 t) (unPr $ o2 t))
  def lift2[A, B, C](f: (A, B) ⇒ C, obsA: Observable[A], obsB: Observable[B]): Observable[C] = {
    val rvF = (rvA: RV[A], rvB: RV[B]) ⇒ zipWith(rvA, rvB)((a: A, b: B) ⇒ f(a, b))
    Observable((t: Date) ⇒ PR(zipWith(obsA.f(t).unPr, obsB.f(t).unPr)(rvF)))
  }

  implicit def ObservableOps[T <% Double](obs: Observable[T]) = new {
    def *(a: Observable[T]) = lift2((_: T) * (_: T), obs, a)
    def /(a: Observable[T]) = lift2((_: T) / (_: T), obs, a)
    def +(a: Observable[T]) = lift2((_: T) + (_: T), obs, a)
    def -(a: Observable[T]) = lift2((_: T) - (_: T), obs, a)
  }

  implicit def ObservableRelations[T <% Ordered[T]](obs: Observable[T]) = new {
    def %<(a: Observable[T]) = lift2((_: T) < (_: T), obs, a)
    def %<=(a: Observable[T]) = lift2((_: T) <= (_: T), obs, a)
    def %>(a: Observable[T]) = lift2((_: T) > (_: T), obs, a)
    def %>=(a: Observable[T]) = lift2((_: T) >= (_: T), obs, a)
  }

  def between(d1: Date, d2: Date) = lift2((_: Boolean) && (_: Boolean), date %<= konst(d1), date %>= konst(d2))

  // example contracts

  def zcb(d: Date, n: Double, c: Currency) = when(at(d))(scale(n)(One(c)))
  def european(d: Date, c: Contract) = when(at(d))(c or Zero)
  def american(d1: Date, d2: Date, c: Contract) = anytime(between(d1, d2))(c)

  // Model

  def takePr[T](n: Int, pr: PR[T]) = PR(pr.unPr.take(n))
  def horizonPr(pr: PR[_]) = pr.unPr.length
  def andPr(pr: PR[Boolean]) = pr.unPr.forall(_)

  case class Model(
    modelStart: Date,
    disc: Currency ⇒ PR[Boolean] ⇒ PR[Double] ⇒ PR[Double],
    exch: Currency ⇒ Currency ⇒ PR[Double],
    absorb: Currency ⇒ PR[Boolean] ⇒ PR[Double] ⇒ PR[Double],
    rateModel: Currency ⇒ PR[Double])

  def exampleModel(modelDate: Date) = Model(
    modelStart = Date(modelDate, 0),
    disc = (disc _).curried,
    exch = (exch _).curried,
    absorb = (absorb _).curried,
    rateModel = rateModel _)

  // Interest Rate Model
  def rates(rateNow: Double, delta: Double): PR[Double] = {
    def makeRateSlices(rateNow: Double, n: Int): Stream[RV[Double]] = rateSlice(rateNow, n) #:: makeRateSlices(rateNow - delta, n + 1)
    def rateSlice(minRate: Double, n: Int) = comb(minRate).take(n)
    def comb(x: Double): Stream[Double] = x #:: comb(x + delta)
    PR(makeRateSlices(rateNow, 1))
  }

  val rateModels: Map[Currency, PR[Double]] = Map(
    CHF -> rates(7, 0.8),
    EUR -> rates(6.5, 0.25),
    GBP -> rates(8, 0.5),
    KYD -> rates(11, 1.2),
    USD -> rates(5, 1),
    ZAR -> rates(15, 1.5))

  def rateModel(k: Currency) = rateModels.get(k).getOrElse(sys.error("rateModel: currency not found " + k))

  // Disc primitive
  /*
   disc :: Currency -> (PR Bool, PR Double) -> PR Double
   disc k (PR bs, PR rs) = PR $ discCalc bs rs (unPr $ rateModel k)
   */

  def disc(k: Currency, bs: PR[Boolean], rs: PR[Double]): PR[Double] = PR(discCalc(bs.unPr, rs.unPr, rateModel(k).unPr))

  /*
   discCalc :: [RV Bool] -> [RV Double] -> [RV Double] -> [RV Double]
   discCalc (bRv:bs) (pRv:ps) (rateRv:rs)
   if and bRv -- test for horizon
           then [pRv]
           else let rest@(nextSlice:_) = discCalc bs ps rs
                    discSlice = zipWith (\x r -> x / (1 + r/100)) (prevSlice nextSlice) rateRv
                    thisSlice = zipWith3 (\b p q -> if b then p else q) -- allow for partially discounted slices
                                  bRv pRv discSlice
                in thisSlice : rest

   */
  def discCalc(b: Stream[RV[Boolean]], p: Stream[RV[Double]], rate: Stream[RV[Double]]): Stream[RV[Double]] = {
    val (bRv #:: bs) = b
    val (pRv #:: ps) = p
    val (rateRv #:: rs) = rate
    if (bRv.isEmpty || bRv.forall(x ⇒ x)) Stream(pRv)
    else {
      val rest = discCalc(bs, ps, rs)
      val (nextSlice #:: _) = rest
      val discSlice = zipWith(prevSlice(nextSlice), rateRv)((x, r) ⇒ x / (1 + r / 100))
      val thisSlice = zipWith3(bRv, pRv, discSlice)((b, p, q) ⇒ if (b) p else q)
      thisSlice #:: rest
    }
  }

  /*
   prevSlice [] = []
   prevSlice (_:[]) = []
   prevSlice (n1:rest@(n2:_)) = (n1+n2)/2 : prevSlice rest
   */
  def prevSlice(s: RV[Double]): RV[Double] = s match {
    case Stream.Empty         ⇒ Stream.Empty
    case (_ #:: Stream.Empty) ⇒ Stream.Empty
    case (n1 #:: n2 #:: rest) ⇒ ((n1 + n2) / 2.0) #:: prevSlice(n2 #:: rest)
  }

  // Absorb primitive
  //absorb :: Currency -> (PR Bool, PR Double) -> PR Double
  // absorb k (PR bSlices, PR rvs) =
  //     PR $ zipWith (zipWith $ \o p -> if o then 0 else p) bSlices rvs
  def absorb(k: Currency, prb: PR[Boolean], prRvs: PR[Double]): PR[Double] = {
    val bSlices = prb.unPr
    val rvs = prRvs.unPr
    PR(zipWith(bSlices, rvs)((bRv, rvsRv) ⇒ zipWith(bRv, rvsRv)((o, p) ⇒ if (o) 0 else p)))
  }

  // exchange rate model
  // exch :: Currency -> Currency -> PR Double
  // exch k1 k2 = PR (konstSlices 1)

  def exch(k1: Currency, k2: Currency): PR[Double] = PR(konstSlices(1))

  // expected value
  // expectedValue :: RV Double -> RV Double -> Double
  // expectedValue outcomes probabilities = sum $ zipWith (*) outcomes probabilities

  def expectedValue(outcomes: RV[Double], probabilities: RV[Double]): Double = (zipWith(outcomes, probabilities)(_ * _)).sum

  /* Probability Calculation
     probabilityLattice :: [RV Double]
     probabilityLattice = probabilities pathCounts
   	 where
       probabilities :: [RV Integer] -> [RV Double]
       probabilities (sl:sls) = map (\n -> (fromInteger n) / (fromInteger (sum sl))) sl : probabilities sls
   */
  def probabiltyLattice: Stream[RV[Double]] = probabilities(pathCounts)
  def probabilities(s: Stream[RV[Int]]): Stream[RV[Double]] = {
    val (sl #:: sls) = s
    val sum = sl.sum.toDouble
    sl.map(_ / sum) #:: probabilities(sls)
  }

  //pathCounts :: [RV Integer]
  //   pathCounts = paths [1] where paths sl = sl : (paths (zipWith (+) (sl++[0]) (0:sl)))
  def pathCounts: Stream[RV[Int]] = {
    def zero = Stream(0)
    def paths(sl: Stream[Int]): Stream[RV[Int]] = sl #:: (paths(zipWith(sl ++ zero, 0 #:: sl)(_ + _)))
    paths(Stream(1))
  }

  /*
    instance Num a => Num (PR a) where
   fromInteger i = bigK (fromInteger i)
   (+) = lift2PrAll (+)
   (-) = lift2PrAll (-)
   (*) = lift2PrAll (*)
   abs = liftPr  abs
   signum = liftPr signum
   */
  implicit def PrOps(prA: PR[Double]) = new {
    def +(prB: PR[Double]): PR[Double] = lift2PrAll((_: Double) + (_: Double), prA, prB)
    def -(prB: PR[Double]): PR[Double] = lift2PrAll((_: Double) - (_: Double), prA, prB)
    def *(prB: PR[Double]): PR[Double] = lift2PrAll((_: Double) * (_: Double), prA, prB)
    def abs: PR[Double] = liftPr((d: Double) ⇒ math.abs(d), prA)
    def signum: PR[Double] = liftPr((d: Double) ⇒ math.signum(d), prA)
  }

  def max[T <% Double](pra: PR[T], prb: PR[T]) = lift2Pr((a: T, b: T) ⇒ math.max(a, b), pra, prb)

  //implicit def ObservableOps[T <% Double](obs: Observable[T]) = new {
  // def *(a: Observable[T]) = lift2((_: T) * (_: T), obs, a)

  // Compositional valuation semantics for contracts
  /*
   evalC :: Model -> Currency -> Contract -> PR Double
   evalC (Model modelDate disc exch absorb rateModel) k = eval    -- punning on record fieldnames for conciseness
     where eval Zero           = bigK 0
         eval (One k2)       = exch k k2
         eval (Give c)       = -(eval c)
         eval (o `Scale` c)  = (evalO o) * (eval c)
         eval (c1 `And` c2)  = (eval c1) + (eval c2)
         eval (c1 `Or` c2)   = max (eval c1) (eval c2)
         eval (Cond o c1 c2) = condPr (evalO o) (eval c1) (eval c2)
         eval (When o c)     = disc   k (evalO o, eval c)
 --      eval (Anytime o c)  = snell  k (evalO o, eval c)
         eval (Until o c)    = absorb k (evalO o, eval c)
   */

  def evalC(model: Model, k: Currency): Contract ⇒ PR[Double] = {
    def eval(contract: Contract): PR[Double] = contract match {
      case Zero            ⇒ bigK(0)
      case One(k2)         ⇒ model.exch(k)(k2)
      case Give(c)         ⇒ bigK(-1.0) * eval(c)
      case Scale(o, c)     ⇒ evalO(o) * eval(c)
      case And(c1, c2)     ⇒ eval(c1) + eval(c2)
      case Or(c1, c2)      ⇒ max(eval(c1), eval(c2))
      case Cond(o, c1, c2) ⇒ condPr(evalO(o), eval(c1), eval(c2))
      case When(o, c)      ⇒ disc(k, evalO(o), eval(c))
      //      eval (Anytime o c)  = snell  k (evalO o, eval c)
      case Until(o, c)     ⇒ absorb(k, evalO(o), eval(c))
      case _               ⇒ sys.error("todo")
    }
    eval _
  }

  // evalO :: Obs a -> PR a
  // evalO (Obs o) = o time0
  def evalO[T](o: Observable[T]): PR[T] = o.f(time0)

  // condPr :: PR Bool -> PR a -> PR a -> PR a
  // condPr = lift3Pr (\b tru fal -> if b then tru else fal)
  def condPr[T](aPr: PR[Boolean], bPr: PR[T], cPr: PR[T]): PR[T] = lift3Pr((b: Boolean, tru: T, fal: T) ⇒ if (b) tru else fal, aPr, bPr, cPr)

  //liftPr :: (a -> b) -> PR a -> PR b
  //liftPr f (PR a) = PR $ map (map f) a
  def liftPr[A, B](f: A ⇒ B, pr: PR[A]): PR[B] = PR(pr.unPr.map(_.map(f(_))))

  //lift2Pr :: (a -> b -> c) -> PR a -> PR b -> PR c
  //lift2Pr f (PR a) (PR b) = PR $ zipWith (zipWith f) a b
  def lift2Pr[A, B, C](f: (A, B) ⇒ C, aPr: PR[A], bPr: PR[B]): PR[C] = {
    val rvF = (rvA: RV[A], rvB: RV[B]) ⇒ zipWith(rvA, rvB)((a: A, b: B) ⇒ f(a, b))
    PR(zipWith(aPr.unPr, bPr.unPr)(rvF))
  }

  // lift2PrAll :: (a -> a -> a) -> PR a -> PR a -> PR a
  // lift2PrAll f (PR a) (PR b) = PR $ zipWithAll (zipWith f) a b
  def lift2PrAll[A](f: (A, A) ⇒ A, aPr: PR[A], bPr: PR[A]): PR[A] = {
    val rvF = (rvA: RV[A], rvB: RV[A]) ⇒ zipWith(rvA, rvB)((a: A, b: A) ⇒ f(a, b))
    PR(zipWithAll(rvF, aPr.unPr, bPr.unPr))
  }

  // lift3Pr :: (a -> b -> c -> d) -> PR a -> PR b -> PR c -> PR d
  // lift3Pr f (PR a) (PR b) (PR c) = PR $ zipWith3 (zipWith3 f) a b c
  def lift3Pr[A, B, C, D](f: (A, B, C) ⇒ D, aPr: PR[A], bPr: PR[B], cPr: PR[C]): PR[D] = {
    val rvF = (rvA: RV[A], rvB: RV[B], rvC: RV[C]) ⇒ zipWith3(rvA, rvB, rvC)((a: A, b: B, c: C) ⇒ f(a, b, c))
    PR(zipWith3(aPr.unPr, bPr.unPr, cPr.unPr)(rvF))
  }

  /*
   	zipWithAll :: (a -> a -> a) -> [a] -> [a] -> [a]
	zipWithAll f (a:as) (b:bs)     = f a b : zipWithAll f as bs
 	zipWithAll f as@(_:_) []       = as
 	zipWithAll f []       bs@(_:_) = bs
	zipWithAll _ _        _        = []
   */
  def zipWithAll[A](f: (A, A) ⇒ A, sa: Stream[A], sb: Stream[A]): Stream[A] = (sa, sb) match {
    case (a #:: as, b #:: bs) ⇒ f(a, b) #:: zipWithAll(f, as, bs)
    case (as, Stream.Empty)   ⇒ as
    case (Stream.Empty, bs)   ⇒ bs
    case (_, _)               ⇒ Stream.Empty
  }
  
  def printPr(pr: PR[_], n: Int) = pr.unPr.take(n).foreach(s => {s.foreach(s => print(s + " "));println("")})
}