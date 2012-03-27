package org.casualmiracles.finance.contracts

import Contracts._

object ExampleModel {
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
    def comb(x: Double): Stream[Double] = x #:: comb(x + 2*delta)
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

  // evalO :: Obs a -> PR a
  // evalO (Obs o) = o time0
  def evalO[T](o: Observable[T]): PR[T] = o.f(time0)
}