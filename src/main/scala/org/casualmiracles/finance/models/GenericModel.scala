package org.casualmiracles.finance.models

import org.casualmiracles.finance.contracts._
import Contracts._
import Stream._

case class Model(
    modelStart: Date,
    disc: Currency ⇒ PR[Boolean] ⇒ PR[Double] ⇒ PR[Double],
    exch: Currency ⇒ Currency ⇒ PR[Double],
    absorb: Currency ⇒ PR[Boolean] ⇒ PR[Double] ⇒ PR[Double],
    rateModel: Currency ⇒ PR[Double])
 
abstract class GenericModel extends InterestRateModel {
  // Compositional valuation semantics for contracts
  def evalC(model: Model, k: Currency): Contract ⇒ PR[Double] = {
    def eval(contract: Contract): PR[Double] = contract match {
      case Zero            ⇒ bigK(0)
      case One(k2)         ⇒ model.exch(k)(k2)
      case Give(c)         ⇒ bigK(-1.0) %* eval(c)
      case Scale(o, c)     ⇒ evalO(o) %* eval(c)
      case And(c1, c2)     ⇒ eval(c1) %+ eval(c2)
      case Or(c1, c2)      ⇒ max(eval(c1), eval(c2))
      case Cond(o, c1, c2) ⇒ condPr(evalO(o), eval(c1), eval(c2))
      case When(o, c)      ⇒ disc(k, evalO(o), eval(c))
      //      eval (Anytime o c)  = snell  k (evalO o, eval c)
      case Until(o, c)     ⇒ absorb(k, evalO(o), eval(c))
      case _               ⇒ sys.error("todo")
    }
    eval
  }

  def takePr[T](n: Int, pr: PR[T]) = PR(pr.unPr.take(n))
  def horizonPr(pr: PR[_]) = pr.unPr.length
  def andPr(pr: PR[Boolean]):Boolean = pr.unPr.forall(rvb => rvb.forall(identity))


  def makeModel(modelDate: Date) = Model(
    modelStart = modelDate,
    disc = (disc _).curried,
    exch = (exch _).curried,
    absorb = (absorb _).curried,
    rateModel = rateModel _)

  // Disc primitive
  def disc(k: Currency, bs: PR[Boolean], rs: PR[Double]): PR[Double] = PR(discCalc(bs.unPr, rs.unPr, rateModel(k).unPr))

  def discCalc(b: Stream[RV[Boolean]], p: Stream[RV[Double]], rate: Stream[RV[Double]]): Stream[RV[Double]] = {
    val (bRv #:: bs) = b
    val (pRv #:: ps) = p
    val (rateRv #:: rs) = rate
    if (bRv.forall(bv ⇒ bv)) Stream(pRv)
    else {
      val rest = discCalc(bs, ps, rs)
      val (nextSlice #:: _) = rest
      val discSlice = zipWith(prevSlice(nextSlice), rateRv)((x, r) ⇒ x / (1 + r / 100))
      val thisSlice = zipWith3(bRv, pRv, discSlice)((b, p, q) ⇒ if (b) p else q)
      thisSlice #:: rest
    }
  }

  def prevSlice(s: RV[Double]): RV[Double] = s match {
    case Empty                ⇒ Empty
    case (_ #:: Empty)        ⇒ Empty
    case (n1 #:: n2 #:: rest) ⇒ ((n1 + n2) / 2.0) #:: prevSlice(n2 #:: rest)
  }

  def absorb(k: Currency, prb: PR[Boolean], prRvs: PR[Double]): PR[Double] = {
    val bSlices = prb.unPr
    val rvs = prRvs.unPr
    PR(zipWith(bSlices, rvs)((bRv, rvsRv) ⇒ zipWith(bRv, rvsRv)((o, p) ⇒ if (o) 0 else p)))
  }

  def exch(k1: Currency, k2: Currency): PR[Double] = PR(konstSlices(1))

  def expectedValue(outcomes: RV[Double], probabilities: RV[Double]): Double = zipWith(outcomes, probabilities)(_ * _).sum

  def probabilityLattice: Stream[RV[Double]] = probabilities(pathCounts)

  def probabilities(s: Stream[RV[Int]]): Stream[RV[Double]] = {
    val (sl #:: sls) = s
    val sum = sl.sum.toDouble
    sl.map(_ / sum) #:: probabilities(sls)
  }

  def pathCounts: Stream[RV[Int]] = {
    def zero = Stream(0)
    def paths(sl: Stream[Int]): Stream[RV[Int]] = sl #:: paths(zipWith(sl ++ zero, 0 #:: sl)(_ + _))
    paths(Stream(1))
  }

  def evalO[T](o: Observable[T]): PR[T] = o.f(time0)
}
