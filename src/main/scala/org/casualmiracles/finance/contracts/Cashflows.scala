package org.casualmiracles.finance.contracts

import Contracts._
import ExampleModel._

object Cashflows {

  def cashflow(model: Model, k: Currency): Contract ⇒ PR[Double] = {
    def eval(contract: Contract): PR[Double] = contract match {
      case Zero            ⇒ bigK(0)
      case One(k2)         ⇒ model.exch(k)(k2)
      case Give(c)         ⇒ bigK(-1.0) * eval(c)
      case Scale(o, c)     ⇒ evalO(o) * eval(c)
      case And(c1, c2)     ⇒ eval(c1) + eval(c2)
      case Or(c1, c2)      ⇒ max(eval(c1), eval(c2))
      case Cond(o, c1, c2) ⇒ condPr(evalO(o), eval(c1), eval(c2))
      case When(o, c)      ⇒ cat(k, evalO(o), eval(c))
      //      eval (Anytime o c)  = snell  k (evalO o, eval c)
      case Until(o, c)     ⇒ absorb(k, evalO(o), eval(c))
      case _               ⇒ sys.error("todo")
    }
    eval _
  }
  
  def cat(k: Currency, bs: PR[Boolean], rs: PR[Double]): PR[Double] = PR(catCalc(bs.unPr, rs.unPr))

  def catCalc(b: Stream[RV[Boolean]], p: Stream[RV[Double]]): Stream[RV[Double]] = {
    val (bRv #:: bs) = b
    val (pRv #:: ps) = p
    if (bRv.forall(bv ⇒ bv)) Stream(pRv)
    else {
      val rest = catCalc(bs, ps)
      val (nextSlice #:: _) = rest
      val thisSlice = zipWith(bRv, pRv)((b, p) ⇒ if (b) p else 0.0)
      thisSlice #:: rest
    }
  }
}