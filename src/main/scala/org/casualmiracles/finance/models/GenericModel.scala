package org.casualmiracles.finance.models

import scala.Stream._

import org.casualmiracles.finance.contracts._
import org.casualmiracles.finance.contracts.Contracts._

import com.casualmiracles.utilities.ContractsTracer

case class Model(
    modelStart: Date,
    
    disc: Currency ⇒ PR[Boolean] ⇒ PR[Double] ⇒ PR[Double],
    snell: Currency ⇒ PR[Boolean] ⇒ PR[Double] ⇒ PR[Double],
    exch: Currency ⇒ Currency ⇒ PR[Double],
    comp: Currency ⇒ PR[Boolean] ⇒ PR[Double] ⇒ PR[Double],
    absorb: Currency ⇒ PR[Boolean] ⇒ PR[Double] ⇒ PR[Double],
    rateModel: Currency ⇒ PR[Double],
    
    inputs: ModelParameters
  )
 
abstract class GenericModel {
  // tracing 
  val tracer = new ContractsTracer()

  // Compositional valuation semantics for contracts
  def evalC(model: Model, k: Currency): Contract ⇒ PR[Double] = {
    def eval(level: Int, contract: Contract): PR[Double] = contract match {
      case Zero            ⇒ { val f = "Zero: bigK(0)"
                                tracer.trace(level, "In:"+f )
                                tracer.trace(level, "Out: "+f, bigK(0) )
                              }
      case One(k2)         ⇒ {  val f = "One(k2) => model.exch(k)(k2)"
                                tracer.trace(level, "In:"+f)
                                tracer.trace( level, "Out:"+f, model.exch(k)(k2) )
                              }
      case Give(c)         ⇒ { val f = "Give(o, c) => bigK(-1.0) %* eval(level+1, c)"
                                tracer.trace(level, "In:"+f )
                                tracer.trace(level, "Out"+f, bigK(-1.0) %* eval(level+1, c) )
                              }
      case Scale(o, c)     ⇒ { val f = "Scale(o, c) => evalO(o) %* eval(c)"
                                tracer.trace(level, "In:"+f )
                                tracer.trace(level, "Out"+f, evalO(level+1, o) %* eval(level+1, c) )
                              }
      case And(c1, c2)     ⇒ { val f = "And(c1,c2) => eval(c1) %+ eval(c2)"
                                tracer.trace(level, "In:"+f)
                                tracer.trace(level, "Out:"+f, eval(level+1, c1) %+ eval(level+1, c2) )
                             }
      case Or(c1, c2)      ⇒ { val f = "Or(c1,c2) => max(eval(c1), eval(c2))"
                                tracer.trace(level, "In:"+f)
                                tracer.trace(level, "Out:"+f, max(eval(level+1, c1), eval(level+1, c2)) )
                             }
      case Cond(o, c1, c2) ⇒ { val f = "Cond(o,c1,c2) => condPr(evalO(o), eval(c1), eval(c2))"
                                tracer.trace(level, "In:"+f)
                                tracer.trace(level, "Out:"+f, condPr(evalO(level+1, o), eval(level+1, c1), eval(level+1, c2)) )
                             }
      case When(o, c)      ⇒ { val f = "When(o,c) => disc(k,evalO(o), eval(c))"
                                tracer.trace(level, "In:"+f)
                                tracer.trace(level, "Out:"+f, model.disc(k)(evalO(level+1,o))(eval(level+1, c)) )
                             }
      case Upto(o, c)      ⇒ { val f = "Upto(o,c) => comp(k,evalO(o), eval(c))"
                                tracer.trace(level, "In:"+f)
                                tracer.trace(level, "Out:"+f, model.comp(k)(evalO(level+1,o))(eval(level+1, c)) )
                             }
      case Anytime(o, c)  => { val f = "Anytime(o,c) => snell(k, evalO(o), eval(c))"
                                tracer.trace(level, "In:"+f)
                                tracer.trace(level, "Out:"+f, model.snell(k)(evalO(level+1, o))(eval(level+1, c)) )
                             } 
      case Until(o, c)     ⇒ { val f = "Until(o,c) => absorb(k, evalO(o), eval(c))"
                                tracer.trace(level, "In:"+f)
                                tracer.trace(level, "Out:"+f, model.absorb(k)(evalO(level+1, o))(eval(level+1, c)) )
                             }
      case _               ⇒ sys.error("todo")
    }
    tracer.trace(0, s"In:evalC(model,$k")
    tracer.trace(0, "Interest Rate Model", model.rateModel(k))
    tracer.trace(0, model.inputs.toString() )
    eval(1, _) 
  }
   
  // Overload for backward compatible tracing so we don't break Cashflows
  def evalO[T](o: Observable[T]): PR[T] = {
    o.f(time0)
  }
  
  def evalO[T](i:Int, o: Observable[T]): PR[T] = {
    tracer.trace( i, "eval0", evalO(o) )
  }
    
}

trait GenericComputations extends InterestRateModel {
  
  def takePr[T](n: Int, pr: PR[T]) = PR(pr.unPr.take(n))
  def horizonPr(pr: PR[_]) = pr.unPr.length
  def andPr(pr: PR[Boolean]):Boolean = pr.unPr.forall(rvb => rvb.forall(identity))

  
 
  // compounding functions
  // TODO: refactor hard-coded constant rate into another InterestRateClas r --> 0.5
  def discretecomp(r:Double):Double = 1.0+r/100.0
  // TODO: add step as a first parameter when creating model and partially apply it
  def continuouscomp(r:Double):Double = Math.exp((r/100.0)*(1.0/5.0))
  
  // reducing logic
  def crr(b:Boolean, p:Double, q:Double):Double = if (b) p else q
  def snell(b:Boolean, p:Double, q:Double):Double = if (b) p else Math.max(p, q)
  
  
  def makeModel(modelDate: Date, inputsData: ModelParameters) = Model(
    modelStart = modelDate,
    disc = (discount( crr, discretecomp, inputsData.p _, _:Currency, _:PR[Boolean], _:PR[Double])).curried,
    snell = (discount( snell, discretecomp, inputsData.p _, _:Currency, _:PR[Boolean], _:PR[Double])).curried,
    exch = (exch _).curried,
    comp = (comp _).curried,
    absorb = (absorb _).curried,
    rateModel = rateModel _,
    inputs = inputsData)

  // Generic discount function for disc/snell primitives 
  def discount(f: (Boolean, Double, Double) => Double, cf:Double=>Double, p:()=>Double, k: Currency, bs: PR[Boolean], rs: PR[Double]): PR[Double] = {

    //def disc(k: Currency, bs: PR[Boolean], rs: PR[Double]): PR[Double] = PR(discCalc(bs.unPr, rs.unPr, rateModel(k).unPr))
    def discCalc(b: Stream[RV[Boolean]], p: Stream[RV[Double]], rate: Stream[RV[Double]]): Stream[RV[Double]] = {
      val (bRv #:: bs) = b
      val (pRv #:: ps) = p
      val (rateRv #:: rs) = rate
      if (bRv.forall(bv ⇒ bv)) Stream(pRv)
      else {
        val rest = discCalc(bs, ps, rs)
        val (nextSlice #:: _) = rest
        
        // TODO: hack different discounting rate
        // val discSlice = zipWith(prevSlice(nextSlice), rateRv)((x, r) ⇒ x / (1 + r / 100))
        // for discrete: (x, r)=> x/(1+r/100)
        // for continous: (x, r)=> x*Math.exp(-0.05*(1/5))
        val discSlice = zipWith(prevSlice(nextSlice), rateRv)((x, r) ⇒ x/cf(r) )
        //val thisSlice = zipWith3(bRv, pRv, discSlice)((b, p, q) ⇒ if (b) p else q)
        val thisSlice = zipWith3(bRv, pRv, discSlice)((b, p, q) ⇒ f(b, p, q) )
        thisSlice #:: rest
      }
    }
  
    def prevSlice(s: RV[Double]): RV[Double] = s match {
      case Empty                ⇒ Empty
      case (_ #:: Empty)        ⇒ Empty
      // case (n1 #:: n2 #:: rest) ⇒ ((n1 + n2) / 2.0) #:: prevSlice(n2 #:: rest)
      // TODO: Hack hard-code 0.5169, 1-0.5169 instead of hard-coded 0.5, 0.5
      // TODO: those should be moved into Model parameters
      // sic!! the parameters n1 and n2 were swapped around!!
      
      // TODO: HACK: with divident
      // no divident:    case (n1 #:: n2 #:: rest) ⇒ (n1*(1-0.5169) + n2*0.5169) #:: prevSlice(n2 #:: rest)
      case (n1 #:: n2 #:: rest) ⇒ (n2*p() + n1*(1-p())) #:: prevSlice(n2 #:: rest)
    } 
    PR(discCalc(bs.unPr, rs.unPr, rateModel(k).unPr))
  }

  // compound -- opposite to discount -- primitive
  def comp(k: Currency, bs: PR[Boolean], rs: PR[Double]): PR[Double] = {
      // TODO: hack to prove the point
      //def compCalc()
      //  def ratesUpDown(rateNow: Double, up: Double, down: Double): PR[Double] = {
    
      // hard coded: 100.0, 1.1183, 0.8942
    val rateNow = 100.0
    val up = 1.1183
    val down = 0.8942
       def makeRateSlices(rateNow: Double, n: Int): Stream[RV[Double]] = rateSlice(rateNow, n) #:: makeRateSlices( rateNow * down, n + 1)
       def rateSlice(minRate: Double, n: Int) = comb(minRate).take(n)
       def comb(x:Double): Stream[Double] = x #:: comb(x/down*up)
       PR(makeRateSlices(rateNow, 1))
    }

  def absorb(k: Currency, prb: PR[Boolean], prRvs: PR[Double]): PR[Double] = {
    val bSlices = prb.unPr
    val rvs = prRvs.unPr
    PR(zipWith(bSlices, rvs)((bRv, rvsRv) ⇒ zipWith(bRv, rvsRv)((o, p) ⇒ if (o) 0 else p)))
  }

  def exch(k1: Currency, k2: Currency): PR[Double] = PR(konstSlices(1))

  private def expectedValue(outcomes: RV[Double], probabilities: RV[Double]): Double = zipWith(outcomes, probabilities)(_ * _).sum

  def expectedValuePr( pr: PR[Double]): Stream[Double] = zipWith(pr.unPr, probabilityLattice)(expectedValue(_,_))
      
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

}
