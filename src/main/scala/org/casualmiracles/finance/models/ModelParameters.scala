package org.casualmiracles.finance.models

/**
 * @author yl
 */

// TODO: Inference other parameters by changing any [NiceToHave]
// TODO: DISCUSS: Consider immutability in method chaining [https://en.wikipedia.org/wiki/Method_chaining]
//        but this as well [http://stackoverflow.com/questions/6955563/is-it-possible-to-chain-methods-from-different-traits]
class ModelParameters( var r:Double=0.0, var sigma: Double=0.0, var div:Double=0.0, var step: Double=0.0 ){
  import ModelParameters._
  
  private var _up:Double = 0
  private var _down:Double = 0
  private var _p: Double = 0
  
  _up = upCalc(sigma, step)
  _down = downCalc(_up)
  _p = pCalc(r, div, step, _up, _down)

  def up:Double = _up
  def up_= (d:Double):Unit = _up = d
  def up(d:Double):ModelParameters = { up = d; this }
  
  def down:Double = _down
  def down_= (d:Double):Unit = _down = d
  def down(d:Double):ModelParameters = { down = d; this }
  
  def p:Double = _p
  def p_= (d:Double):Unit = _p = d
  def p(d:Double):ModelParameters = { p = d; this }
  
  override def toString:String = {
    s"sigma: $sigma; r: $r; div: $div; step: $step; up: $up; down: $down; p: $p"
  }
}

object ModelParameters {
  // up/down factors and risk-neutral probability
  def upCalc(sigma:Double, step:Double):Double = Math.exp(sigma*Math.sqrt(step))
  def downCalc(up:Double):Double = 1.0/up 
  def pCalc(r: Double, div: Double, step: Double, U: Double, D: Double): Double = (Math.exp((r-div)*step) - D)/(U-D)
}