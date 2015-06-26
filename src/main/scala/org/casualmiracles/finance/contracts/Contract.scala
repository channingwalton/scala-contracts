package org.casualmiracles.finance.contracts

object Contract {
  implicit class WithEnrichment(c: Contract) {
    def and(c2: Contract) = And(c, c2)
    def andGive(c2: Contract) = And(c, Give(c2))
    def or(c2: Contract) = Or(c, c2)
  }
}

trait Contract

case object Zero extends Contract
case class One(currency: Currency) extends Contract
case class Scale(obs: Observable[Double], contract: Contract) extends Contract
case class When(obs: Observable[Boolean], c: Contract) extends Contract
case class Upto(n:Double) extends Contract
case class Anytime(obs: Observable[Boolean], c: Contract) extends Contract
case class Until(obs: Observable[Boolean], c: Contract) extends Contract
case class Cond(obs: Observable[Boolean], c1: Contract, c2: Contract) extends Contract
case class Or(c1: Contract, c2: Contract) extends Contract
case class And(c1: Contract, c2: Contract) extends Contract
case class Give(contract: Contract) extends Contract