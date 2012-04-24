package org.casualmiracles.finance.contracts

import Contracts._

case class Observable[T](f: Date ⇒ PR[T]) {
  override def toString = "Observable " + f(time0) + ")"
}