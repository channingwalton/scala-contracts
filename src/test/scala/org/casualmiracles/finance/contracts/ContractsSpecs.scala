package org.casualmiracles.finance.contracts

import org.specs2.mutable._
import org.specs2.matcher.DataTables
import Cashflows._
import Contracts._
import Stream._

class ContractsSpecs extends SpecificationWithJUnit with DataTables {

  "contracts" should {
    "generate cashflows" in {
      "contract" || "cashflow" || "steps" |>
        Zero ! Stream(0.0).take(3) ! 3 | {
          (contract, expected, steps) ⇒ cashflow(contract).take(steps) must_== expected
        }
    }
  }
}
