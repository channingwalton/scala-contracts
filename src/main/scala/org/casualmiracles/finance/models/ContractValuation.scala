package org.casualmiracles.finance.models

import org.casualmiracles.finance.contracts._

/**
 * @author yl
 */
case class ContractValuation(pr:Option[PR[Double]]=None, trace: Option[String]=None, errors:Option[List[String]]=None)
