package org.casualmiracles.finance.models

/**
 * @author yl
 */
case class ContractParameters( 
    model: String,
    
    contract: String, 
    strike: Double, 
    asset: Double, 
    
    interest: Double,
    volatility: Double,
    divident: Double,
    expiry: Double,
    steps: Int,
    
    trace: Boolean,
    image: Boolean 
)