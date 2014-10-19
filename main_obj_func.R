########## Objective function
obj.target <- psc_template
obj.y  <- psc_template
obj.1day <- psc_template
obj.1day[,] <- rep(SP1day_ave, each=interval_num)
obj.mean <- sensivity.costs$ec[sen.anal]

min_coal_level <- psc_template
min_coal_level[,] <- rep(as.vector(SP1day_ave), times=1, each=interval_num) * min_SPdays

pen_emer <- function(){
  obj.target[,] <- rep(dv_SPinitial, each=interval_num)
  obj.actual <- SPvar_emer/obj.1day
  
  if (obj.actual < min_coal_level){
    obj.y[,] <- obj.mean
  }else if ((min_coal_level <= obj.actual ) && (obj.actual < obj.target)){
    obj.y <- obj.mean *
    exp(-obj.mean * (obj.actual - min_coal_level)/
          (obj.target-min_coal_level) * 2.5) + 
    0.25
  }else{
    obj.y[,] <- 0
  }
  
  return(obj.y)
}

obj_func <- function(){
  z <- c(0,0,0,0)
  
  #### objective function
  # penalties/costs
  hc <- sensivity.costs$hc[sen.anal] #holding cost
  
  psc_heatrate   <- getDBvalues(param_ = 'HEATRATE', paramkind_ = 'INP')
  psc_cv   <- getDBvalues(param_ = 'CV', paramkind_ = 'INP')
  pen_shor <- psc_cv / psc_heatrate * 75 * 1000
  
  pen_canc <- sensivity.costs$cc[sen.anal] #cancellation
    
  ## options.sp defines which stockpiles are being optimised.  
  
  #1 holding cost
  z[1] <- sum(psc_SPvol[, options.ps] * psc_cost[, options.ps]) * hc
  
  #2 shortage penalty
  z[2] <- sum( (psc_SPvol[, options.ps] < min_coal_level[, options.ps]) *
                 abs(min_coal_level[, options.ps] - psc_SPvol[, options.ps]) * 
                 pen_shor)
  
  ## only incur emer/canc delivery costs if all dv's are chosen. if only SPinitial, then don't step through z3 and z4.
  
  if (options.dv == 3){
    #3 emergency delivery cost
    z[3] <- sum(psc_cost[, options.ps] * 
                  delv_emer[, options.ps] * 
                  pen_emer())
    
    #4 cancellation of delivery cost
    z[4] <- sum(psc_cost[, options.ps] * 
                  delv_canc[, options.ps]) * 
      pen_canc
    
    return(z)
  }
  
  # return all costs
  return (z)
}


