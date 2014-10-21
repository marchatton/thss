########## Objective function
obj.target <- psc_template
obj.y  <- psc_template
obj.1day <- psc_template
obj.1day[,] <- rep(SP1day_ave, each=interval_num)
obj.mean <- sensivity.costs$ec[sen.anal]

min_coal_level <- psc_template
min_coal_level[,] <- rep(as.vector(SP1day_ave), times=1, each=interval_num) * min_SPdays
obj.min <- min_coal_level/obj.1day
obj.coalcost <- psc_cv/psc_heatrate * psc_cost *1000 # convert from R/MWh to R/kton

# penalties/costs
hc <- ( (1+sensivity.costs$hc[sen.anal])^(1/12) - 1) *12
pen_shor <- psc_cv / psc_heatrate * 75 * 1000 * 1000 #convert from R75/kWh to R/ton
pen_canc <- sensivity.costs$cc[sen.anal] 

pen_emer <- function(){
  obj.target[,] <- rep(dv_SPinitial, each=interval_num)/obj.1day
  obj.var <- SPvar_emer/obj.1day
  
  obj.y <- obj.mean *
    exp(-obj.mean * ((obj.target-obj.min) - obj.var)/
          (obj.target-obj.min) * 2.5) + 
    0.25
  
  obj.y [(obj.target-obj.min) - obj.var < 0] <- obj.mean + 0.25
  
  return(obj.y)
}

obj_func <- function(){
  z <- c(0,0,0,0)
  
  #1 holding cost
  z[1] <- sum(psc_SPvol[, options.ps] * obj.coalcost[, options.ps]) * hc 
  
  #2 shortage penalty
  z[2] <- sum( (psc_SPvol[, options.ps] < min_coal_level[, options.ps]) *
                 abs(min_coal_level[, options.ps] - psc_SPvol[, options.ps]) * 
                 pen_shor)
  
  ## only incur emer/canc delivery costs if all dv's are chosen. if only SPinitial, then don't step through z3 and z4.
  
  if (options.dv == 3){
    #3 emergency delivery cost
    z[3] <- sum(obj.coalcost[, options.ps] * 
                  delv_emer[, options.ps] * 
                  pen_emer())
    
    #4 cancellation of delivery cost
    z[4] <- sum(obj.coalcost[, options.ps] * 
                  delv_canc[, options.ps]) * 
      pen_canc
    
  }
  
  # return all costs
  return (z)
}



# xyz <- psc_template
# xyz[,] <- 50*rep(SP1day_ave, each=interval_num)
# sum(xyz[, options.ps] * obj.coalcost[, options.ps]) * ((1+hc)^(1/12)-1) *12      /1000000000*12/8


# pen_emer <- function(){
#   obj.target[,] <- rep(dv_SPinitial, each=interval_num)
#   obj.actual <- SPvar_emer/obj.1day
#   
#   if (obj.actual < min_coal_level){
#     obj.y[,] <- obj.mean
#   }else if ((min_coal_level <= obj.actual ) && (obj.actual < obj.target)){
#     obj.y <- obj.mean *
#       exp(-obj.mean * (obj.actual - min_coal_level)/
#             (obj.target-min_coal_level) * 2.5) + 
#       0.25
#   }else{
#     obj.y[,] <- 0
#   }
#   
#   return(obj.y)
# }

# sum(xyz[, options.ps] * obj.coalcost[, options.ps]) * ((1+hc)^(1/12)-1) *12      /1000000000*12/8
