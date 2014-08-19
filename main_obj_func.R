obj_func <- function(SP_day){
z <- c(0,0,0,0)

#### objective function
# penalties/costs
hc <- 10 #holding cost
ave_prod_cost <- mean(unlist(psc_cost)) #shortage
pen_shor <- ave_prod_cost*150 #shortage
min_SPdays <- 2 #shortage
pen_emer <- 1 #penalty
pen_canc <- 0.5 #cancellation


## options.sp defines which stockpiles are being optimised.  
  
#1 holding cost
z[1] <- sum(psc_SPvol[, options.ps])*hc

#2 shortage penalty
min_coal_level <- psc_template
min_coal_level[,] <- rep(as.vector(SP1day_ave), times=1, each=interval_num) * min_SPdays
z[2] <- sum( (psc_SPvol[, options.ps] < min_coal_level[, options.ps]) *
             abs(min_coal_level[, options.ps] - psc_SPvol[, options.ps])) * 
                  pen_shor

## only incur emer/canc delivery costs if all dv's are chosen. if only SPinitial, then don't step through z3 and z4.

if (options.dv == 3){
#3 emergency delivery cost
z[3] <- sum(psc_cost[, options.ps] * 
              delv_emer[, options.ps]) * 
                pen_emer

#4 cancellation of delivery cost
z[4] <- sum(psc_cost[, options.ps] * 
              delv_canc[, options.ps]) * 
                pen_canc

}





# return all costs
return (z)

}

# nettStock <- psc_SPvol + psc_delvout - psc_burnout

# #2 shortage penalty
# source(paste(Rcode_path,"GetLoad.R",sep=sep_))
# source(paste(Rcode_path,"GetEnergyOut.R",sep=sep_))
# load <- apply(node_load,1,sum) #in GWh
# ener_sched <- rowSums(psall_ener_sched[,1:14],na.rm=T) #in GWh
# pen_shor <- 100000
# ener_dif <- ener_sched-(load*0.9)
# z[2] <- sum(ener_dif[ener_dif>0]) * pen_shor



# # shortage cost
# shortage_penalty <- 100000 #200*150
# Z <- Z + sum(psc_SPvol[,]<=0)*shortage_penalty
# 
# # delivery cost
# ExtraDelv <- 1.5
# WarnDays <- 20 
# psc_burnin / 30 * WarnDays
# SPbelowWarn <- psc_SPvol < psc_burnin/30*WarnDays
# Z <- Z + sum(SPbelowWarn*psc_cost*ExtraDelv) #+ sum((!SPbelowWarn)*psc_cost)

# #### objective function
# # holding cost
# interest  <- 0.05
# Z <- sum(psc_SPvol*psc_cost*interest)
# 
# # shortage cost
# shortage_penalty <- 100000 #200*150
# Z <- Z + sum(psc_SPvol[,]<=0)*shortage_penalty
# 
# # delivery cost
# ExtraDelv <- 1.5
# WarnDays <- 20 
# psc_burnin / 30 * WarnDays
# SPbelowWarn <- psc_SPvol < psc_burnin/30*WarnDays
# Z <- Z + sum(SPbelowWarn*psc_cost*ExtraDelv) #+ sum((!SPbelowWarn)*psc_cost)
