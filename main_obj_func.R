obj_func <- function(SP_day){
z <- c(0,0,0,0)

#### objective function
#1 holding cost
z[1] <- sum(psc_SPvol)*10

#2 shortage penalty
ave_prod_cost <- mean(unlist(psc_cost))
pen_shor <- ave_prod_cost*150
# pen_shor <- 1000000000
min_coal_level <- psc_template
min_coal_level[,] <- rep(as.vector(SP1day_ave), times=1, each=interval_num) * 2



z[2] <- sum( (psc_SPvol< min_coal_level) * abs(min_coal_level - psc_SPvol) ) * pen_shor
# nettStock <- psc_SPvol + psc_delvout - psc_burnout

# #2 shortage penalty
# source(paste(Rcode_path,"GetLoad.R",sep=sep_))
# source(paste(Rcode_path,"GetEnergyOut.R",sep=sep_))
# load <- apply(node_load,1,sum) #in GWh
# ener_sched <- rowSums(psall_ener_sched[,1:14],na.rm=T) #in GWh
# pen_shor <- 100000
# ener_dif <- ener_sched-(load*0.9)
# z[2] <- sum(ener_dif[ener_dif>0]) * pen_shor

#3 emergency delivery cost
pen_emer <- 1
z[3] <- sum(psc_cost*delv_emer*pen_emer)

#4 cancellation of delivery cost
pen_canc <- 0.5
z[3] <- sum(psc_cost*delv_canc*pen_canc)

# return all costs
return (z)

}

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
