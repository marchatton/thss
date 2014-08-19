###### CLEAR PREVIOUS RUN 
sink()
sink()
sink()
cat("\014") #clear console
rm(list = ls()) #clear global environment

maxIterations <- 23
N <- 150
fp_set <- 1
options.ps <- c(1:14,99)[15] #which power station to optimise. 99 (#15) = all
options.dv <- c(1,3)[1]
options.eval <- c(1,5)[1]

if (options.dv == 3){
  print(paste("estimated completion time = ", round(maxIterations*N*15*8/60/60,2), " hours", sep="")) 
}else if (options.dv == 1){
  print(paste("estimated completion time = ", round(maxIterations*N*15/60/60,2), " hours", sep="")) 
}

###### START STOPWATCH
tic <- Sys.time() #begin stopwatch

###### FILE PATHS USED IN OPTIMISER
## !!Adjust these paths to the folder where EFS is running!!
## First Start DIAS then Run this in RStudio
if (fp_set == 0){
  Rcode_path  <- file.path("C:\\Users\\MarcHatton\\Copy\\Postgraduate\\Thesis\\Algorithms\\R code - Marc")
  THEPATH  <-  "C:/Users/MarcHatton\\Desktop\\EFS APP"
  THEDBPATH  <-  "C:\\Users\\MarcHatton\\Desktop\\EFS APP\\e-breadboard\\resources\\za.co.enerweb_energy-flow-simulator3-var\\dbs"
  print(fp_set)
}else if(fp_set == 1){
  Rcode_path  <- file.path("H:\\R code - Marc\\thss") #where to source Rcode
  THEPATH  <-  "C:\\Users\\17878551\\Desktop\\EFS APP"
  THEDBPATH  <-  "C:\\Users\\17878551\\Desktop\\EFS APP\\e-breadboard\\resources\\za.co.enerweb_energy-flow-simulator3-var\\dbs" 
  print(fp_set)
}else if(fp_set == 2){
  Rcode_path  <- file.path("H:\\R code - Marc2") #where to source Rcode
  THEPATH  <-  "C:\\Users\\17878551\\Desktop\\EFS APP"
  THEDBPATH  <-  "C:\\Users\\17878551\\Desktop\\EFS APP\\e-breadboard\\resources\\za.co.enerweb_energy-flow-simulator3-var\\dbs" 
  print(fp_set)
}else if(fp_set == 3){
  Rcode_path  <- file.path("H:\\R code - Marc3") #where to source Rcode
  THEPATH  <-  "C:\\Users\\17878551\\Desktop\\EFS APP"
  THEDBPATH  <-  "C:\\Users\\17878551\\Desktop\\EFS APP\\e-breadboard\\resources\\za.co.enerweb_energy-flow-simulator3-var\\dbs" 
  print(fp_set)
}else if(fp_set == 4){
  Rcode_path  <- file.path("H:\\R code - Marc4") #where to source Rcode
  THEPATH  <-  "C:\\Users\\17878551\\Desktop\\EFS APP"
  THEDBPATH  <-  "C:\\Users\\17878551\\Desktop\\EFS APP\\e-breadboard\\resources\\za.co.enerweb_energy-flow-simulator3-var\\dbs" 
  print(fp_set)
}else if(fp_set == 5){
  Rcode_path  <- file.path("C:\\Users\\MarcHatton\\Copy\\Postgraduate\\Thesis\\Algorithms\\R code - Marc") #where to source Rcode
  THEPATH  <-  "C:\\Users\\MarcHatton\\Desktop\\EFS APP"
  THEDBPATH  <-  "C:\\Users\\MarcHatton\\Desktop\\EFS APP\\e-breadboard\\resources\\za.co.enerweb_energy-flow-simulator3-var\\dbs" 
  print(fp_set)
}

###### NECESSARY INITIAL VALUES USED IN DEFINING SETTINGS
func_name <- "CEM"
t <- 1 # time-step counter

###### MAIN SETTINGS. MUST CHANGE FILEPATHS IF RUNNING ON A DIFFERENT COMPUTER. MUST ALSO INSTALL PACKAGES LISTED THEREIN.
source(paste(Rcode_path,"main_settings.R",sep=.Platform$file.sep))

###### IF OPTIONS.PS = 99 (al powerstations), then we let PS = a seq of 1 to psc_tot
if (options.ps == 99){
  options.ps <- seq(from=1, to=psc_tot, by=1)
  print("all powerstations will be optimised")
} else {
  print(paste("powerstation ", options.ps," (", colnames(psc_template)[options.ps], ") ",  "will be optimised", sep=""))
}

###### LOAD OBJECTIVE FUNCTION
source(paste(Rcode_path,"main_obj_func.R",sep=.Platform$file.sep))

###### LOAD EMER DELIVERIES / CANCELLATION DELIVERIES CALCULATOR FUNCTION
# source(paste(Rcode_path,"Calc_EmerOrCanc.R",sep=sep_)) #@

###### Stop the optimiser if the estimator has not been run initially.
# if file doesnt exist, stop the optimiser. Estimates are required for the optimiser's deliveries
if (!file.exists(paste(optEstPath,"est_burnout.csv",sep=sep_))) {
  stop("For the optimiser to work, 'main_estimates.R' must first be run.") 
}

###### GET INITIAL VALUES
#when using getDBvalues() be aware of all the optional parameters
#the default values are: name_='psc_delvin', param_='COAL_DELIVERY_IN', paramkind_='INP', type_='COAL_PS', ent_='POWERSTATION'
psc_SPinitial <- getDBvalues(param_ = 'INITIALSTOCK', paramkind_='INP')
psc_delvin <- getDBvalues(param_ = 'COAL_DELIVERY_IN', paramkind_='INP')
psc_delvout <- getDBvalues(param_ = 'COAL_DELIVERY_OUT', paramkind_ = 'RES')
psc_burnin <- getDBvalues(param_ = 'COAL_BURN_IN', paramkind_='INP')
psc_burnout <- getDBvalues(param_ = 'COAL_BURN_OUT', paramkind_ = 'RES')
psc_SPvol <- getDBvalues(param_ = 'STOCKPILE_VOL', paramkind_ = 'RES')
psc_cost <- getDBvalues(param_ = 'COSTOFSUPPLY', paramkind_='INP')

###### Use estimator's ave burnout to set delvin (baseline deliveries). Only set in the beginning, doesnt change thereafter. 
est_ave_burnout <- read.csv(paste(paste(optEstPath, est_names[4], sep=sep_) ,".csv",sep=""), 
                    header = TRUE, sep = ",", quote = "\"", dec = ".", 
                    fill = TRUE, comment.char = "")
est_ave_burnout[,1] <- NULL # clean dataframe
est_ave_burnout <- apply(est_ave_burnout,2,mean)
dv_delv_base <- est_ave_burnout
dv_delv <- psc_template
dv_delv[,]  <- dv_delv_base

setDBvalues(values_ = dv_delv, param_ = 'COAL_DELIVERY_IN')
psc_delvin <- getDBvalues(param_ = 'COAL_DELIVERY_IN', paramkind_='INP')

###### SET UP NUMBER OF TIMES TO RE-RUN SIMOPT
#@ numRuns  <- 1 
#@ Z_final  <- rep(NA,numRuns)
#@ success_count  <- 0
######choose to display a graph to view convergence. Turn off if you want algorithm to run faster
#@quick_graph <- FALSE

###### Values to be set

#(numVar = dv_SPinitial + dv_SPlower + dv_SPupper + dv_delv_more + dv_delv_less)
numVar <- psc_tot + psc_tot + psc_tot # must add dv_delv_more + dv_delv_less

rho <- 0.2 #0<=rho<=1. Percentage of solutions to keep (elite %)
epsNum <- 5 #Maximum error (a.k.a required accuracy)
epsErr  <- 10 #try make it as close to zero as possible.
# maxIterations <- 5
alpha <- 0.75 #convergence rate, typical varies between 0.6 & 0.9

###### INITIALISE DECISION VARIABLES & SET CONSTRAINTS
### DV: Stockpiles: Initial, UpperWarning & LowerWarning.
mus <- rep(NA,numVar)
sigmas <- rep(NA,numVar) 

### initial stock
SP1day_ave <- apply(psc_burnout,2,mean)/30
dv_SPinitial <- SP1day_ave*15 
mus[1:psc_tot] <- dv_SPinitial #@ keep dv_SPinitial. I use it below in next 2 paragraphs
sigmas[1:psc_tot] <- mus[1:psc_tot] #@@@@

### SET CONSTRAINTS
llim_init <- SP1day_ave*5
ulim_init <- SP1day_ave*25
llim_delv <- rep(0,psc_tot)
ulim_delv <- rep(3000,psc_tot)

llim_lower <- SP1day_ave*1
ulim_lower <- 0.9
llim_upper <- 1.1
ulim_upper <- SP1day_ave*30

### upperWarning & lowerWarning
#lowerWarning
mus[psc_tot + 1:psc_tot] <- (llim_lower + dv_SPinitial*ulim_lower) /2
sigmas[psc_tot + 1:psc_tot] <- mus[psc_tot + 1:psc_tot] #@@@@

#upperWarning
mus[2*psc_tot + 1:psc_tot] <- (dv_SPinitial*llim_upper + ulim_upper) /2
sigmas[2*psc_tot + 1:psc_tot] <- mus[2*psc_tot + 1:psc_tot] #@@@@

###### Initialise other values
elite <- as.integer(rho*N)
mus_prev <- mus*N # arbitrary large vector
sigmas_prev <- sigmas*N # arbitrary large vector
x <- matrix(0,N,numVar)
Z_x <- matrix(0,N,1+numVar) #objective function including: x, obj func value, emer & canc deliveries
z_archive <- rep(NA,maxIterations)
z_quantile <- rep(NA,maxIterations)

delv_emer <- psc_template
delv_canc <- psc_template
delv_emer[,] <- 0
delv_canc[,] <- 0
SPvar <- 0

#print parameters
write.row(c("opt_type",func_name), append=FALSE)
write.row(c('numVar',numVar))
write.row(c('rho',rho))
write.row(c('N',N))
write.row(c('epsNum',epsNum))
write.row(c('epsErr',epsErr))
write.row(c('maxIterations',maxIterations))
write.row(c('alpha',alpha))
write.row('')

#print the header for stored data matrix
header <- c("t","seed","Z(mus)","z.h", "z.s","z.e", "z.c", "Z(quan)",
            paste('mu_in', 1:psc_tot, sep=""), #mu of initial SP
            paste('mu_lo', 1:psc_tot, sep=""), #mu of upper warning
            paste('mu_up', 1:psc_tot, sep=""), #mu of lower warning
            paste('si_in', 1:psc_tot, sep=""), #sigma of initial SP
            paste('si_lo', 1:psc_tot, sep=""), #sigma of upper warning
            paste('si_up', 1:psc_tot, sep=""), #sigma of lower warning
            paste('emer', 1:(interval_num*psc_tot), sep=""), #emergency deliveries
            paste('canc', 1:(interval_num*psc_tot), sep=""), #cancellation of deliveries
            paste('base', 1:(interval_num*psc_tot), sep="") #baseline deliveries            
)
write.row(header)

#print initialised values
write.row(c("0", rep("NA",times=7),
            mus,
            sigmas,
            rep(0,interval_num*psc_tot),
            rep(0,interval_num*psc_tot),
            dv_delv_base
))

###### MAIN LOOP
# while (ifelse(t<epsNum, TRUE, !all(abs(z_quantile[t-seq(0,length=epsNum)] - z_quantile[t]) <= epsErr)) && (t < maxIterations))
# {  

while (t <= maxIterations){
  sim_seed.current <- sim_seed[1]
  changeSimSet(seed=sim_seed.current) #change the simulation seed for current time step (t)
  mus_prev <- mus
  sigmas_prev <- sigmas
  
  for (j in 1:psc_tot){ 
    x[,j]            <- rtruncnorm(N, a=llim_init[j],  b=ulim_init[j],  mean=mus[j], sd=sigmas[j])
    x[,j + psc_tot]  <- rtruncnorm(N, a=llim_lower[j], b=ulim_lower*dv_SPinitial[j], mean=mus[j+psc_tot], sd=sigmas[j+psc_tot])
    x[,j+ 2*psc_tot] <- rtruncnorm(N, a=llim_upper*dv_SPinitial[j], b=ulim_upper[j], mean=mus[j+2*psc_tot], sd=sigmas[j+2*psc_tot])
  }
    
  #calculate and store values in a matrix
  for(kkk in 1:N){
    dv_SPinitial <- x[kkk,1:psc_tot]
    dv_SPlower   <- x[kkk,1:psc_tot + psc_tot]
    dv_SPupper   <- x[kkk,1:psc_tot + 2*psc_tot]
    source(paste(Rcode_path,"main_sim.R",sep=sep_)) #call the simulator
    
    #only calculate emer/canc if options.dv == 3
    if (options.dv == 3){
    
      #next 2 lines must go together!! and in that order!!
      mode <- "x"
      source(paste(Rcode_path,"Calc_EmerOrCanc.R",sep=sep_)) #calculate emergency & cancellation deliveries
    }
    
    Z_x[kkk,]  <- c(sum(obj_func()),x[kkk,])
    #@ write.csv(c("emer",unlist(delv_emer),"canc",unlist(delv_canc)), file = paste("C:\\Users\\17878551\\Desktop\\EFS APP\\CSPS_optimiser_output\\EmerOrCanc_t", t,"_k", kkk, ".csv",sep='')) 
  }
  
  #sort the population from lowest to highest cost (best to worst)
  Z_x_sorted  <- Z_x[order(Z_x[,1]),]
  
  if (elite == 1){
    mus <- Z_x_sorted[1:numVar+1]
    sigmas  <- Z_x_sorted[1:elite,1:numVar+1]
  }else{
  mus <- apply(Z_x_sorted[1:elite,1:numVar+1] , 2, mean)
  sigmas  <-  apply(Z_x_sorted[1:elite,1:numVar+1] , 2, sd)
  }
  
  z_quantile[t] <- Z_x_sorted[elite]
  
  #smoothing function. used to avoid premature convergence
  mus <- alpha*mus + (1-alpha)*mus_prev
  sigmas <- alpha*sigmas + (1-alpha)*sigmas_prev
  
  #calculate decision variables
  dv_SPinitial <- mus[1:psc_tot]
  dv_SPlower   <- mus[1:psc_tot + psc_tot]
  dv_SPupper   <- mus[1:psc_tot + 2*psc_tot]
  source(paste(Rcode_path,"main_sim.R",sep=sep_))

  #only calculate emer/canc if options.dv == 3
  if (options.dv == 3){
    
    #next 2 lines must go together!! and in that order!!
    mode <- "mu"
    source(paste(Rcode_path,"Calc_EmerOrCanc.R",sep=sep_))
  }
    
  Z_mus <- sum(obj_func())
  
  #export results of time step (t) to .csv
  write.row(c(t, sim_seed.current, Z_mus,obj_func(), z_quantile[t], mus, sigmas, unlist(delv_emer), unlist(delv_canc), unlist(dv_delv)))
  #@ used for inspection of Z_x_sorted. must remove for final version
  #write.csv(Z_x_sorted, file = paste(THEPATH,"\\CSPS_optimiser_output\\Z_x_sorted", t, ".csv",sep=''))  
  
  #If we chose quick graph = TRUE, we need to keep record of obj_func values
  #@ if (quick_graph) 
  #@ {z_archive[t] <- Z_mus}
  
  t <- t+1
}

#write blank line. just for fun
write.row('')

#@
# #did algorithm converge or where max iterations reached?
# if (ifelse(t<epsNum, TRUE, !all(abs(z_quantile[t-seq(0,length=epsNum)] - z_quantile[t]) <= epsErr))) {
#   write.row("Maximum number of iterations reached. Did not converge.")
# }else{
#   write.row(paste("Winner winner chicken dinner. Successfully converged to ",Z_mus))
#   success_count <- success_count + 1
# }
# #create quick graph
# if (quick_graph){
#   plot(z_archive, type="o", col="blue")
#   title(xlab="t",ylab=func_name, main="Convergence of objective function", col.main="red", font.main=4)
# }
# 
# Z_final[k] <- Z_mus


#@}

#@ success_percen  <- success_count / numRuns *100
#@ print(success_percen)

toc <- Sys.time() #end stopwatch
print(toc-tic)
winDialog("ok", paste("OPTIMISER completed in ",round(print(toc-tic),1),units(toc-tic), sep=""))

