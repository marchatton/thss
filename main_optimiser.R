# d.scen=1
# sen.anal=9
# maxIterations=100
# N=50
# options.ps=99
# options.dv=3
# options.eval=1
# option.halfwidth=FALSE
# out.stat=3
# load_shedding=FALSE
# ls.id=1


optimser.csps <- function(d.scen=1, sen.anal=9, maxIterations=100, N=50, options.ps=99, options.dv=3, options.eval=1, option.halfwidth=FALSE, out.stat=3, load_shedding=FALSE, ls.id=NA){
  
  ############################ INITIALISATION ################################
  
  # calculate estimated completion time
  if (options.dv == 3){
    print(paste("estimated completion time = ", round(maxIterations*N*15*8/60/60,2), " hours", sep="")) 
  }else if (options.dv == 1){
    print(paste("estimated completion time = ", round(maxIterations*N*15/60/60,2), " hours", sep="")) 
  }
  
  # begin stopwatch
  tic <- Sys.time() 
  
  ###### NECESSARY INITIAL VALUES USED IN "main_settings.R"
  func_name <- "CEM"
  t <- 1 # time-step counter
  
  ###### SENSITIVITY ANALYSIS
  sensivity.costs <- data.frame(ec=c(rep(c(1.75,0.75),4) , 1.25),
                                cc=c(rep(c(0.8,0.2),times=2, each=2) , 0.5),
                                hc=c(rep(c(0.0875,0.0775), each=4) , 0.0825)
  )  
  
  ###### MAIN SETTINGS. MUST CHANGE FILEPATHS IF RUNNING ON A DIFFERENT COMPUTER. MUST ALSO INSTALL PACKAGES LISTED THEREIN.
  source(paste(Rcode_path,"main_settings.R",sep=.Platform$file.sep), local=TRUE)
  
  ###### IF OPTIONS.PS = 99 (all powerstations), then we let PS = a sequence of 1-to-psc_tot
  if (options.ps == 99){
    options.ps_write <- "all"
    options.ps <- 1:psc_tot
    print("all powerstations will be optimised")
  } else {
    print(paste("powerstation ", options.ps," (", colnames(psc_template)[options.ps], ") ",  "will be optimised", sep=""))
  }
  
  
  ###### CHECK THAT ESTIMATED VALUES EXIST
  # Stop the optimiser if the estimator has not been run initially.
  # If file doesnt exist, stop the optimiser. Estimates are required for the optimiser's planned deliveries.
  if (!file.exists(paste(optEstPath,"est_burnout.csv",sep=sep_))) {
    stop("For the optimiser to work, 'main_estimates.R' must first be run.") 
  }
  
  ###### GET INITIAL VALUES FROM EFS DATABASE
  # !when using getDBvalues() be aware of all the optional parameters!
  # the default values are: name_='psc_delvin', param_='COAL_DELIVERY_IN', paramkind_='INP', type_='COAL_PS', ent_='POWERSTATION'
  psc_SPinitial <- getDBvalues(param_ = 'INITIALSTOCK', paramkind_='INP')
  psc_delvin    <- getDBvalues(param_ = 'COAL_DELIVERY_IN', paramkind_='INP')
  psc_delvout   <- getDBvalues(param_ = 'COAL_DELIVERY_OUT', paramkind_ = 'RES')
  psc_burnin    <- getDBvalues(param_ = 'COAL_BURN_IN', paramkind_='INP')
  psc_burnout   <- getDBvalues(param_ = 'COAL_BURN_OUT', paramkind_ = 'RES')
  psc_SPvol     <- getDBvalues(param_ = 'STOCKPILE_VOL', paramkind_ = 'RES')
  psc_cost      <- getDBvalues(param_ = 'COSTOFSUPPLY', paramkind_='INP')
  psc_heatrate  <- getDBvalues(param_ = 'HEATRATE', paramkind_ = 'INP')
  psc_cv        <- getDBvalues(param_ = 'CV', paramkind_ = 'INP')
  
  #simulation settings
  changeSimSet(seed=10, iter=1000)
  print(paste("simulation seed =", CM_sim_settings()$SEED))
  print(paste("simulation iter =", CM_sim_settings()$ITERATIONS))
  
  ###### Use estimator's ave burnout to set delvin (baseline deliveries). Only set in the beginning, doesnt change thereafter. 
  est_ave_burnout <- read.csv(paste(paste(optEstPath, est_names[4], sep=sep_) ,".csv",sep=""), 
                              header = TRUE, sep = ",", quote = "\"", dec = ".", 
                              fill = TRUE, comment.char = "")
  est_ave_burnout[,1] <- NULL # clean dataframe
  est_ave_burnout <- apply(est_ave_burnout,2,mean)
  dv_delv_base <- est_ave_burnout
  dv_delv <- psc_template
  dv_delv[,]  <- dv_delv_base
  
  ######## DELIVERY SCENARIOS
  #   d_scen_num_months <- rep(NA,psc_tot)
  #   d_scen_num_months[iiii] <- rep(sample(1:interval_num, 1))
  
  #   d_scen_num_months <- c(6, 5, 3, 6, 8, 4, 7, 1, 8, 3, 5, 5, 7, 4)
  #   d_scen_which_months <- psc_template
  #   d_scen_which_months[,] <- 0
  #   d_scen_rand <- psc_template
  #   d_scen_rand[,] <- 0
  #   d_scen_delv <- psc_template
  #   d_scen_which_months[,] <- 0
  #     
  #   for (iiii in 1:psc_tot){
  #     d_scen_which_months[sample(1:interval_num, d_scen_num_months[iiii]), iiii] <- 1    
  #   }
  #     
  #   d_scen_rand[,] <- data.frame(replicate(psc_tot, runif(interval_num, 0.8, 1.2)))
  #   d_scen_delv[,] <- d_scen_rand * d_scen_which_months
  #   d_scen_delv[d_scen_delv==0] <- 1
  #   save(d_scen_delv, file="d_scen_delv.Rda")
  #   dv_delv <- dv_delv* d_scen_delv
  
  d_scen_delv <- psc_template
  d_scen_delv[1,] <- c(1.0000000, 1.1305066, 1.0000000, 1.1107442, 1.1491233, 1.0000000, 0.8937211, 
                       1.000000, 1.1826445, 1.0000000, 0.9042976, 1.0590593, 1.1924352, 1.0258141)
  d_scen_delv[2,] <- c(0.9432821, 1.0021594, 1.0000000, 0.8547828, 0.9669022, 1.0000000, 1.1410923, 
                       0.882669, 1.1410907, 1.0000000, 1.1522853, 1.0776141, 0.8696910, 0.9641442)
  d_scen_delv[3,] <- c(0.8184659, 0.8934392, 1.0000000, 1.0000000, 0.9617290, 1.0187876, 1.0000000, 
                       1.000000, 1.0851240, 0.9398053, 1.0000000, 1.0000000, 0.9714676, 0.8003543)
  d_scen_delv[4,] <- c(1.0130660, 1.0000000, 0.9777912, 1.1053455, 0.9359125, 1.0000000, 0.9274559, 
                       1.000000, 0.9712974, 0.9575661, 1.0000000, 1.1055342, 1.0886405, 1.0000000)
  d_scen_delv[5,] <- c(0.8984939, 1.1450149, 1.0000000, 0.9622945, 1.0296295, 0.9429449, 1.1086424, 
                       1.000000, 1.0063406, 1.0000000, 0.8273502, 0.8857251, 1.1882916, 1.0000000)
  d_scen_delv[6,] <- c(1.0000000, 1.0000000, 1.0726945, 0.9590161, 1.0434778, 1.0002753, 0.8024794, 
                       1.000000, 1.0104574, 1.0000000, 1.0000000, 1.0000000, 1.0000000, 1.0000000)
  d_scen_delv[7,] <- c(1.1689540, 0.8482176, 0.8844548, 0.8226094, 0.8470352, 1.0000000, 1.1624594, 
                       1.000000, 1.0063418, 1.0000000, 0.8854596, 0.9177667, 1.0554908, 0.8471909)
  d_scen_delv[8,] <- c(0.8811328, 1.0000000, 1.0000000, 1.0000000, 0.8483108, 0.9841560, 0.8032255, 
                       1.000000, 1.0763479, 1.0556937, 0.8374403, 1.0000000, 1.1769271, 1.0000000)
  
  
  if (d.scen==1){ #normal
    dv_delv <- dv_delv 
    
  }else if (d.scen==2){ #85%
    dv_delv <- dv_delv*0.85
    
  }else if (d.scen==3){ #115%
    dv_delv <- dv_delv*1.15
    
  }else if (d.scen==22){ #80%
    dv_delv <- dv_delv*0.8
    
  }else if (d.scen==33){ #120%
    dv_delv <- dv_delv*1.2
    
  }else if (d.scen==4){ #random
    dv_delv <- dv_delv * d_scen_delv #   +/- 20%
    
  }else if (d.scen==5){ #random
    dv_delv <- dv_delv * ((d_scen_delv-1)/0.2*0.3+1) #   +/- 30%
    
  }else if (d.scen==6){ #random
    dv_delv <- dv_delv * ((d_scen_delv-1)/0.2*0.4+1) #   +/- 40%
    
  }else if (d.scen==7){ #random
    dv_delv <- dv_delv * ((d_scen_delv-1)/0.2*0.5+1) #   +/- 50%
  }
  
  setDBvalues(values_ = dv_delv, param_ = 'COAL_DELIVERY_IN')
  psc_delvin <- getDBvalues(param_ = 'COAL_DELIVERY_IN', paramkind_='INP')
  
  
  ###### NUMBER OF DECISION VARIABLES
  numVar <- 3*psc_tot
  
  ###### SET PARAMETERS FOR CEM (CROSS ENTROPY METHOD)
  rho <- 0.2 #0<=rho<=1. Percentage of solutions to keep (elite %)
  epsNum <- 5 #Maximum error (a.k.a required accuracy)
  epsErr  <- 10 #try make it as close to zero as possible.
  alpha <- 0.75 #convergence rate, typical varies between 0.6 & 0.9
  
  ###### INITIALISE DECISION VARIABLES & SET CONSTRAINTS
  ### DELIVERY CONSTRAINTS
  llim_delv <- rep(0,psc_tot)
  ulim_delv <- rep(3000,psc_tot)
  
  ### MIN SP level
  min_SPdays <- 5 #shortage
  
  ### DV = Stockpiles: Initial(desired), UpperWarningLimit & LowerWarningLimit.
  # initialise all mus and sigmas
  mus <- rep(NA,numVar)
  sigmas <- rep(NA,numVar) 
  sigma.factor <- 5 #@@@
  
  # Calc 1 sp day average
  days.sim_range <- sum(days_in_month(dates))
  SP1day_ave <- as.numeric(apply(dv_delv,2,sum)/days.sim_range)
  
  ### INITIAL (DESIRED) SP
  # Constraints
  llim_init <- SP1day_ave*5
  ulim_init <- SP1day_ave*25
  
  # Initial decision variables
  dv_SPinitial <- SP1day_ave*20 
  mus[1:psc_tot] <- dv_SPinitial 
  sigmas[1:psc_tot] <- (ulim_init-llim_init)*sigma.factor #@@@
  
  ### LOWER WARNING LIMIT & UPPER WARNING LIMIT
  # Constraints: LWL & UWL
  llim_lower <- SP1day_ave*1
  ulim_lower <- 0.9 # It is a factor of each randomly generated initial (desired) stockpile level.
  llim_upper <- 1.1 # It is a factor of each randomly generated initial (desired) stockpile level.
  ulim_upper <- SP1day_ave*40
  
  # Decision variables: LWL
  mus[psc_tot + 1:psc_tot] <- (llim_lower + dv_SPinitial*ulim_lower) /2
  sigmas[psc_tot + 1:psc_tot] <- (dv_SPinitial-llim_lower)*sigma.factor  #@@@
  
  # Decision variables: UWL
  mus[2*psc_tot + 1:psc_tot] <- (dv_SPinitial*llim_upper + ulim_upper) /2
  sigmas[2*psc_tot + 1:psc_tot] <- (ulim_upper-dv_SPinitial)*sigma.factor  #@@@
  
  ###### LOAD OBJECTIVE FUNCTION
  source(paste(Rcode_path,"main_obj_func.R",sep=.Platform$file.sep), local=TRUE)
  
  
  
  ###### INITIALISE OTHER VALUES USED IN OPTIMSER
  elite <- as.integer(rho*N)
  mus_prev <- rep(NA,numVar)
  sigmas_prev <- rep(NA,numVar)
  x <- matrix(0,N,numVar) # population matrix
  Z_x <- matrix(0,N,1+numVar) #objective function including: x, obj func value, emer & canc deliveries
  sigma_quantile <- matrix(NA,numVar,maxIterations)
  z_quantile <- rep(NA,maxIterations)
  
  delv_emer <- psc_template
  delv_canc <- psc_template
  delv_emer[,] <- 0
  delv_canc[,] <- 0
  SPvar <- 0
  
  ###### WRITE TO RESULTS FILE
  #print parameters
  write.row(c("opt_type",func_name), append=FALSE)
  write.row(c('options.ps',options.ps_write))
  write.row(c('options.dv',options.dv))
  write.row(c('options.eval',options.eval))
  write.row(c('rho',rho))
  write.row(c('N',N))
  write.row(c('maxIterations',maxIterations))
  write.row(c('alpha',alpha))
  write.row('')
  
  #print the header for the results
  header <- c("t","seed","Z(mus)","z.h", "z.s","z.e", "z.c", "Z(quan)",
              paste('mu_in', 1:psc_tot, sep=""), #mu of initial SP
              paste('mu_lo', 1:psc_tot, sep=""), #mu of upper warning
              paste('mu_up', 1:psc_tot, sep=""), #mu of lower warning
              paste('si_in', 1:psc_tot, sep=""), #sigma of initial SP
              paste('si_lo', 1:psc_tot, sep=""), #sigma of upper warning
              paste('si_up', 1:psc_tot, sep=""), #sigma of lower warning
              paste('emer', 1:(interval_num*psc_tot), sep=""), #emergency deliveries
              paste('canc', 1:(interval_num*psc_tot), sep=""), #cancellation of deliveries
              paste('base', 1:(interval_num*psc_tot), sep=""), #baseline deliveries            
              paste('SP', 1:(interval_num*psc_tot), sep=""), #sp out
              paste('del', 1:(interval_num*psc_tot), sep=""), #delv out
              paste('bur', 1:(interval_num*psc_tot), sep="") #burn out
  )
  write.row(header)
  
  #print the initialised values (iteration = 0)
  write.row(c("0", rep("NA",times=7),
              mus,
              sigmas,
              rep(0,interval_num*psc_tot),
              rep(0,interval_num*psc_tot),
              dv_delv_base,
              rep(0,interval_num*psc_tot),
              rep(0,interval_num*psc_tot),
              rep(0,interval_num*psc_tot)
  ))
  
  #################################### MAIN LOOP ######################################
  
  # while (ifelse(t<epsNum, TRUE, !all(abs(z_quantile[t-seq(0,length=epsNum)] - z_quantile[t]) <= epsErr)) && (t < maxIterations))
  # {   # @@@
  
  ######LOAD SHEDDING
  if (load_shedding==TRUE){
    load_shedding.filenames <- paste("/",
                                     c("d22", "d5", "55", "d5", "d33", "d4", "95p", "d4","d7","d6"),
                                     sep="")
    writepath <- paste(optPath, load_shedding.filenames[ls.id], sep="")
    load_shedding.results <- read.csv(text=readLines(paste(writepath, ".csv", sep=""))[-(1:9)])
    mus <- as.numeric(load_shedding.results[nrow(load_shedding.results), 1:42 + 8])
    sigmas <- as.numeric(load_shedding.results[nrow(load_shedding.results), 1:42 + 8 +42])
    t <- as.numeric(load_shedding.results[nrow(load_shedding.results),1]) +1
  }
  
  
  while (t <= maxIterations){
    # for the smoothing function
    mus_prev <- mus
    sigmas_prev <- sigmas
    
    # generate population of random values
    for (j in 1:psc_tot){ 
      x[,j]            <- rtruncnorm(N, a=llim_init[j],  b=ulim_init[j],  mean=mus[j], sd=sigmas[j])
      x[,j + psc_tot]  <- rtruncnorm(N, a=llim_lower[j], b=ulim_lower*dv_SPinitial[j], mean=mus[j+psc_tot], sd=sigmas[j+psc_tot])
      x[,j+ 2*psc_tot] <- rtruncnorm(N, a=llim_upper*dv_SPinitial[j], b=ulim_upper[j], mean=mus[j+2*psc_tot], sd=sigmas[j+2*psc_tot])
    }
    
    # calculate the emergency/cancellation of deliveries for the x's. Also, calculate the objective function value.
    for(kkk in 1:N){
      # Analyse the current 'person' in the population
      dv_SPinitial <- x[kkk,1:psc_tot]
      dv_SPlower   <- x[kkk,1:psc_tot + psc_tot]
      dv_SPupper   <- x[kkk,1:psc_tot + 2*psc_tot]
      source(paste(Rcode_path,"main_sim.R",sep=sep_), local=TRUE) #call the simulator
      
      # only calculate emer/canc if options.dv == 3. i.e. if using LWL and UWL as decision variables.
      if (options.dv == 3){
        
        # next 2 lines must go together!! and in that order!!
        mode <- "x"
        source(paste(Rcode_path,"Calc_EmerOrCanc.R",sep=sep_), local=TRUE) #calculate emergency & cancellation deliveries
      }
      
      Z_x[kkk,]  <- c(sum(obj_func()),x[kkk,])
      
    }
    
    # sort the population from best to worst
    Z_x_sorted  <- Z_x[order(Z_x[,1]),]
    
    # ignore if statement. basically its just used to avoid a bug (when elite = 1)
    if (elite == 1){
      mus <- Z_x_sorted[1:numVar+1]
      sigmas  <- Z_x_sorted[1:elite,1:numVar+1]
    }else{
      mus <- apply(Z_x_sorted[1:elite,1:numVar+1] , 2, mean)
      sigmas  <-  apply(Z_x_sorted[1:elite,1:numVar+1] , 2, sd)
    }
    
    z_quantile[t] <- Z_x_sorted[elite]
    
    # smoothing function
    mus <- alpha*mus + (1-alpha)*mus_prev
    sigmas <- alpha*sigmas + (1-alpha)*sigmas_prev
    
    # calculate the emergency/cancellation of deliveries for the mus. Also, calculate the objective function value.
    dv_SPinitial <- mus[1:psc_tot]
    dv_SPlower   <- mus[1:psc_tot + psc_tot]
    dv_SPupper   <- mus[1:psc_tot + 2*psc_tot]
    source(paste(Rcode_path,"main_sim.R",sep=sep_), local=TRUE)
    
    # only calculate emer/canc if options.dv == 3
    if (options.dv == 3){
      
      # next 2 lines must go together!! and in that order!!
      mode <- "mu"
      source(paste(Rcode_path,"Calc_EmerOrCanc.R",sep=sep_), local=TRUE)
    }
    
    Z_mus <- sum(obj_func())
    
    psc_delvout   <- getDBvalues(param_ = 'COAL_DELIVERY_OUT', paramkind_ = 'RES')
    psc_burnout   <- getDBvalues(param_ = 'COAL_BURN_OUT', paramkind_ = 'RES')
    
    # export results of algorithm iteration (t) to .csv
    write.row(c(t, CM_sim_settings()$SEED, Z_mus,obj_func(), z_quantile[t], mus, sigmas, 
                unlist(delv_emer), unlist(delv_canc), unlist(dv_delv), 
                unlist(psc_SPvol), unlist(psc_delvout), unlist(psc_burnout)))
    
    # only store each sorted population is interested in calculating the half.width. (OTHERWISE IGNORE)
    if (option.halfwidth==TRUE){
      write.csv(Z_x_sorted, file = paste(optPath,"\\Z_x_sorted", t, ".csv",sep=''))  
    }
    
    # increment algorithm iteration counter
    t <- t+1
  }
  
  ############################ END #################################
  
  #   #did the algorithm converge or were the max iterations reached?
  #   if (ifelse(t<epsNum, TRUE, !all(abs(z_quantile[t-seq(0,length=epsNum)] - z_quantile[t]) <= epsErr))) {
  #     print("Maximum number of iterations reached. Did not converge.")
  #   }else{
  #     print(paste("Winner winner chicken dinner. Successfully converged (to ",Z_mus, ")", sep=""))
  #   }
  
  # display the time taken to run algorithm
  toc <- Sys.time() #end stopwatch
  print(toc-tic)
  
  # create a pop up dialog
  winDialog("ok", paste("OPTIMISER completed in ",round(print(toc-tic),1),units(toc-tic), sep=""))
  
}


