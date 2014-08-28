############################ INITIALISATION ################################
###### CLEAR PREVIOUS RUN 
sink()
sink()
sink()
cat("\014") #clear console
rm(list = ls()) #clear global environment

# ###### INITIAL REQUIRED SETTINGS. USED FOR THE NEXT FEW SETTINGS, ETC.  
# maxIterations <- 50
# N <- 50
# fp_set <- 1 # fp. created due to laziness. makes switching between different computers easy.
# options.ps <- 99 # 1,2,3,4,5,6,7,8,9,10,11,12,13,14, or 99. which power station to optimise? 99 = all
# options.dv <- 3 #1 or 3. how many types of decision variables? des? or des, LWL, UWL?
# options.eval <- 1 # 1 or 5
# option.halfwidth <- FALSE

###### optimiser function
optimser.csps <- function(fp_set=1, maxIterations=50, N=50, options.ps=99, options.dv=3, options.eval=1, option.halfwidth=FALSE){
  
  print(paste("fp_set", fp_set))
  print(paste("maxIterations", maxIterations))
  print(paste("N", N))
  print(paste("options.ps", options.ps))
  print(paste("options.dv", options.dv))
  print(paste("options.eval", options.eval))
  print(paste("option.halfwidth", option.halfwidth))
  
  # calculate estimated completion time
  if (options.dv == 3){
    print(paste("estimated completion time = ", round(maxIterations*N*15*8/60/60,2), " hours", sep="")) 
  }else if (options.dv == 1){
    print(paste("estimated completion time = ", round(maxIterations*N*15/60/60,2), " hours", sep="")) 
  }
  
  # begin stopwatch
  tic <- Sys.time() 
  
  ###### FILE PATHS USED IN OPTIMISER
  ## !!Adjust these paths to the folder where EFS is running!!
  ## !!First Start DIAS then Run this in RStudio!!
  if (fp_set == 0){
    Rcode_path  <- file.path("H:\\R code - Marc\\thss") #where to source Rcode
    THEPATH  <-  "C:\\Users\\17878551\\Desktop\\EFS APP"
    THEDBPATH  <-  "C:\\Users\\17878551\\Desktop\\EFS APP\\e-breadboard\\resources\\za.co.enerweb_energy-flow-simulator3-var\\dbs" 
    print(paste("computer",fp_set))
  }else if(fp_set == 1){
    Rcode_path  <- file.path("C:\\Users\\17878551\\Desktop\\EFS APP\\Rcode") #where to source Rcode
    THEPATH  <-  "C:\\Users\\17878551\\Desktop\\EFS APP"
    THEDBPATH  <-  "C:\\Users\\17878551\\Desktop\\EFS APP\\e-breadboard\\resources\\za.co.enerweb_energy-flow-simulator3-var\\dbs" 
    print(paste("computer",fp_set))
  }else if(fp_set == 2){
    Rcode_path  <- file.path("H:\\R code - Marc2") #where to source Rcode
    THEPATH  <-  "C:\\Users\\17878551\\Desktop\\EFS APP"
    THEDBPATH  <-  "C:\\Users\\17878551\\Desktop\\EFS APP\\e-breadboard\\resources\\za.co.enerweb_energy-flow-simulator3-var\\dbs" 
    print(paste("computer",fp_set))
  }else if(fp_set == 3){
    Rcode_path  <- file.path("H:\\R code - Marc3") #where to source Rcode
    THEPATH  <-  "C:\\Users\\17878551\\Desktop\\EFS APP"
    THEDBPATH  <-  "C:\\Users\\17878551\\Desktop\\EFS APP\\e-breadboard\\resources\\za.co.enerweb_energy-flow-simulator3-var\\dbs" 
  }else if(fp_set == 4){
    Rcode_path  <- file.path("H:\\R code - Marc4") #where to source Rcode
    THEPATH  <-  "C:\\Users\\17878551\\Desktop\\EFS APP"
    THEDBPATH  <-  "C:\\Users\\17878551\\Desktop\\EFS APP\\e-breadboard\\resources\\za.co.enerweb_energy-flow-simulator3-var\\dbs" 
    print(paste("computer",fp_set))
  }else if(fp_set == 5){
    Rcode_path  <- file.path("C:\\Users\\MarcHatton\\MEGA\\Postgraduate\\Thesis\\Algorithms\\R code - Marc") #where to source Rcode
    THEPATH  <-  "C:\\Users\\MarcHatton\\Desktop\\EFS APP"
    THEDBPATH  <-  "C:\\Users\\MarcHatton\\Desktop\\EFS APP\\e-breadboard\\resources\\za.co.enerweb_energy-flow-simulator3-var\\dbs" 
    print(paste("computer",fp_set))
  }
  
  print(paste("Using computer",fp_set))
  print(Rcode_path)
  print(THEPATH)
  print(THEDBPATH)
  
  if (!(exists("Rcode_path") && exists("THEPATH") && exists("THEDBPATH"))) {
    stop("For the optimiser to work, filepaths must be set!") 
  }
  
  ###### NECESSARY INITIAL VALUES USED IN "main_settings.R"
  func_name <- "CEM"
  t <- 1 # time-step counter
  
  ###### MAIN SETTINGS. MUST CHANGE FILEPATHS IF RUNNING ON A DIFFERENT COMPUTER. MUST ALSO INSTALL PACKAGES LISTED THEREIN.
  source(paste(Rcode_path,"main_settings.R",sep=.Platform$file.sep), local=TRUE)
  
  break 
  
  ###### IF OPTIONS.PS = 99 (al powerstations), then we let PS = a sequence of 1-to-psc_tot
  if (options.ps == 99){
    options.ps_write <- "all"
    options.ps <- 1:psc_tot
    print("all powerstations will be optimised")
  } else {
    print(paste("powerstation ", options.ps," (", colnames(psc_template)[options.ps], ") ",  "will be optimised", sep=""))
  }
  
  ###### LOAD OBJECTIVE FUNCTION
  source(paste(Rcode_path,"main_obj_func.R",sep=.Platform$file.sep), local=TRUE)
  
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
  
  #simulation settings
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
  
  ### DV = Stockpiles: Initial(desired), UpperWarningLimit & LowerWarningLimit.
  # initialise all mus and sigmas
  mus <- rep(NA,numVar)
  sigmas <- rep(NA,numVar) 
  sigma.factor <- 5 #@@@
  
  # Calc 1 sp day average
  SP1day_ave <- apply(psc_burnout,2,mean)/30
  
  ### INITIAL (DESIRED) SP
  # Constraints
  llim_init <- SP1day_ave*5
  ulim_init <- SP1day_ave*25
  
  # Initial decision variables
  dv_SPinitial <- SP1day_ave*15 
  mus[1:psc_tot] <- dv_SPinitial 
  sigmas[1:psc_tot] <- (ulim_init-llim_init)*sigma.factor #@@@
  
  ### LOWER WARNING LIMIT & UPPER WARNING LIMIT
  # Constraints: LWL & UWL
  llim_lower <- SP1day_ave*1
  ulim_lower <- 0.9 # It is a factor of each randomly generated initial (desired) stockpile level.
  llim_upper <- 1.1 # It is a factor of each randomly generated initial (desired) stockpile level.
  ulim_upper <- SP1day_ave*30
  
  # Decision variables: LWL
  mus[psc_tot + 1:psc_tot] <- (llim_lower + dv_SPinitial*ulim_lower) /2
  sigmas[psc_tot + 1:psc_tot] <- (dv_SPinitial-llim_lower)*sigma.factor  #@@@
  
  # Decision variables: UWL
  mus[2*psc_tot + 1:psc_tot] <- (dv_SPinitial*llim_upper + ulim_upper) /2
  sigmas[2*psc_tot + 1:psc_tot] <- (ulim_upper-dv_SPinitial)*sigma.factor  #@@@
  
  ###### INITIALISE OTHER VALUES USED IN OPTIMSER
  elite <- as.integer(rho*N)
  mus_prev <- rep(NA,numVar)
  sigmas_prev <- rep(NA,numVar)
  x <- matrix(0,N,numVar) # population matrix
  Z_x <- matrix(0,N,1+numVar) #objective function including: x, obj func value, emer & canc deliveries
  z_archive <- rep(NA,maxIterations)
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
              paste('base', 1:(interval_num*psc_tot), sep="") #baseline deliveries            
  )
  write.row(header)
  
  #print the initialised values (iteration = 0)
  write.row(c("0", rep("NA",times=7),
              mus,
              sigmas,
              rep(0,interval_num*psc_tot),
              rep(0,interval_num*psc_tot),
              dv_delv_base
  ))
  
  #################################### MAIN LOOP ######################################
  
  # while (ifelse(t<epsNum, TRUE, !all(abs(z_quantile[t-seq(0,length=epsNum)] - z_quantile[t]) <= epsErr)) && (t < maxIterations))
  # {   # @@@
  
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
    
    # export results of algorithm iteration (t) to .csv
    write.row(c(t, CM_sim_settings()$SEED, Z_mus,obj_func(), z_quantile[t], mus, sigmas, unlist(delv_emer), unlist(delv_canc), unlist(dv_delv)))
    
    # only store each sorted population is interested in calculating the half.width. (OTHERWISE IGNORE)
    if (option.halfwidth==TRUE){
      write.csv(Z_x_sorted, file = paste(optPath,"\\Z_x_sorted", t, ".csv",sep=''))  
    }
    
    # increment algorithm iteration counter
    t <- t+1
  }
  
  ############################ END #################################
  
  #did the algorithm converge or were the max iterations reached?
  if (ifelse(t<epsNum, TRUE, !all(abs(z_quantile[t-seq(0,length=epsNum)] - z_quantile[t]) <= epsErr))) {
    print("Maximum number of iterations reached. Did not converge.")
  }else{
    print(paste("Winner winner chicken dinner. Successfully converged (to ",Z_mus, ")", sep=""))
  }
  
  # display the time taken to run algorithm
  toc <- Sys.time() #end stopwatch
  print(toc-tic)
  
  # create a pop up dialog
  winDialog("ok", paste("OPTIMISER completed in ",round(print(toc-tic),1),units(toc-tic), sep=""))
  
}

optimser.csps()

optimser.csps(fp_set=1, maxIterations=2, N=2, options.ps=99, options.dv=3, options.eval=1, option.halfwidth=FALSE)
