pem.csps <- function(maxIterations=100, out.stat=3){
  
  ############################ INITIALISATION ################################
  # begin stopwatch
  tic <- Sys.time() 
  
  ###### NECESSARY INITIAL VALUES USED IN "main_settings.R"
  func_name <- "pem"
  t <- 1 # time-step counter
  
  ###### MAIN SETTINGS. MUST CHANGE FILEPATHS IF RUNNING ON A DIFFERENT COMPUTER. MUST ALSO INSTALL PACKAGES LISTED THEREIN.
  source(paste(Rcode_path,"main_settings.R",sep=.Platform$file.sep), local=TRUE)
  
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
  
  #simulation settings
  changeSimSet(seed=555, iter=1000)
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
  
  mus <- rep(NA,numVar)
  sigmas <- rep(NA,numVar) 
  SP1day_ave <- apply(psc_burnout,2,mean)/30
  
  dv_SPinitial <- SP1day_ave*100 
  mus[1:psc_tot] <- dv_SPinitial
  mus[-(1:psc_tot)] <- 0
  sigmas[] <- 0
  
  corr_form_dv_SPinitial <- psc_template #getting dv_SPinitial into the correct format for writing to the database.
  corr_form_dv_SPinitial[,] <- 0
  corr_form_dv_SPinitial[1,] <- dv_SPinitial
  setDBvalues(values_ = corr_form_dv_SPinitial, param_ = 'INITIALSTOCK')
  
  delv_emer <- psc_template
  delv_canc <- psc_template
  delv_emer[,] <- 0
  delv_canc[,] <- 0
  
  write.row(c("opt_type",func_name), append=FALSE)
  
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
  
  while (t <= maxIterations){
    changeSimSet(seed=round(runif(1)*10000), iter=1000)
    print(paste("simulation seed =", CM_sim_settings()$SEED))
    source(paste(Rcode_path,"RunCSPS.R",sep=sep_), local=TRUE)
    psc_delvout   <- getDBvalues(param_ = 'COAL_DELIVERY_OUT', paramkind_ = 'RES')
    psc_burnout   <- getDBvalues(param_ = 'COAL_BURN_OUT', paramkind_ = 'RES')
    psc_SPvol     <- getDBvalues(param_ = 'STOCKPILE_VOL', paramkind_ = 'RES')
    
    # export results of algorithm iteration (t) to .csv
    write.row(c(t, CM_sim_settings()$SEED, 0, rep(0,times=4), 0, mus, sigmas, 
                unlist(delv_emer), unlist(delv_canc), unlist(dv_delv), 
                unlist(psc_SPvol), unlist(psc_delvout), unlist(psc_burnout)))
    
    t <- t+1
  }
  
  toc <- Sys.time() #end stopwatch
  print(toc-tic)
  
  # create a pop up dialog
  winDialog("ok", paste("PEM completed in ",round(print(toc-tic),1),units(toc-tic), sep=""))
}


