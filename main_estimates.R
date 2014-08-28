###### CLEAR PREVIOUS RUN 
cat("\014") #same as using ctrl+"l"
rm(list = ls()) #clear global environment

maxIterations <- 1000
fp_set <- 5
print(paste("estimated completion time ", round(maxIterations*18/60/60,2), " hours.", sep="")) 

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
  Rcode_path  <- file.path("H:\\R code - Marc") #where to source Rcode
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
func_name <- "opt_estimator"
t <- 1 # time-step counter

###### MAIN SETTINGS. MUST CHANGE FILEPATHS IF RUNNING ON A DIFFERENT COMPUTER. MUST ALSO INSTALL PACKAGES LISTED THEREIN.
source(paste(Rcode_path,"main_settings.R",sep=.Platform$file.sep))

###### CHANGE INITIAL STOCK TO BE REALLY HIGH, JUST FOR THE PURPOSES OF THE ESTIMATES.
corr_form_dv_SPinitial <- psc_template #getting dv_SPinitial into the correct format for writing to the database.
corr_form_dv_SPinitial[,] <- 10000 #arbitrarily large number. I chose 10000
setDBvalues(values_ = corr_form_dv_SPinitial, param_ = 'INITIALSTOCK')

###### GET INITIAL VALUES
psc_delvin <- getDBvalues(param_ = 'COAL_DELIVERY_IN', paramkind_='INP')
psc_delvout <- getDBvalues(param_ = 'COAL_DELIVERY_OUT', paramkind_ = 'RES')
psc_burnin <- getDBvalues(param_ = 'COAL_BURN_IN', paramkind_='INP')
psc_burnout <- getDBvalues(param_ = 'COAL_BURN_OUT', paramkind_ = 'RES')

numEstVar <- prod(dim(psc_delvin)) 

###### SET DELIVERIES_IN = BURN_IN
dv_delv <- psc_burnin
setDBvalues(values_ = dv_delv, param_ = 'COAL_DELIVERY_IN')

#####SET SIMULATOR ITERATIONS TO 1000. (DEFAULT IS 10).
sim_iter_est <- 1000
changeSimSet(iter = sim_iter_est)

###### INITIALISE ESTIMATES
est_delvin  <- matrix(NA,maxIterations,prod(dim(psc_template)))
est_delvout <- matrix(NA,maxIterations,prod(dim(psc_template)))
est_burnin  <- matrix(NA,maxIterations,prod(dim(psc_template)))
est_burnout <- matrix(NA,maxIterations,prod(dim(psc_template)))

###### BEGIN MAIN LOOP
while (t <= maxIterations)
{  
  changeSimSet(seed=sim_seed[t], iter=sim_iter_est)
  source(paste(Rcode_path,"main_sim_estimates.R", sep=sep_))
  
  est_delvin[t,] <- as.vector(as.matrix(psc_delvin))
  est_delvout[t,] <- as.vector(as.matrix(psc_delvout))
  est_burnin[t,] <- as.vector(as.matrix(psc_burnin))
  est_burnout[t,] <- as.vector(as.matrix(psc_burnout))

  t <- t+1
}

write.csv(est_delvin, file = paste(optEstPath, "/", est_names[3], ".csv", sep=""))
write.csv(est_delvout, file = paste(optEstPath, "/", est_names[4], ".csv", sep=""))
write.csv(est_burnin, file = paste(optEstPath, "/", est_names[1], ".csv", sep=""))
write.csv(est_burnout, file = paste(optEstPath, "/", est_names[2], ".csv", sep=""))

###### ANALYSE RESULTS
# draw boxplots for estimates
source(paste(Rcode_path,"boxplots_estimates.R", sep=sep_))

toc <- Sys.time() #end stopwatch
print(toc-tic)
winDialog("ok", paste("ESTIMATOR completed in ",round(print(toc-tic)),units(toc-tic), sep=""))

