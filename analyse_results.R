require(ggplot2)
require(reshape2)
require(lubridate)

###### CLEAR PREVIOUS RUN 
#2014 06 25
sink()
sink()
sink()
cat("\014") #same as using ctrl+"l"
rm(list = ls()) #clear global environment

maxIterations
ations <- 40
N <- 25
fp_set <- 1
print(paste("estimated completion time = ", round(maxIterations*N*18*8/60/60,2), " hours", sep="")) 

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
func_name <- "CEM"
t <- 1 # time-step counter

###### MAIN SETTINGS. MUST CHANGE FILEPATHS IF RUNNING ON A DIFFERENT COMPUTER. MUST ALSO INSTALL PACKAGES LISTED THEREIN.
source(paste(Rcode_path,"main_settings.R",sep=.Platform$file.sep))

###### results - analyse
results <- read.csv(text=readLines(paste(optPath, "/CEM_results.csv", sep=sep_))[-(1:9)])
results <- results[-1,] #remove first row
row.names(results) <- results[,1]
results <- results[,-2] # remove first 2 columns

ts <- results[nrow(results), 1]


results.costs <- results[, c(1, 2:7)]

results.mus <- data.frame(t=rep(results[,1],psc_tot))
results.mus["Desired"] <- melt(results[, c(1:14 +7)])[,2]
results.mus["LWL"] <- melt(results[, c(1:14 +7+14)])[,2]
results.mus["UWL"] <- melt(results[, c(1:14 +7+14+14)])[,2]
results.mus["Powerstation"] <- rep(colnames(psc_template), 
                                   each=ts)

results.sigmas <- data.frame(t=rep(results[,1],psc_tot))
results.sigmas["Desired"] <- melt(results[, c(1:14 +7+42)])[,2]
results.sigmas["LWL"] <- melt(results[, c(1:14 +7+14+42)])[,2]
results.sigmas["UWL"] <- melt(results[, c(1:14 +7+14+14+42)])[,2]
results.sigmas["Powerstation"] <- rep(colnames(psc_template), 
                                   each=ts)

head(results.mus)
p.mus.all <- ggplot(data=results.mus[, -c(3,4)], aes(x=t, y=Desired, colour=Powerstation)) + 
  geom_line() +
  geom_point()
p.mus.all


head(results.sigmas)
p.sigmas.all <- ggplot(data=results.sigmas[, -c(3,4)], aes(x=t, y=Desired, colour=Powerstation)) + 
  geom_line() +
  geom_point()
p.sigmas.all


ps_chosen <- colnames(psc_template)[1:2]
results.mus.chosen <- results.mus[results.mus[,5]==ps_chosen, ]
head(results.mus.chosen)
p.mus.chosen <- ggplot(data=results.mus.chosen, aes(x=t, y=Desired, ymin=LWL,
                                                    ymax=UWL, fill=Powerstation, linetype=Powerstation, color=Powerstation)) + 
  geom_line() +
  geom_point() +
  geom_ribbon(alpha=0.2)
p.mus.chosen


results.costs <- melt(results.costs, id.vars=t)

p.costs <- ggplot(data=results.costs, aes(x=t, y=value, colour=variable)) + 
  geom_line() +
  geom_point()
p.costs


