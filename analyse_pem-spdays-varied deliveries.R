################################################# SETTINGS
sink()
sink()
sink()
cat("\014") #clear console
rm(list = ls()) #clear global environment

###### Make sure these R packages are installed
library(lubridate)
library(ggplot2)
library(reshape2)
library(zoo)
library(reshape2)
library(extrafont)

# extrafont settings
loadfonts(quiet=TRUE)
Sys.setenv(R_GSCMD = "C:\\Program Files\\gs\\gs9.15\\bin\\gswin64c.exe") # Adjust the path to match your installation of Ghostscript

###settings
maxIterations <- 1
N <- 1
fp_set <- 5

###### START STOPWATCH
tic <- Sys.time() #begin stopwatch

###### FILE PATHS USED IN OPTIMISER
## !!Adjust these paths to the folder where EFS is running!!
## First Start DIAS then Run this in RStudio
if (fp_set == 0){
  Rcode_path  <- file.path("H:\\R code - Marc\\thss") #where to source Rcode
  THEPATH  <-  "C:\\Users\\17878551\\Desktop\\EFS APP"
  THEDBPATH  <-  "C:\\Users\\17878551\\Desktop\\EFS APP\\e-breadboard\\resources\\za.co.enerweb_energy-flow-simulator3-var\\dbs" 
  print(fp_set)
}else if(fp_set == 1){
  Rcode_path  <- file.path("C:\\Users\\17878551\\Desktop\\EFS APP\\Rcode") #where to source Rcode
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
  Rcode_path  <- file.path("C:\\Users\\MarcHatton\\Desktop\\EFS APP\\Rcode") #where to source Rcode
  THEPATH  <-  "C:\\Users\\MarcHatton\\Desktop\\EFS APP"
  THEDBPATH  <-  "C:\\Users\\MarcHatton\\Desktop\\EFS APP\\e-breadboard\\resources\\za.co.enerweb_energy-flow-simulator3-var\\dbs" 
  print(fp_set)
}else if(fp_set == 6){
  Rcode_path  <- file.path("C:\\Users\\15720314\\Desktop\\EFS APP\\Rcode") 
  THEPATH  <-  "C:\\Users\\15720314\\Desktop\\EFS APP"
  THEDBPATH  <-  "C:\\Users\\15720314\\Desktop\\EFS APP\\e-breadboard\\resources\\za.co.enerweb_energy-flow-simulator3-var\\dbs" 
}else if(fp_set == 7){
  Rcode_path  <- file.path("C:\\Users\\17090792\\Desktop\\EFS APP\\Rcode") 
  THEPATH  <-  "C:\\Users\\17090792\\Desktop\\EFS APP"
  THEDBPATH  <-  "C:\\Users\\17090792\\Desktop\\EFS APP\\e-breadboard\\resources\\za.co.enerweb_energy-flow-simulator3-var\\dbs" 
}

print(paste("Using computer",fp_set))
print(Rcode_path)
print(THEPATH)
print(THEDBPATH)

if (!(exists("Rcode_path") && exists("THEPATH") && exists("THEDBPATH"))) {
  stop("For the optimiser (and estimator) to work, filepaths must be set!") 
}


d_scen_delv <- data.frame(matrix(rep(NA,14*8), nrow=8, ncol=14))
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

results.sp_dif_long


###### set variable for the character to be used as a separator in paste(...,sep=sep_). Dependant on Operating System.
sep_ <- .Platform$file.sep #for the sake of laziness

###### Range for reading to and writing from the database
#Define the startdate and enddate range, for reading from and writing to the database
startdate <- as.POSIXct('2013-08-01')
enddate <- as.POSIXct('2014-03-01')   
interval <- 'month' #Month
if (interval=='month'){
  interval_num  <- round((as.yearmon(enddate) - as.yearmon(startdate))*12) + 1
}else if (interval=='month'){
  interval_num <- enddate - startdate + 1
}
dates <- seq(startdate,enddate,interval)

optPath <- file.path(paste(THEPATH,"CSPS_optimiser_output",sep=sep_)) 
#optimiser estimate path. Used for baseline delivery.
optEstPath <- file.path(paste(optPath, "estimates",sep=sep_))

###### create directories to store results
#create optimser directory
dir.create(optPath, showWarnings = FALSE) 
#create optimser estimate directory
dir.create(optEstPath, showWarnings = FALSE)

compare.path <- paste(optPath, "final-results", "compare-last-iter", sep=sep_)
dir.create(compare.path, showWarnings = FALSE)

dates.formatted <- format(dates, "%Y-%m")
psc_tot <- 14
# SPday.ave <- c(17.37026, 14.59808, 25.62025, 10.94664, 17.03563, 51.92641, 39.03720, 43.67920,  9.14883, 11.85286, 37.52245, 30.37924, 27.31237, 10.00592) #@

###### estimator names
est_names <- c("est_delvin", "est_delvout", "est_burnin", "est_burnout")

###### Use estimator's ave burnout to set delvin (baseline deliveries). Only set in the beginning, doesnt change thereafter. 
est_ave_burnout <- read.csv(paste(paste(optEstPath, est_names[4], sep=sep_) ,".csv",sep=""), 
                            header = TRUE, sep = ",", quote = "\"", dec = ".", 
                            fill = TRUE, comment.char = "")
est_ave_burnout[,1] <- NULL # clean dataframe
est_ave_burnout <- apply(est_ave_burnout,2,mean)
dv_delv_base <- est_ave_burnout
dv_delv <- data.frame(matrix(rep(NA, 14*8), ncol=14, nrow=8))
dv_delv[,]  <- dv_delv_base

days.sim_range <- sum(days_in_month(dates))
SPday.ave <- as.numeric(apply(dv_delv,2,sum)/days.sim_range)

dv_SPinitial <- SPday.ave*100 

############################################## MAIN

confidential=TRUE

filename <- paste("pem.csv", sep="")

graphname <- "pem-"


results <- read.csv(text=readLines(paste(optEstPath, "/", filename, sep=sep_))[-1])
results <- results[-1,] #remove first row (all the initialised values)
row.names(results) <- results[,1]
results <- results[,-2] # remove first 2 columns

ts <- nrow(results)

results <- results[,428:ncol(results)]
results["Iteration"] <- 1:ts
results.sp <- results[,c(1:112,ncol(results))]
results.delv <- results[,c(1:112 + 112,ncol(results))]
results.burn <- results[,c(1:112 +112+112,ncol(results))]

results.sp <- melt(results.sp, id.vars="Iteration")
results.delv <- melt(results.delv, id.vars="Iteration")
results.burn <- melt(results.burn, id.vars="Iteration")

if (confidential==FALSE){
  ps.names <- c("Arnot", "Camden", "Duvha", "Grootvlei", "Hendrina", "Kendal", "Komati", 
                "Kriel_OC", "Kriel_UG", "Majuba", "Matimba", "Matla", "Tutuka", "Lethabo")
}else if (confidential==TRUE){
  ps.names <- LETTERS[1:psc_tot]
}

results.sp["Powerstation"] <- rep(rep(ps.names, each=interval_num), each=ts)
results.sp["Month"] <- rep((rep(dates, each=ts)), times=psc_tot)
results.sp$variable <- NULL
results.delv["Powerstation"] <- rep(rep(ps.names, each=interval_num), each=ts)
results.delv["Month"] <- rep((rep(dates, each=ts)), times=psc_tot)
results.delv$variable <- NULL
results.burn["Powerstation"] <- rep(rep(ps.names, each=interval_num), each=ts)
results.burn["Month"] <- rep((rep(dates, each=ts)), times=psc_tot)
results.burn$variable <- NULL

results.sp <- rbind(results.sp, 
                    data.frame(Iteration=rep(1:ts, times=psc_tot),
                               Powerstation=rep(ps.names, each=ts),
                               Month=rep(rep(dates[1]-months(1), 
                                             times=ts), times=psc_tot),
                               value=rep(SPday.ave*100, each=ts)
                    ))

results.sp_wide <- dcast(results.sp, Powerstation + Iteration ~ Month)

### makes sp into days
SPday.ave_long <- rep(SPday.ave, each=ts)
results.sp_wide[, 3:11] <- results.sp_wide[, 3:11]/SPday.ave_long

### diff in sp
results.sp_wide1 <- results.sp_wide[,-c(1,2,3)]
colnames(results.sp_wide1) <- 1:8

results.sp_wide2 <- results.sp_wide[,-c(1,2,11)]
colnames(results.sp_wide2) <- 1:8

results.sp_dif <- cbind(results.sp_wide[,1:2] ,
                        results.sp_wide1 - results.sp_wide2
)
rm(results.sp_wide1)
rm(results.sp_wide2)

colnames(results.sp_dif) <- c(colnames(results.sp_wide)[1:2], 
                              dates.formatted)

### cum dif
results.sp_dif_cum <- cbind(results.sp_dif[, 1:2], 
                            results.sp_wide[,3], results.sp_dif[, -c(1,2)])
colnames(results.sp_dif_cum) <- c(colnames(results.sp_wide)[1:2],
                                  "2013-07",
                                  dates.formatted)
results.sp_dif_cum[,3] <- 0

results.sp_dif_cum[, -(1:2)] <- data.frame(t(cumsum(data.frame(t(results.sp_dif_cum[, -c(1,2)])))))
results.sp_dif_cum[,3] <- NULL

### long format for plotting
results.sp_dif_long <- melt(results.sp_dif, id.vars=c("Powerstation", "Iteration"))
results.sp_dif_long_cum <- melt(results.sp_dif_cum, id.vars=c("Powerstation", "Iteration"))





p.results.sp <- ggplot(data=results.sp_dif_long, aes(x=variable, y=value, group=Iteration)) + 
  geom_path(alpha=0.25) +
  ylab("Stockpile days") +
  xlab("Months") +
  scale_y_continuous(#limit=c(0,max(results.mus$Desired)), 
    breaks=seq(-6,6,by=2), minor_breaks =seq(-6,6,by=1)    ) +  
  #   scale_x_continuous(breaks=seq(0, 100, length.out=11)) +
  facet_wrap( ~ Powerstation, ncol=3) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.4),
        text = element_text(size=20, family="CM Roman"))

p.results.sp
ggsave(file=paste(optEstPath,"\\", graphname, "diff-sp-days-ALL.pdf",sep=""),height=14,width=10)
embed_fonts(paste(optEstPath,"\\", graphname, "diff-sp-days-ALL.pdf",sep=""))



p.results.sp <- ggplot(data=results.sp_dif_long_cum, aes(x=variable, y=value, group=Iteration)) + 
  geom_path(alpha=0.25) +
  ylab("Stockpile days") +
  xlab("Months") +
  scale_y_continuous(#limit=c(0,max(results.mus$Desired)), 
    breaks=seq(-40,40,by=10)    ) +  
  #   scale_x_continuous(breaks=seq(0, 100, length.out=11)) +
  facet_wrap( ~ Powerstation, ncol=3) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.4),
        text = element_text(size=20, family="CM Roman"))

p.results.sp
ggsave(file=paste(optEstPath,"\\", graphname, "cum-diff-sp-days-ALL.pdf",sep=""),height=14,width=10)
embed_fonts(paste(optEstPath,"\\", graphname, "cum-diff-sp-days-ALL.pdf",sep=""))
