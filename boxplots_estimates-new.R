######################################################### SETTINGS

###### CLEAR PREVIOUS RUN 
sink()
sink()
sink()
cat("\014") #clear console
rm(list = ls()) #clear global environment

###### CHOOSE A FILEPATH - DIFFERENT FOR DIFFERENT COMPUTERS
fp_set <- 5

###### FILE PATHS USED IN OPTIMISER
## !!Adjust these paths to the folder where EFS is running!!
## !!First Start DIAS then Run this in RStudio!!
if (fp_set == 0){
  Rcode_path  <- file.path("H:\\R code - Marc\\thss") 
  THEPATH  <-  "C:\\Users\\17878551\\Desktop\\EFS APP"
  THEDBPATH  <-  "C:\\Users\\17878551\\Desktop\\EFS APP\\e-breadboard\\resources\\za.co.enerweb_energy-flow-simulator3-var\\dbs" 
}else if(fp_set == 1){
  Rcode_path  <- file.path("C:\\Users\\17878551\\Desktop\\EFS APP\\Rcode") 
  THEPATH  <-  "C:\\Users\\17878551\\Desktop\\EFS APP"
  THEDBPATH  <-  "C:\\Users\\17878551\\Desktop\\EFS APP\\e-breadboard\\resources\\za.co.enerweb_energy-flow-simulator3-var\\dbs" 
}else if(fp_set == 2){
  Rcode_path  <- file.path("H:\\R code - Marc2") 
  THEPATH  <-  "C:\\Users\\17878551\\Desktop\\EFS APP"
  THEDBPATH  <-  "C:\\Users\\17878551\\Desktop\\EFS APP\\e-breadboard\\resources\\za.co.enerweb_energy-flow-simulator3-var\\dbs" 
}else if(fp_set == 3){
  Rcode_path  <- file.path("H:\\R code - Marc3") 
  THEPATH  <-  "C:\\Users\\17878551\\Desktop\\EFS APP"
  THEDBPATH  <-  "C:\\Users\\17878551\\Desktop\\EFS APP\\e-breadboard\\resources\\za.co.enerweb_energy-flow-simulator3-var\\dbs" 
}else if(fp_set == 4){
  Rcode_path  <- file.path("H:\\R code - Marc4") 
  THEPATH  <-  "C:\\Users\\17878551\\Desktop\\EFS APP"
  THEDBPATH  <-  "C:\\Users\\17878551\\Desktop\\EFS APP\\e-breadboard\\resources\\za.co.enerweb_energy-flow-simulator3-var\\dbs" 
}else if(fp_set == 5){
  Rcode_path  <- file.path("C:\\Users\\MarcHatton\\Desktop\\EFS APP\\Rcode") 
  THEPATH  <-  "C:\\Users\\MarcHatton\\Desktop\\EFS APP"
  THEDBPATH  <-  "C:\\Users\\MarcHatton\\Desktop\\EFS APP\\e-breadboard\\resources\\za.co.enerweb_energy-flow-simulator3-var\\dbs" 
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


###### Make sure these R packages are installed
library(lubridate)
library(ggplot2)
library(reshape2)
library(zoo)
library(extrafont)

# extrafont settings
loadfonts(quiet=TRUE)
Sys.setenv(R_GSCMD = "C:\\Program Files\\gs\\gs9.15\\bin\\gswin64c.exe") # Adjust the path to match your installation of Ghostscript

#colour blind palette
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


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

###### estimator names
est_names <- c("est_delvin", "est_delvout", "est_burnin", "est_burnout")

psc_tot <- 14

###################################################################### USED TO CREATE BOX PLOTS - BOX PLOTS SHOW VARIATION IN THE ESTIMATES

confidential <- T

### READ FILES TO DATA.FRAMES

est_delvin <- read.csv(paste(paste(optEstPath, est_names[1], sep=sep_) ,".csv",sep=""), 
                       header = TRUE, sep = ",", quote = "\"", dec = ".", 
                       fill = TRUE, comment.char = "")
est_delvin[,1] <- NULL # clean dataframe

est_delvout <- read.csv(paste(paste(optEstPath, est_names[2], sep=sep_) ,".csv",sep=""), 
                        header = TRUE, sep = ",", quote = "\"", dec = ".", 
                        fill = TRUE, comment.char = "")
est_delvout[,1] <- NULL # clean dataframe

est_burnin <- read.csv(paste(paste(optEstPath, est_names[3], sep=sep_) ,".csv",sep=""), 
                       header = TRUE, sep = ",", quote = "\"", dec = ".", 
                       fill = TRUE, comment.char = "")
est_burnin[,1] <- NULL # clean dataframe

est_burnout <- read.csv(paste(paste(optEstPath, est_names[4], sep=sep_) ,".csv",sep=""), 
                        header = TRUE, sep = ",", quote = "\"", dec = ".", 
                        fill = TRUE, comment.char = "")
est_burnout[,1] <- NULL # clean dataframe

est_dif <- est_delvout - est_burnout
est_delv_dif <- est_delvin - est_delvout
est_burn_dif <- est_burnin - est_burnout
# apply(est_dif,2,summary)



### NAMES FOR BOX PLOT
if (confidential==FALSE){
  ps_names <- c("Arnot", "Camden", "Duvha", "Grootvlei", "Hendrina", "Kendal", "Komati", 
                "Kriel_OC", "Kriel_UG", "Majuba", "Matimba", "Matla", "Tutuka", "Lethabo")
}else if (confidential==TRUE){
  ps_names <- LETTERS[1:psc_tot]
}

bp_dates <- format(dates, "%Y-%m")
bp_dates2 <- substr(month(dates, label = T),1,4)




variation.anal <- function(type="both"){ #a=all, d=delv, b=burn
  cnames <- c("Burn (stockpile days)", "Deliveries (stockpile days)", "Deliveries - burn (stockpile days)")
  
  ylab <- "Stockpile days"
  
  if (type=="burn"){
    m1 <- est_burnin
    m2 <- est_burnout
    by_ <- 2
    by2_ <- 100
    ylab2 <- "Burn (ktons)"
    #   ylab <- cnames[1]
  }else if(type=="delv"){
    m1 <- est_delvin
    m2 <- est_delvout
    by_ <- 1
    ylab2 <- "Deliveries (ktons)"
    by2_ <- 100
    #   ylab <- cnames[2]
  }else if(type=="both"){
    m1 <- est_delvout
    m2 <- est_burnout
    by_ <- 2
    ylab2 <- "Deliveries - burn (ktons)"
    by2_ <- 50
    #   ylab <- cnames[3]
  }
  
  n.estimates <- nrow(m1)
  
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
  
  
  SPday.df <- m1
  SPday.df[,] <- rep(SPday.ave, each=n.estimates)
  
  if (type=="both"){
    variation_value <- melt(m1-m2)$value 
  }else {
    variation_value <- melt(m2)$value
  }
  
  
  variation.df <- data.frame(Powerstation = rep( rep(ps_names, each=length(bp_dates)) 
                                                 , each=n.estimates),
                             Month = rep( rep(bp_dates, psc_tot),
                                          , each=n.estimates),
                             value = variation_value)
 
  variation.df_spdays <- data.frame(Powerstation = rep( rep(ps_names, each=length(bp_dates)) 
                                                        , each=n.estimates),
                                    Month = rep( rep(bp_dates, psc_tot),
                                                 , each=n.estimates),
                                    value = melt(m1-m2)$value / 
                                      rep( rep(SPday.ave, each=length(bp_dates)) , each=n.estimates))
  
  if (type=="both"){
    seq_ <- c(rev(-seq(0, round(-min(variation.df$value), -1), by=by2_)[-1]),
            seq(0, round(max(variation.df$value), -1), by=by2_)) 
  }else{
    seq_ <- seq(0,
                round(max(variation.df$value), -1),
                by=by2_)
  }
    
    
  p1a <- ggplot(variation.df_spdays, aes(x=Powerstation, y=value, fill=Month)) +
    geom_boxplot(outlier.size = 0.75, size=0.1, width=0.7, position=position_dodge(0.8)) +
    scale_fill_manual(values=cbPalette) +
    theme_bw() +
    ylab(ylab) +
    xlab("Power station") +
    theme(#axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.4),
      text = element_text(size=20, family="CM Roman")) +
      #panel.grid.minor = element_line(colour="#e9e9e9",size=0.1)) +
    scale_y_continuous(breaks=seq(-32,32,by=by_))
                       #minor_breaks=seq(-32,32,by=(by_/2)))
  
  p1a
  ggsave(file=paste(optEstPath, "/", "one-", type, ".pdf", sep=""),height=10,width=15)
  embed_fonts(paste(optEstPath, "/", "one-", type, ".pdf", sep=""))

  
  p1b <- ggplot(variation.df[variation.df$Powerstation==LETTERS[11:14],], aes(x=Powerstation, y=value, fill=Month)) +
    geom_boxplot(outlier.size = 1.5, size=0.3, width=0.7, position=position_dodge(0.8)) +
    scale_fill_manual(values=cbPalette) +
    theme_bw() +
    ylab(ylab2) +
    xlab("Power station") +
    theme(#axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.4),
      text = element_text(size=20, family="CM Roman")) +
      #panel.grid.minor = element_line(colour="#e9e9e9",size=0.1)) +
    scale_y_continuous(breaks=seq_#,minor_breaks=seq(-32,32,by=(by_/2))) 
                       )
  
  p1b
  ggsave(file=paste(optEstPath, "/", "one-four-", type, ".pdf", sep=""),height=6,width=10)
  embed_fonts(paste(optEstPath, "/", "one-four-", type, ".pdf", sep=""))
  
  
  p2 <- ggplot(variation.df_spdays, aes(x=Month, y=value)) +
    geom_boxplot(outlier.size=1.5) +
    facet_wrap( ~ Powerstation, ncol=3) + #, scales = "free_x"
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.4),
          text = element_text(size=20, family="CM Roman")) +
    ylab(ylab)
  p2
  ggsave(file=paste(optEstPath, "/", "facet-", type, ".pdf", sep=""),height=14,width=10)
  embed_fonts(paste(optEstPath, "/", "facet-", type, ".pdf", sep=""))
  
}

variation.anal("both")
variation.anal("burn")
variation.anal("delv")







