################################################# SETTINGS
sink()
sink()
sink()
cat("\014") #clear console
rm(list = ls()) #clear global environment

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


###### Make sure these R packages are installed
library(lubridate)
library(ggplot2)
library(reshape2)
library(zoo)


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


if (!(exists("Rcode_path") && exists("THEPATH") && exists("THEDBPATH"))) {
  stop("For the optimiser (and estimator) to work, filepaths must be set!") 
}

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

############################################################ MAIN


ps.names <- LETTERS[1:psc_tot]


Analyse.Results <- function(res.choose=9, confidential=TRUE){
  
  sensivity.costs <- data.frame(ec=c(rep(c(1.75,0.75),4) , rep(1.25,4)),
                                cc=c(rep(c(0.8,0.2),times=2, each=2) , rep(0.5,4)),
                                hc=c(rep(c(0.0875,0.0775), each=4) , rep(0.0825,4))
  ) 
  
  experiments <- c(paste("s",1:8,sep=""),
                   "b1","b2",
                   "5p","95p")
  filename <- paste(experiments[res.choose], ".csv", sep="")
  
  FRpath <- paste(optPath, "final-results", experiments[res.choose], sep=sep_)
  
  title_ <- paste("ec=", sensivity.costs$ec[res.choose], 
                  ". cc=", sensivity.costs$cc[res.choose], 
                  ". hc=", sensivity.costs$hc[res.choose],
                  ".", sep="")
  graphname <- paste(experiments[res.choose], "-",sep="")
  
  parameters_ <- read.csv(text=readLines(paste(FRpath, "/", filename, sep=sep_))[1:8], header=F)[,1:2]
  options.ps2 <- as.vector(parameters_$V2)[2]
  options.ps2 <- ifelse(options.ps2=="all", options.ps2, as.numeric(options.ps2))
  options.dv2 <- as.numeric(as.vector(parameters_$V2)[3])
  options.eval2 <- as.numeric(as.vector(parameters_$V2)[4])
  
  results <- read.csv(text=readLines(paste(FRpath, "/", filename, sep=sep_))[-(1:9)])
  results <- results[-1,] #remove first row (all the initialised values)
  row.names(results) <- results[,1]
  results <- results[,-2] # remove first 2 columns
  
  results.costs <- cbind(results[, 1], results[, 2:7]/1000000)
  colnames(results.costs) <- c("Iteration", "Overall.mu", "Holding", 
                               "Shortage", "Emergency", "Cancellation", "Overall.quantile")
  
  if (options.dv2==1){
    results.costs <- results.costs[, -c(5,6)]
  }
  
  # dec.var <- ifelse(options.dv2==1, "DES", 
  #                   ifelse(options.dv2==3, "LWL, DES & UWL", NA))
  ts <- results[nrow(results), 1]
  
  if (confidential==FALSE){
    ps.names <- c("Arnot", "Camden", "Duvha", "Grootvlei", "Hendrina", "Kendal", "Komati", 
                  "Kriel_OC", "Kriel_UG", "Majuba", "Matimba", "Matla", "Tutuka", "Lethabo")
  }else if (confidential==TRUE){
    ps.names <- LETTERS[1:psc_tot]
  }
  
  SPday.ave_long <- rep(SPday.ave, each=ts)
  SPday.ave_medium <- rep(SPday.ave, each=interval_num)
  
  
  results.mus <- data.frame(Iteration=rep(results[,1],psc_tot))
  results.mus["Desired"] <- melt(results[, c(1:14 +7)], id.vars=)[,2]
  results.mus["LWL"] <- melt(results[, c(1:14 +7+14)])[,2]
  results.mus["UWL"] <- melt(results[, c(1:14 +7+14+14)])[,2]
  results.mus["Powerstation"] <- rep(ps.names, each=ts)
  results.mus[, 2:4] <- results.mus[, 2:4]/SPday.ave_long
  
  results.sigmas <- data.frame(Iteration=rep(results[,1],psc_tot))
  results.sigmas["Desired"] <- melt(results[, c(1:14 +7+42)])[,2]
  results.sigmas["LWL"] <- melt(results[, c(1:14 +7+14+42)])[,2]
  results.sigmas["UWL"] <- melt(results[, c(1:14 +7+14+14+42)])[,2]
  results.sigmas["Powerstation"] <- rep(ps.names, each=ts)
  results.sigmas[, 2:4] <- results.sigmas[, 2:4]/SPday.ave_long
  
  results.emer <- data.frame(Iteration=results[,1])
  for (i in 1:14){
    results.emer[ps.names[i]] <- rowSums(results[ ,c(7+42+42 + ((i-1)*8 + 1):(i*8)) ])
  }
  
  results.canc <- data.frame(Iteration=results[,1])
  for (i in 1:14){
    results.canc[ps.names[i]] <- rowSums(results[ ,c(7+42+42 + 14*8 + ((i-1)*8 + 1):(i*8)) ])
  } 
  
  
  results.emercanc_last <- data.frame(Powerstation = rep(ps.names, each=8),
                                      Month = rep(dates.formatted, 14),
                                      Emer = as.numeric(results[nrow(results) ,c(7+42+42 + 1:(14*8)) ])  /SPday.ave_medium ,
                                      Canc = as.numeric(results[nrow(results) ,c(7+42+42 + 14*8 + 1:(14*8)) ])   /SPday.ave_medium)
  
  
  
  #only use specified functions
  if (options.ps2!="all"){
    results.mus <- results.mus[results.mus$Powerstation==ps.names[options.ps2], ]
    results.sigmas <- results.sigmas[results.sigmas$Powerstation==ps.names[options.ps2], ]
  }
  
  ###mus
  head(results.mus)
  
  ##des
  p.mus.des <- ggplot(data=results.mus[, -c(3,4)], aes(x=Iteration, y=Desired, colour=Powerstation)) + 
    geom_line(size=0.25) +
    geom_point(aes(shape=Powerstation),
               fill = "white",    
               size = 2.5)  +       
    scale_shape_manual(values=(1:psc_tot -1)) +
    ylab("Stockpile days") +
    xlab("Iterations") +
    scale_y_continuous(#limit=c(0,max(results.mus$Desired)), 
      breaks=seq(0, max(results.mus$Desired), by=2)) +  
    scale_x_continuous(breaks=seq(0, 100, length.out=11)) +
    theme_bw() +
    theme(text = element_text(size=15))
  p.mus.des
  ggsave(file=paste(FRpath,"\\", graphname, "mus-des.pdf",sep=""),height=6,width=10)
  
  ##lwl
  p.mus.lwl <- ggplot(data=results.mus[, -c(2,4)], aes(x=Iteration, y=LWL, colour=Powerstation)) + 
    geom_line() +
    geom_point(aes(shape=Powerstation),
               fill = "white",    
               size = 2.5)  +       
    scale_shape_manual(values=(1:psc_tot -1)) +
    ylab("Stockpile days") +
    xlab("Iterations") +
    scale_y_continuous(#limit=c(0,max(results.mus$LWL)), 
      breaks=seq(0, max(results.mus$LWL), by=2)) +  
    scale_x_continuous(breaks=seq(0, 100, length.out=11)) +
    theme_bw() +
    theme(text = element_text(size=15))
  p.mus.lwl
  ggsave(file=paste(FRpath,"\\", graphname, "mus-lwl.pdf",sep=""),height=6,width=10)
  
  
  ##uwl
  p.mus.uwl <- ggplot(data=results.mus[, -c(2,3)], aes(x=Iteration, y=UWL, colour=Powerstation)) + 
    geom_line() +
    geom_point(aes(shape=Powerstation),
               fill = "white",    
               size = 2.5)  +       
    scale_shape_manual(values=(1:psc_tot -1)) +
    ylab("Stockpile days") +
    xlab("Iterations") +
    scale_y_continuous(#limit=c(0,max(results.mus$UWL)), 
      breaks=seq(0, max(results.mus$UWL), by=2)) +  
    scale_x_continuous(breaks=seq(0, 100, length.out=11)) +
    theme_bw()+
    theme(text = element_text(size=15))
  p.mus.uwl
  ggsave(file=paste(FRpath,"\\", graphname, "mus-uwl.pdf",sep=""),height=6,width=10)
  
  
  ###sigmas
  head(results.sigmas)
  
  ##des
  p.sigmas.des <- ggplot(data=results.sigmas[, -c(3,4)], aes(x=Iteration, y=Desired, colour=Powerstation)) + 
    geom_line() +
    geom_point(aes(shape=Powerstation),
               fill = "white",    
               size = 2.5)  +       
    scale_shape_manual(values=(1:psc_tot -1)) +
    ylab("Stockpile days") +
    xlab("Iterations") +
    scale_y_continuous(limit=c(0,max(results.sigmas$Desired)), 
                       breaks=seq(0, max(results.sigmas$Desired), by=2)) +  
    scale_x_continuous(breaks=seq(0, 100, length.out=11)) +
    theme_bw()+
    theme(text = element_text(size=15))
  p.sigmas.des
  ggsave(file=paste(FRpath,"\\", graphname, "sigmas-des.pdf",sep=""),height=6,width=10)
  
  ##lwl
  p.sigmas.lwl <- ggplot(data=results.sigmas[, -c(2,4)], aes(x=Iteration, y=LWL, colour=Powerstation)) + 
    geom_line() +
    geom_point(aes(shape=Powerstation),
               fill = "white",    
               size = 2.5)  +       
    scale_shape_manual(values=(1:psc_tot -1)) +
    ylab("Stockpile days") +
    xlab("Iterations") +
    scale_y_continuous(limit=c(0,max(results.sigmas$LWL)), 
                       breaks=seq(0, max(results.sigmas$LWL), by=2)) +  
    scale_x_continuous(breaks=seq(0, 100, length.out=11)) +
    theme_bw()+
    theme(text = element_text(size=15))
  p.sigmas.lwl
  ggsave(file=paste(FRpath,"\\", graphname, "sigmas-lwl.pdf",sep=""),height=6,width=10)
  
  
  ##uwl
  p.sigmas.uwl <- ggplot(data=results.sigmas[, -c(2,3)], aes(x=Iteration, y=UWL, colour=Powerstation)) + 
    geom_line() +
    geom_point(aes(shape=Powerstation),
               fill = "white",    
               size = 2.5)  +       
    scale_shape_manual(values=(1:psc_tot -1)) +
    ylab("Stockpile days") +
    xlab("Iterations") +
    scale_y_continuous(limit=c(0,max(results.sigmas$UWL)), 
                       breaks=seq(0, max(results.sigmas$UWL), by=2)) +  
    scale_x_continuous(breaks=seq(0, 100, length.out=11)) +
    theme_bw()+
    theme(text = element_text(size=15))
  p.sigmas.uwl
  ggsave(file=paste(FRpath,"\\", graphname, "sigmas-uwl.pdf",sep=""),height=6,width=10)
  
  
  
  ###plot stockpile ribbon function
  Stockpile.Ribbon <- function(ps_chosen=1){
    # #create custom palette
    #myColors <- colorRampPalette(brewer.pal(9,"Set1"))(psc_tot)
    #names(myColors) <- levels(ps.names)
    #colScale <- scale_colour_manual(name = "Powerstation",values = myColors)
    
    ps_chosen.name <- ps.names[ps_chosen]
    results.mus.chosen <- results.mus[results.mus[,5]==ps_chosen.name, ]
    head(results.mus)
    
    p.mus.chosen <- ggplot(data=results.mus.chosen, aes(x=Iteration, y=Desired, ymin=LWL, ymax=UWL)) + 
      geom_line() +
      geom_ribbon(alpha=0.6) +
      geom_point(fill = "white",    
                 size = 2.5)  +       
      scale_shape_manual(values=(1:psc_tot -1)) + 
      ylab("Stockpile days") +
      xlab("Iterations") +
      scale_y_continuous(limit=c(0,max(results.mus$UWL)), 
                         breaks=seq(0, max(results.mus$UWL), by=2)) +  
      scale_x_continuous(breaks=seq(0, 100, length.out=11)) +
      theme_bw()+
      theme(text = element_text(size=15))
    
    p.mus.chosen
    ggsave(file=paste(FRpath,"\\", graphname, "sp-ribbon-", ps_chosen.name, ".pdf",sep=""),height=6,width=10)
  }
  
  ###loop through stockpile ribbon function
  if ((options.ps2>0) && (options.ps2<=psc_tot)){
    Stockpile.Ribbon(options.ps2)
  }else if(options.ps2=="all"){
    draw.count <- 0
    while (draw.count<psc_tot){
      draw.count <- draw.count+1
      Stockpile.Ribbon(draw.count)
    }
  }
  
  # facet ribbon
  p.mus.ribbon.all <- ggplot(data=results.mus, aes(x=Iteration, y=Desired, ymin=LWL, ymax=UWL)) + 
    geom_line() +
    geom_ribbon(alpha=0.25) +
    #     geom_point(fill = "white",    
    #                size = 2.5)  +       
    scale_shape_manual(values=(1:psc_tot -1)) + 
    ylab("Stockpile days") +
    xlab("Iterations") +
    scale_y_continuous(limit=c(0,max(results.mus$UWL)), 
                       breaks=seq(0, max(results.mus$UWL), by=4)) +  
    scale_x_continuous(breaks=seq(0, 100, length.out=11)) +
    facet_wrap( ~ Powerstation, ncol=3) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.4), 
          text = element_text(size=15))  
  
  p.mus.ribbon.all
  ggsave(file=paste(FRpath,"\\", graphname, "sp-ribbon-all.pdf",sep=""),height=14,width=10)
  
  
  #results costs
  results.costs <- melt(results.costs, id.vars="Iteration")
  colnames(results.costs) <- c("Iteration", "Cost", "value")
  
  p.costs.all <- ggplot(data=results.costs, aes(x=Iteration, y=value, colour=Cost)) + 
    geom_line() +
    geom_point(aes(shape=Cost),
               fill = "white",    
               size = 2.5)  +       
    scale_shape_manual(values=(1:length(levels(results.costs$Cost)) -1)) +
    ylab("Rands (x10 ^6)") +
    xlab("Iterations") +
    scale_y_continuous(limit=c(0,max(results.costs$value)), 
                       breaks=seq(0, max(results.costs$value), by=100)) +  
    scale_x_continuous(breaks=seq(0, 100, length.out=11)) +
    theme_bw()+
    theme(text = element_text(size=15))
  p.costs.all
  ggsave(file=paste(FRpath,"\\", graphname, "costs-all.pdf",sep=""),height=6,width=10)
  
  p.costs.quan <- ggplot(data=results.costs[results.costs["Cost"]=="Overall.quantile",], aes(x=Iteration, y=value)) + 
    geom_line() +
    geom_point() +
    ylab("Rands (x10 ^6)") +
    xlab("Iterations") +
    scale_y_continuous(#limit=c(0,max(results.costs$value)), 
      breaks=seq(0, max(results.costs$value), by=100)) +  
    scale_x_continuous(breaks=seq(0, 100, length.out=11)) +
    theme_bw()+
    theme(text = element_text(size=15))
  p.costs.quan
  ggsave(file=paste(FRpath,"\\", graphname, "costs-Zquan.pdf",sep=""),height=6,width=10)
  
  #@@
  p.costs.mu <- ggplot(data=results.costs[results.costs["Cost"]=="Overall.mu",], aes(x=Iteration, y=value)) + 
    geom_line() +
    geom_point(size=3) +
    xlab("Iteration") +
    ylab("Rands (x10 ^6)") +
    xlab("Iterations") +
    scale_y_continuous(limit=c(0,max(results.costs$value)), 
                       breaks=seq(0, max(results.costs$value), by=100)) +  
    scale_x_continuous(breaks=seq(0, 100, length.out=11)) +
    theme_bw()+
    theme(text = element_text(size=15))
  p.costs.mu
  ggsave(file=paste(FRpath,"\\", graphname, "costs-Zmu.pdf",sep=""),height=6,width=10)
  
  ## plot last iteration
  iter.last <- max(results.mus$Iteration)
  results_SPdays.iter.last <- results.mus[results.mus$Iteration==iter.last, ]
  
  p.mus.chosen <- ggplot(results_SPdays.iter.last, aes(x=Powerstation)) + 
    geom_boxplot(aes(lower=LWL, 
                     upper=UWL, 
                     middle=Desired,
                     ymin=LWL,
                     ymax=UWL), stat="identity", width=0.6) +
    xlab("Power stations") + 
    ylab("Stockpile days") +
    scale_y_continuous(limit=c(0,max(results_SPdays.iter.last$UWL)), 
                       breaks=seq(0, max(results_SPdays.iter.last$UWL), by=2)) + 
    theme_bw() +
    theme(#axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.4),
          text = element_text(size=15))   
  p.mus.chosen
  ggsave(file=paste(FRpath,"\\", graphname, "final-results", ".pdf",sep=""),height=6,width=10)
  ggsave(file=paste(compare.path,"\\", "final-results-", graphname, ".pdf",sep=""),height=6,width=10)
  
  
  p.results.emer <- ggplot(results.emercanc_last[, c(1,2,3)], aes(x=Month, y=Emer)) + 
    geom_bar(colour="black", stat="identity") +
    facet_wrap( ~ Powerstation, ncol=5) +
    ylab("Stockpile days") +
    xlab("Months") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.4),
          text = element_text(size=15))
  p.results.emer
  ggsave(file=paste(FRpath,"\\", graphname, "final-emer", ".pdf",sep=""),height=6,width=10)
  ggsave(file=paste(compare.path,"\\", "final-emer-", graphname, ".pdf",sep=""),height=14,width=10)
  
  p.results.canc <- ggplot(results.emercanc_last[, c(1,2,4)], aes(x=Month, y=Canc)) + 
    geom_bar(colour="black", stat="identity") +
    facet_wrap( ~ Powerstation, ncol=5) +
    ylab("Stockpile days") +
    xlab("Months") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.4),
          text = element_text(size=15))
  p.results.canc
  ggsave(file=paste(FRpath,"\\", graphname, "final-canc", ".pdf",sep=""),height=6,width=10)
  ggsave(file=paste(compare.path,"\\", "final-canc-", graphname, ".pdf",sep=""),height=14,width=10)
  
}

Analyse.Results(9)
# 
# Analyse.Results(1)
# Analyse.Results(2)
# Analyse.Results(3)
# Analyse.Results(4)
# Analyse.Results(5)
# Analyse.Results(6)
# Analyse.Results(7)
# Analyse.Results(8)
# Analyse.Results(9)
# Analyse.Results(10)


# Analyse.Results(11)
# Analyse.Results(12)


######################################## SENSITIVITY

##### experiments

experiments <- c(paste("s",1:8,sep=""),
                 "b1","b2",
                 "5p","95p")

sensitivity.results <- data.frame(t=rep(NA,8),
                                  Cost=rep(NA,8))

for (i in 1:8){
  filename <- paste(experiments[i], ".csv", sep="")
  FRpath <- paste(optPath, "final-results", experiments[i], sep=sep_)
  
  results <- read.csv(text=readLines(paste(FRpath, "/", filename, sep=sep_))[-(1:9)])
  results <- results[-1,] #remove first row (all the initialised values)
  row.names(results) <- results[,1]
  results <- results[,-2] # remove first 2 columns
  
  results.costs <- cbind(results[, 1], results[, 2:7]/1000000)
  colnames(results.costs) <- c("Iteration", "Overall.mu", "Holding", 
                               "Shortage", "Emergency", "Cancellation", "Overall.quantile")
  
  sensitivity.results[i, ] <- tail(results.costs,1)[1:2]
}

### If not all at iteration i =100
min.iter <- min(sensitivity.results$t)

for (i in 1:8){
  filename <- paste(experiments[i], ".csv", sep="")
  FRpath <- paste(optPath, "final-results", experiments[i], sep=sep_)
  
  results <- read.csv(text=readLines(paste(FRpath, "/", filename, sep=sep_))[-(1:9)])
  results <- results[-1,] #remove first row (all the initialised values)
  row.names(results) <- results[,1]
  results <- results[,-2] # remove first 2 columns
  
  results.costs <- cbind(results[, 1], results[, 2:7]/1000000)
  colnames(results.costs) <- c("Iteration", "Overall.mu", "Holding", 
                               "Shortage", "Emergency", "Cancellation", "Overall.quantile")
  
  sensitivity.results[i, ] <- results.costs[min.iter, 1:2]
}

effect <- rep(0,3)

for (i in 1:3){
  effect[i] <- sum((-1)^  ceiling((1:8 / (2^ (i-1))))    *
                     sensitivity.results$Cost) /8
}
effect


###### facet - SA
results.final.mus <- data.frame(matrix(rep(NA, 42*8), nrow=8, ncol=42))

for (i in 1:8){
  filename <- paste(experiments[i], ".csv", sep="")
  FRpath <- paste(optPath, "final-results", experiments[i], sep=sep_)
  
  results <- read.csv(text=readLines(paste(FRpath, "/", filename, sep=sep_))[-(1:9)])
  results <- results[-1,] #remove first row (all the initialised values)
  row.names(results) <- results[,1]
  results <- results[,-2] # remove first 2 columns
  
  results.final.mus[i,] <- tail(results[, 1:(psc_tot*3) + 7],1) 
}

SPday.ave_medium <- rep(rep(SPday.ave, times=3), each=8)
results.final.mus <- results.final.mus/SPday.ave_medium
results.final.mus <- melt(results.final.mus)

results.final.mus["Powerstation"] <- rep(rep(ps.names, each=8) , times=3)
results.final.mus["DV"] <- rep(c("Target","LWL","UWL"), each=(14*8))
results.final.mus["SA"] <- rep(c("i","ii","iii","iv","v","vi","vii","viii"), times=(14*3))
# results.final.mus$variable <- NULL


results.final.mus.cor <- results.final.mus

for (i in 1:(nrow(results.final.mus.cor)/8)){
  j <- (i-1)*8 +1
  k <- i*8
  results.final.mus.cor$value[j:k] <- (results.final.mus.cor$value[j:k])/
    max(results.final.mus.cor$value[j:k])
}



p.results.final.mus.cor <- ggplot(results.final.mus.cor, aes(x=SA, y=value, fill=SA)) + 
  geom_bar(stat="identity") +
  facet_grid(Powerstation ~ DV) +
  ylab("Stockpile days") +
  xlab("SA") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.4),
        text = element_text(size=15))
p.results.final.mus.cor
ggsave(file=paste(compare.path,"\\", "final-sa.pdf",sep=""),height=14,width=10)


##### HEATMAP - LWL Target UWL

p.results.final.mus.cor.lwl <- ggplot(results.final.mus.cor[results.final.mus.cor$DV=="LWL",], 
                                      aes(x=SA, y=Powerstation)) + 
  geom_tile(aes(fill=value)) +
#   ylab("Stockpile days") +
#   xlab("SA") +
  theme_bw() +
  theme(text = element_text(size=15))
p.results.final.mus.cor.lwl
ggsave(file=paste(compare.path,"\\", "final-sa-cor-lwl.pdf",sep=""),height=14,width=10)

p.results.final.mus.cor.tar <- ggplot(results.final.mus.cor[results.final.mus.cor$DV=="Target",], 
                                      aes(x=SA, y=Powerstation)) + 
  geom_tile(aes(fill=value)) +
#   ylab("Stockpile days") +
#   xlab("SA") +
  theme_bw() +
  theme(text = element_text(size=15))
p.results.final.mus.cor.tar
ggsave(file=paste(compare.path,"\\", "final-sa-cor-tar.pdf",sep=""),height=12,width=10)


p.results.final.mus.cor.uwl <- ggplot(results.final.mus.cor[results.final.mus.cor$DV=="UWL",], 
                                      aes(x=SA, y=Powerstation)) + 
  geom_tile(aes(fill=value)) +
#   ylab("Stockpile days") +
#   xlab("SA") +
  theme_bw() +
  theme(text = element_text(size=15))
p.results.final.mus.cor.uwl
ggsave(file=paste(compare.path,"\\", "final-sa-cor-uwl.pdf",sep=""),height=12,width=10)


