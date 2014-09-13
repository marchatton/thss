require(ggplot2)
require(reshape2)
require(lubridate)

###### CLEAR PREVIOUS RUN 
#2014 06 25
sink()
sink()
sink()

maxIterations <- 1
N <- 1
fp_set <- 0

###### START STOPWATCH
tic <- Sys.time() #begin stopwatch

###### FILE PATHS USED IN OPTIMISER
## !!Adjust these paths to the folder where EFS is running!!
## First Start DIAS then Run this in RStudio
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
  Rcode_path  <- file.path("C:\\Users\\MarcHatton\\MEGA\\Postgraduate\\Thesis\\Algorithms\\R code - Marc") 
  THEPATH  <-  "C:\\Users\\MarcHatton\\Desktop\\EFS APP"
  THEDBPATH  <-  "C:\\Users\\MarcHatton\\Desktop\\EFS APP\\e-breadboard\\resources\\za.co.enerweb_energy-flow-simulator3-var\\dbs" 
}

print(paste("Using computer",fp_set))
print(Rcode_path)
print(THEPATH)
print(THEDBPATH)

###### NECESSARY INITIAL VALUES USED IN DEFINING SETTINGS
func_name <- "CEM"
t <- 1 # time-step counter

###### MAIN SETTINGS. MUST CHANGE FILEPATHS IF RUNNING ON A DIFFERENT COMPUTER. MUST ALSO INSTALL PACKAGES LISTED THEREIN.
source(paste(Rcode_path,"main_settings.R",sep=.Platform$file.sep), local=TRUE)


Analyse.Results <- function(res.choose=4, confidential=FALSE){
  
  ###### results - analyse
  if (res.choose == 1){
    filename <- "CEM_results1.csv"
    title_ <- paste("Powerstation = 1.  Decision variable = desired SP", sep="")  
    graphname <- "ps1-dv1_"
  }else if (res.choose == 2){
    filename <- "CEM_results2.csv"
    title_ <- paste("Powerstation = 14.  Decision variable= desired SP", sep="")  
    graphname <- "ps14-dv1_"
  } else if (res.choose == 3){
    filename <- "CEM_results3.csv"
    title_ <- paste("Powerstation = 1.  Decision variables = LWL, desired SP, UWL", sep="")  
    graphname <- "ps1-dv3_"
  } else if (res.choose == 4){
    filename <- "CEM_results4.csv"
    title_ <- paste("Powerstation = 14.  Decision variables = LWL, desired SP, UWL", sep="")  
    graphname <- "ps14-dv3_"
  } 
  
  parameters_ <- read.csv(text=readLines(paste(optPath, "/", filename, sep=sep_))[1:8], header=F)[,1:2]
  options.ps2 <- as.vector(parameters_$V2)[2]
  options.ps2 <- ifelse(options.ps2=="all", options.ps2, as.numeric(options.ps2))
  options.dv2 <- as.numeric(as.vector(parameters_$V2)[3])
  options.eval2 <- as.numeric(as.vector(parameters_$V2)[4])
  
  results <- read.csv(text=readLines(paste(optPath, "/", filename, sep=sep_))[-(1:9)])
  results <- results[-1,] #remove first row (all the initialised values)
  row.names(results) <- results[,1]
  results <- results[,-2] # remove first 2 columns
  
  # dec.var <- ifelse(options.dv2==1, "DES", 
  #                   ifelse(options.dv2==3, "LWL, DES & UWL", NA))
  ts <- results[nrow(results), 1]
  
  if (confidential==FALSE){
    ps.names <- colnames(psc_template)
  } else if (confidential==TRUE){
    ps.names <- paste("Powerstation ", 1:psc_tot, sep="")
  }
    
  results.costs <- results[, c(1, 2:7)]
  colnames(results.costs) <- c("Iteration", "Overall.mu", "Holding", 
                               "Shortage", "Emergency", "Cancellation", "Overall.quantile")
  if (options.dv2==1){
    results.costs <- results.costs[, -c(5,6)]
  }
  
  
  results.mus <- data.frame(Iteration=rep(results[,1],psc_tot))
  results.mus["Desired"] <- melt(results[, c(1:14 +7)], id.vars=)[,2]
  results.mus["LWL"] <- melt(results[, c(1:14 +7+14)])[,2]
  results.mus["UWL"] <- melt(results[, c(1:14 +7+14+14)])[,2]
  results.mus["Powerstation"] <- rep(ps.names, 
                                     each=ts)
  
  results.sigmas <- data.frame(Iteration=rep(results[,1],psc_tot))
  results.sigmas["Desired"] <- melt(results[, c(1:14 +7+42)])[,2]
  results.sigmas["LWL"] <- melt(results[, c(1:14 +7+14+42)])[,2]
  results.sigmas["UWL"] <- melt(results[, c(1:14 +7+14+14+42)])[,2]
  results.sigmas["Powerstation"] <- rep(ps.names, 
                                        each=ts)
  
  #only use specified functions
  if (options.ps2!="all"){
    results.mus <- results.mus[results.mus$Powerstation==ps.names[options.ps2], ]
    results.sigmas <- results.sigmas[results.sigmas$Powerstation==ps.names[options.ps2], ]
  }
  
  ###mus
  head(results.mus)
  
  ##des
  p.mus.des <- ggplot(data=results.mus[, -c(3,4)], aes(x=Iteration, y=Desired, colour=Powerstation)) + 
    geom_line() +
    geom_point(aes(shape=Powerstation),
               fill = "white",    
               size = 2)  +       
    scale_shape_manual(values=(1:psc_tot -1)) +
    ylab("Desired stockpile level (ktons)") +
    scale_y_continuous(breaks=seq(0, 1000,50)) +  
    scale_x_continuous(breaks=seq(0, 100, 10)) + 
    ggtitle(title_)
  p.mus.des
  ggsave(file=paste(optPath,"\\", graphname, "mus-des.pdf",sep=""),height=6,width=9)
  
  ##lwl
  if (res.choose==3 || res.choose==4){
    p.mus.lwl <- ggplot(data=results.mus[, -c(2,4)], aes(x=Iteration, y=LWL, colour=Powerstation)) + 
      geom_line() +
      geom_point(aes(shape=Powerstation),
                 fill = "white",    
                 size = 2)  +       
      scale_shape_manual(values=(1:psc_tot -1)) +
      ylab("LWL (ktons)") +
      scale_y_continuous(breaks=seq(0, 1000,25)) + 
      scale_x_continuous(breaks=seq(0, 100, 10)) + 
      ggtitle(title_)
    p.mus.lwl
    ggsave(file=paste(optPath,"\\", graphname, "mus-lwl.pdf",sep=""),height=6,width=9)
  }
  
  ##uwl
  if (res.choose==3 || res.choose==4){
    p.mus.uwl <- ggplot(data=results.mus[, -c(2,3)], aes(x=Iteration, y=UWL, colour=Powerstation)) + 
      geom_line() +
      geom_point(aes(shape=Powerstation),
                 fill = "white",    
                 size = 2)  +       
      scale_shape_manual(values=(1:psc_tot -1)) +
      scale_y_continuous(breaks=seq(0, 2000, 100)) + 
      scale_x_continuous(breaks=seq(0, 100, 10)) + 
      ylab("UWL (ktons)") +
      ggtitle(title_)
    p.mus.uwl
    ggsave(file=paste(optPath,"\\", graphname, "mus-uwl.pdf",sep=""),height=6,width=9)
  }
  
  ###sigmas
  head(results.sigmas)
  
  ##des
  p.sigmas.des <- ggplot(data=results.sigmas[, -c(3,4)], aes(x=Iteration, y=Desired, colour=Powerstation)) + 
    geom_line() +
    geom_point(aes(shape=Powerstation),
               fill = "white",    
               size = 2)  +       
    scale_shape_manual(values=(1:psc_tot -1)) +
    scale_y_continuous(breaks=seq(0, 2000, 25)) + 
    scale_x_continuous(breaks=seq(0, 100, 10)) + 
    ylab("Desired stockpile level (ktons)") +
    ggtitle(title_)
  p.sigmas.des
  ggsave(file=paste(optPath,"\\", graphname, "sigmas-des.pdf",sep=""),height=6,width=9)
  
  ##lwl
  if (res.choose==3 || res.choose==4){
    p.sigmas.lwl <- ggplot(data=results.sigmas[, -c(2,4)], aes(x=Iteration, y=LWL, colour=Powerstation)) + 
      geom_line() +
      geom_point(aes(shape=Powerstation),
                 fill = "white",    
                 size = 2)  +       
      scale_shape_manual(values=(1:psc_tot -1)) +
      scale_y_continuous(breaks=seq(0, 2000, 25)) + 
      scale_x_continuous(breaks=seq(0, 100, 10)) + 
      ylab("LWL (ktons)") +
      ggtitle(title_)
    p.sigmas.lwl
    ggsave(file=paste(optPath,"\\", graphname, "sigmas-lwl.pdf",sep=""),height=6,width=9)
  }
  
  ##uwl
  if (res.choose==3 || res.choose==4){
    p.sigmas.uwl <- ggplot(data=results.sigmas[, -c(2,3)], aes(x=Iteration, y=UWL, colour=Powerstation)) + 
      geom_line() +
      geom_point(aes(shape=Powerstation),
                 fill = "white",    
                 size = 2)  +       
      scale_shape_manual(values=(1:psc_tot -1)) +
      scale_y_continuous(breaks=seq(0, 2000, 25)) + 
      scale_x_continuous(breaks=seq(0, 100, 10)) + 
      ylab("UWL (ktons)") +
      ggtitle(title_)
    p.sigmas.uwl
    ggsave(file=paste(optPath,"\\", graphname, "sigmas-uwl.pdf",sep=""),height=6,width=9)
  }
  
  
  ###plot stockpile ribbon function
  Stockpile.Ribbon <- function(ps_chosen=1){
    # #create custom palette
    #myColors <- colorRampPalette(brewer.pal(9,"Set1"))(psc_tot)
    #names(myColors) <- levels(colnames(psc_template))
    #colScale <- scale_colour_manual(name = "Powerstation",values = myColors)
    
    ps_chosen.name <- ps.names[ps_chosen]
    results.mus.chosen <- results.mus[results.mus[,5]==ps_chosen.name, ]
    head(results.mus)
    
    p.mus.chosen <- ggplot(data=results.mus.chosen, aes(x=Iteration, y=Desired, ymin=LWL, ymax=UWL)) + 
      geom_line() +
      geom_ribbon(alpha=0.6) +
      geom_point(fill = "white",    
                 size = 2)  +       
      scale_shape_manual(values=(1:psc_tot -1)) + 
      scale_y_continuous(breaks=seq(0, 2000, 50)) + 
      scale_x_continuous(breaks=seq(0, 100, 10)) + 
      ylab("Stockpile level (ktons)") 
    
    p.mus.chosen
    ggsave(file=paste(optPath,"\\", graphname, "stockpile.ribbon-", ps_chosen.name, ".pdf",sep=""),height=6,width=9)
  }
  
  ###loop through stockpile ribbon function
  if ((options.ps2>0) && (options.ps2<=psc_tot)){
    Stockpile.Ribbon(options.ps2)
  }else if(options.ps2!="all"){
    draw.count <- 0
    while (draw.count<psc_tot){
      draw.count <- draw.count+1
      Stockpile.Ribbon(draw.count)
    }
  }
  
  results.costs <- melt(results.costs, id.vars="Iteration")
  colnames(results.costs) <- c("Iteration", "Cost", "value")
  
  p.costs.all <- ggplot(data=results.costs, aes(x=Iteration, y=value, colour=Cost)) + 
    geom_line() +
    geom_point(aes(shape=Cost),
               fill = "white",    
               size = 2)  +       
    scale_shape_manual(values=(1:length(levels(results.costs$Cost)) -1))
  p.costs.all
  ggsave(file=paste(optPath,"\\", graphname, "costs-all.pdf",sep=""),height=6,width=9)
  
  p.costs.quan <- ggplot(data=results.costs[results.costs["Cost"]=="Overall.quantile",], aes(x=Iteration, y=value)) + 
    geom_line() +
    geom_point()
  p.costs.quan
  ggsave(file=paste(optPath,"\\", graphname, "costs-Zquan.pdf",sep=""),height=6,width=9)
  
}



# Analyse.Results(1)
# Analyse.Results(2)
# Analyse.Results(3)
# Analyse.Results(4)

Analyse.Results(4,TRUE)
