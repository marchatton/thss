###### CLEAR PREVIOUS RUN 
#2014 06 25
sink()
sink()
sink()
cat("\014") #same as using ctrl+"l"
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

###### THE REAL DEAL
est_burnout_Duvha <- read.csv(paste(paste(optEstPath, est_names[4], sep=sep_) ,".csv",sep=""), 
                        header = TRUE, sep = ",", quote = "\"", dec = ".", 
                        fill = TRUE, comment.char = "")
est_burnout_Duvha[,1] <- NULL # clean dataframe
est_burnout_Duvha <- est_burnout_Duvha[,1:8 + 2*8]

# names for box plot
bp_names <- format(dates, "%y-%m")
colours <- heat.colors(interval_num)

####create box plots - est_dif
#all on a single box plot
pdf(paste(optEstPath,"/Duvha_burnout.pdf",sep=""),width=40,height=30)
par(cex.lab=2, cex.axis=1.5, cex.main=2, cex.sub=2, mar=c(6,10,1,1)+0.1) #mar:bottom,left,top,right
boxplot(est_burnout_Duvha, las=2, col=colours, names=bp_names)
lines(apply(est_burnout_Duvha,2,mean),type="o",col="blue")
mtext("BURN OUT   (kTons)", 2, line=5, cex=2)
loc = par("usr") 
text(loc[1], loc[3], "\nDATE\n(yy-mm)", adj=c(0.5,1), xpd = T,cex=2) 
dev.off()

pdf(paste(optEstPath,"/Duvha_dif.pdf",sep=""),width=40,height=30)
par(cex.lab=2, cex.axis=1.5, cex.main=2, cex.sub=2, mar=c(6,10,1,1)+0.1) #mar:bottom,left,top,right
boxplot(apply(est_burnout_Duvha,2,mean) - est_burnout_Duvha, las=2, col=colours, names=bp_names)
lines(apply(est_burnout_Duvha,2,mean),type="o",col="blue")
mtext("BURN OUT   (kTons)", 2, line=5, cex=2)
loc = par("usr") 
text(loc[1], loc[3], "\nDATE\n(yy-mm)", adj=c(0.5,1), xpd = T,cex=2) 
dev.off()
