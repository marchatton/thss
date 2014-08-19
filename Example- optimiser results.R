###### CLEAR PREVIOUS RUN 
#2014 06 25
sink()
sink()
sink()
cat("\014") #same as using ctrl+"l"
rm(list = ls()) #clear global environment

maxIterations <- 50
N <- 50
fp_set <- 5
print(paste("estimated completion time ", round(maxIterations*N*18*8/60/60,2), " hours.", sep="")) 

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



opt_results <- read.csv(paste(paste(THEPATH, "CEM_results", sep=sep_) ,".csv",sep=""), 
                            header = TRUE, sep = ",", quote = "\"", dec = ".", 
                            fill = TRUE, comment.char = "",skip=9)
opt_results_objfunc <- opt_results[-1,3]

# autos_data <- read.table("C:/R/autos.dat", header=T, sep="\t")

# # Graph cars with specified labels for axes.  Use blue 
# # borders and diagnal lines in bars.
# barplot(autos_data$cars, main="Cars", xlab="Days",  
#         ylab="Total", names.arg=c("Mon","Tue","Wed","Thu","Fri"), 
#         border="blue", density=c(10,20,30,40,50))
# 
# 
# 
# 
# barplot()


pdf(paste(optEstPath,"/results_objfunc.pdf",sep=""),width=40,height=30)
par(cex.lab=2, cex.axis=1.5, cex.main=2, cex.sub=2, mar=c(9,14,1,1)+0.1) #mar:bottom,left,top,right
barplot(opt_results_objfunc, las=1, names=1:length(opt_results_objfunc), yaxt="n", frame="f")


axis(
  2, # puts the axis on the left
  at=seq(0,500000,by=50000), # creates a vector of label locations starting at 0 ever 1 mil to 11 mil
  labels=formatC(seq(0,500000,by=50000),, format="d", big.mark=','), # similar labels with formatting
  las=1, # rotate labels to be parallel
  cex.axis=2 # offset of labels
#   lwd=0, # width of the long axis line is zero, makes invisible
#   lwd.ticks=1, # tick marks are 1 wide
#   tck=-0.02, # length of ticks, negative goes out from the plot
#   mgp=c(0,0.35,0) # 0.35 controls the left-right movemenet of the tick labels
)

mtext("Objective function value (Rands)", 2, line=10, cex=2)
loc = par("usr")
text(loc[1], loc[3], "\nt", adj=c(0.5,1), xpd = T,cex=2)
dev.off()



pdf(paste(optEstPath,"/resultsAscend.pdf",sep=""),width=40,height=30)
par(cex.lab=2, cex.axis=1.5, cex.main=2, cex.sub=2, mar=c(9,14,1,1)+0.1) #mar:bottom,left,top,right
barplot(opt_results_objfunc, las=1, names=1:length(opt_results_objfunc), yaxt="n", frame="f")


axis(
  2, # puts the axis on the left
  at=seq(0,500000,by=50000), # creates a vector of label locations starting at 0 ever 1 mil to 11 mil
  labels=formatC(seq(0,500000,by=50000),, format="d", big.mark=','), # similar labels with formatting
  las=1, # rotate labels to be parallel
  cex.axis=2 # offset of labels
  #   lwd=0, # width of the long axis line is zero, makes invisible
  #   lwd.ticks=1, # tick marks are 1 wide
  #   tck=-0.02, # length of ticks, negative goes out from the plot
  #   mgp=c(0,0.35,0) # 0.35 controls the left-right movemenet of the tick labels
)

mtext("Objective function value (Rands)", 2, line=10, cex=2)
loc = par("usr")
text(loc[1], loc[3], "\nt", adj=c(0.5,1), xpd = T,cex=2)
dev.off()



