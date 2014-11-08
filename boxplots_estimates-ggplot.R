library(lubridate)
library(reshape2)

confidential <- FALSE

######## USED TO CREATE BOX PLOTS - BOX PLOTS SHOW VARIATION IN THE ESTIMATES

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
apply(est_dif,2,summary)

#names for box plot
if (confidential==FALSE){
  ps_names <- colnames(psc_template)
  for (i in 1:psc_tot){
    ps_names[i] <- substr(ps_names[i],1,4)
  }
}else if (confidential==TRUE){
  ps_names <- LETTERS[1:psc_tot]
}

bp_dates <- format(dates, "%y-%m")
bp_dates2 <- substr(month(dates, label = T),1,4)

bp_names2 <- matrix(NA, interval_num, psc_tot)
for (jjj in 1:psc_tot){
  for (iii in 1:interval_num){
    bp_names2[iii,jjj] <- paste(ps_names[jjj], "--", bp_dates2[iii], sep="")
  }
}
bp_names2 <- as.vector(bp_names2)


# est_burnin
# est_burnout
# est_delvin
# est_delvout

burnout.ggplot <- est_burnout
colnames(burnout.ggplot) <- bp_names2
n.estimates <- nrow(burnout.ggplot)

blahblah <- burnout.ggplot

SP1day.ave2 <- rep(c(17.37026, 14.59808, 25.62025, 10.94664, 17.03563, 51.92641, 39.03720, 43.67920,  9.14883, 11.85286, 37.52245, 30.37924, 27.31237, 10.00592),each=8)
blahblah[,] <- rep(SP1day.ave2,each=1000)
burnout.ggplot <- burnout.ggplot/blahblah

burnout.ggplot <- melt(burnout.ggplot)
burnout.ggplot["Powerstation"] <- rep(ps_names, each=(interval_num*n.estimates))



burnout.ggplot <- burnout.ggplot[burnout.ggplot$Powerstation == ps_names, ] #c(3,6,8,9)

boxplot.burnout <- ggplot(burnout.ggplot, aes(x=variable, y=value)) +
  geom_boxplot() +
  facet_wrap( ~ Powerstation) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.4)) +
  xlab("Powerstation--Month") +
  ylab("Coal (ktons)") +
  scale_y_continuous(breaks=seq(0, 2000, 200), limits=c(0,2000)) +
  scale_x_discrete(breaks=bp_dates2)
 
  boxplot.burnout
  
  ggsave(file=paste(optEstPath,"\\", , ".pdf",sep=""),height=6,width=9)
  




bp_names4 <- matrix(NA, interval_num, psc_tot)
grouping4 <- matrix(NA, interval_num, psc_tot)
# ps_names4 <- tail(ps_names,4)
ps_names4 <- ps_names

for (jjj in 1:psc_tot){
  for (iii in 1:interval_num){
    bp_names4[iii,jjj] <- paste(ps_names4[jjj], "--", bp_dates2[iii], sep="")
    grouping4[iii,jjj] <- (jjj-1)*interval_num + iii + jjj-1
  }
}
bp_names4 <- as.vector(bp_names4)
grouping4 <- as.vector(grouping4)
colours4 <- rep(rainbow(psc_tot), each=interval_num)

ps.chosenrange <- (interval_num*psc_tot - psc_tot*interval_num +1):(interval_num*psc_tot)
est_dif4 <- est_dif[, ps.chosenrange]/blahblah
est_delv_dif4 <- est_delv_dif[, ps.chosenrange]/blahblah
est_burn_dif4 <- est_burn_dif[, ps.chosenrange]/blahblah

est_delvout4 <- est_delvout[, ps.chosenrange]/blahblah
est_burnout4 <- est_burnout[, ps.chosenrange]/blahblah


####create box plots - est_dif
#all on a single box plot
pdf(paste(optEstPath,"/dif_all.pdf",sep=""),width=12,height=9)
par(cex.lab=2, cex.axis=0.75, cex.main=2, cex.sub=2, mar=c(9,10,1,1)+0.1) #mar:bottom,left,top,right
boxplot(est_dif4, las=2, col=colours4, at=grouping4, names=bp_names4)
mtext("Deliveries - burn (ktons)", 2, line=5, cex=2)
mtext("Powerstation--Month", 1, line=7, cex=2)
dev.off()

####create box plots - delv_out
#all on a single box plot
pdf(paste(optEstPath,"/delv_out_all.pdf",sep=""),width=12,height=9)
par(cex.lab=2, cex.axis=0.75, cex.main=2, cex.sub=2, mar=c(9,10,1,1)+0.1) #mar:bottom,left,top,right
boxplot(est_delvout4, las=2, col=colours4, at=grouping4, names=bp_names4)
mtext("Deliveries (ktons)", 2, line=5, cex=2)
mtext("Powerstation--Month", 1, line=7, cex=2)
dev.off()

####create box plots - burnout
#all on a single box plot
pdf(paste(optEstPath,"/burn_out_all.pdf",sep=""),width=12,height=9)
par(cex.lab=2, cex.axis=0.75, cex.main=2, cex.sub=2, mar=c(9,10,1,1)+0.1) #mar:bottom,left,top,right
boxplot(est_burnout4, las=2, col=colours4, at=grouping4, names=bp_names4)
mtext("Burn (ktons)", 2, line=5, cex=2)
mtext("Powerstation--Month", 1, line=7, cex=2)
dev.off()

####create box plots - delvdif
#all on a single box plot
pdf(paste(optEstPath,"/delv_dif_all.pdf",sep=""),width=12,height=9)
par(cex.lab=2, cex.axis=0.75, cex.main=2, cex.sub=2, mar=c(9,10,1,1)+0.1) #mar:bottom,left,top,right
boxplot(est_delv_dif4, las=2, col=colours4, at=grouping4, names=bp_names4)
mtext("Deliveries: In - out  (ktons)", 2, line=5, cex=2)
mtext("Powerstation--Month", 1, line=7, cex=2)
dev.off()

####create box plots - burndif
#all on a single box plot
pdf(paste(optEstPath,"/burn_dif_all.pdf",sep=""),width=12,height=9)
par(cex.lab=2, cex.axis=0.75, cex.main=2, cex.sub=2, mar=c(9,10,1,1)+0.1) #mar:bottom,left,top,right
boxplot(est_burn_dif4, las=2, col=colours4, at=grouping4, names=bp_names4)
mtext("Burn: In - out (ktons)", 2, line=5, cex=2)
mtext("Powerstation--Month", 1, line=7, cex=2)
dev.off()

