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
ps_names <- colnames(psc_template)
for (i in 1:psc_tot){
  ps_names[i] <- substr(ps_names[i],1,4)
}

bp_dates <- format(dates, "%y-%m")

bp_names <- matrix(NA, interval_num, psc_tot)
grouping <- matrix(NA, interval_num, psc_tot)
for (jjj in 1:psc_tot){
  for (iii in 1:interval_num){
    bp_names[iii,jjj] <- paste(ps_names[jjj], " ", bp_dates[iii], sep="")
    grouping[iii,jjj] <- (jjj-1)*interval_num + iii + jjj-1
  }
}
bp_names <- as.vector(bp_names)
grouping <- as.vector(grouping)
colours <- rep(heat.colors(interval_num), psc_tot)

####create box plots - est_dif
#all on a single box plot
pdf(paste(optEstPath,"/dif_all.pdf",sep=""),width=40,height=30)
par(cex.lab=2, cex.axis=1.5, cex.main=2, cex.sub=2, mar=c(9,10,1,1)+0.1) #mar:bottom,left,top,right
boxplot(est_dif, las=2, col=colours, at=grouping, names=bp_names)
mtext("DELIVERIES OUT - BURN OUT   (kTons)", 2, line=5, cex=2)
loc = par("usr")
text(loc[1], loc[3], "\nPS \n& DATE\n(yy-mm)", adj=c(0.5,1), xpd = T,cex=2)
dev.off()

#on 2 different box plots
#plot 1st half
pdf(paste(optEstPath,"/dif_est_ps1to7.pdf",sep=""),width=40,height=30)
par(cex.lab=2, cex.axis=1.5, cex.main=2, cex.sub=2, mar=c(9,10,1,1)+0.1) #mar:bottom,left,top,right
boxplot(est_dif[,1:round(dim(est_dif)[2]/2)], las=2, col=colours[1:round(length(colours)/2)], at=grouping[1:round(length(grouping)/2)], names=bp_names[1:round(length(bp_names)/2)])
mtext("DELIVERIES OUT - BURN OUT   (kTons)", 2, line=5, cex=2)
loc = par("usr") 
text(loc[1], loc[3], "\nPS \n& DATE\n(yy-mm)", adj=c(0.5,1), xpd = T,cex=2) 
dev.off()

#plot 2nd half
pdf(paste(optEstPath,"/dif_est_ps8to14.pdf",sep=""),width=40,height=30)
par(cex.lab=2, cex.axis=1.5, cex.main=2, cex.sub=2, mar=c(9,10,1,1)+0.1) #mar:bottom,left,top,right
boxplot(est_dif[,round(dim(est_dif)[2]/2+1):dim(est_dif)[2]], las=2, col=colours[round(length(colours)/2+1):length(colours)], at=grouping[round(length(grouping)/2+1):length(grouping)], names=bp_names[round(length(bp_names)/2+1):length(bp_names)])
mtext("DELIVERIES OUT - BURN OUT   (kTons)", 2, line=5, cex=2)
loc = par("usr") 
text(loc[1], loc[3], "\nPS \n& DATE\n(yy-mm)", adj=c(0.5,1), xpd = T,cex=2) 
dev.off()

####create box plots - delv_out
#all on a single box plot
pdf(paste(optEstPath,"/delv_out_all.pdf",sep=""),width=40,height=30)
par(cex.lab=2, cex.axis=1.5, cex.main=2, cex.sub=2, mar=c(9,10,1,1)+0.1) #mar:bottom,left,top,right
boxplot(est_delvout, las=2, col=colours, at=grouping, names=bp_names)
mtext("DELIVERIES OUT   (kTons)", 2, line=5, cex=2)
loc = par("usr") 
text(loc[1], loc[3], "\nPS \n& DATE\n(yy-mm)", adj=c(0.5,1), xpd = T,cex=2) 
dev.off()

#on 2 different box plots
#plot 1st half
pdf(paste(optEstPath,"/delv_out_est_ps1to7.pdf",sep=""),width=40,height=30)
par(cex.lab=2, cex.axis=1.5, cex.main=2, cex.sub=2, mar=c(9,10,1,1)+0.1) #mar:bottom,left,top,right
boxplot(est_delvout[,1:round(dim(est_delvout)[2]/2)], las=2, col=colours[1:round(length(colours)/2)], at=grouping[1:round(length(grouping)/2)], names=bp_names[1:round(length(bp_names)/2)])
mtext("DELIVERIES OUT   (kTons)", 2, line=5, cex=2)
loc = par("usr") 
text(loc[1], loc[3], "\nPS \n& DATE\n(yy-mm)", adj=c(0.5,1), xpd = T,cex=2) 
dev.off()

#plot 2nd half
pdf(paste(optEstPath,"/delv_out_est_ps8to14.pdf",sep=""),width=40,height=30)
par(cex.lab=2, cex.axis=1.5, cex.main=2, cex.sub=2, mar=c(9,10,1,1)+0.1) #mar:bottom,left,top,right
boxplot(est_delvout[,round(dim(est_delvout)[2]/2+1):dim(est_delvout)[2]], las=2, col=colours[round(length(colours)/2+1):length(colours)], at=grouping[round(length(grouping)/2+1):length(grouping)], names=bp_names[round(length(bp_names)/2+1):length(bp_names)])
mtext("DELIVERIES OUT   (kTons)", 2, line=5, cex=2)
loc = par("usr") 
text(loc[1], loc[3], "\nPS \n& DATE\n(yy-mm)", adj=c(0.5,1), xpd = T,cex=2) 
dev.off()

####create box plots - burnout
#all on a single box plot
pdf(paste(optEstPath,"/burn_out_all.pdf",sep=""),width=40,height=30)
par(cex.lab=2, cex.axis=1.5, cex.main=2, cex.sub=2, mar=c(9,10,1,1)+0.1) #mar:bottom,left,top,right
boxplot(est_burnout, las=2, col=colours, at=grouping, names=bp_names)
mtext("BURN OUT   (kTons)", 2, line=5, cex=2)
loc = par("usr") 
text(loc[1], loc[3], "\nPS \n& DATE\n(yy-mm)", adj=c(0.5,1), xpd = T,cex=2) 
dev.off()

#on 2 different box plots
#plot 1st half
pdf(paste(optEstPath,"/burn_out_est_ps1to7.pdf",sep=""),width=40,height=30)
par(cex.lab=2, cex.axis=1.5, cex.main=2, cex.sub=2, mar=c(9,10,1,1)+0.1) #mar:bottom,left,top,right
boxplot(est_burnout[,1:round(dim(est_burnout)[2]/2)], las=2, col=colours[1:round(length(colours)/2)], at=grouping[1:round(length(grouping)/2)], names=bp_names[1:round(length(bp_names)/2)])
mtext("BURN OUT   (kTons)", 2, line=5, cex=2)
loc = par("usr") 
text(loc[1], loc[3], "\nPS \n& DATE\n(yy-mm)", adj=c(0.5,1), xpd = T,cex=2) 
dev.off()

#plot 2nd half
pdf(paste(optEstPath,"/burn_out_est_ps8to14.pdf",sep=""),width=40,height=30)
par(cex.lab=2, cex.axis=1.5, cex.main=2, cex.sub=2, mar=c(9,10,1,1)+0.1) #mar:bottom,left,top,right
boxplot(est_burnout[,round(dim(est_burnout)[2]/2+1):dim(est_burnout)[2]], las=2, col=colours[round(length(colours)/2+1):length(colours)], at=grouping[round(length(grouping)/2+1):length(grouping)], names=bp_names[round(length(bp_names)/2+1):length(bp_names)])
mtext("BURN OUT   (kTons)", 2, line=5, cex=2)
loc = par("usr") 
text(loc[1], loc[3], "\nPS \n& DATE\n(yy-mm)", adj=c(0.5,1), xpd = T,cex=2) 
dev.off()

####create box plots - delvdif
#all on a single box plot
pdf(paste(optEstPath,"/delv_dif_all.pdf",sep=""),width=40,height=30)
par(cex.lab=2, cex.axis=1.5, cex.main=2, cex.sub=2, mar=c(9,10,1,1)+0.1) #mar:bottom,left,top,right
boxplot(est_delv_dif, las=2, col=colours, at=grouping, names=bp_names)
mtext("DELIVERIES IN - DELIVERIES OUT   (kTons)", 2, line=5, cex=2)
loc = par("usr") 
text(loc[1], loc[3], "\nPS \n& DATE\n(yy-mm)", adj=c(0.5,1), xpd = T,cex=2) 
dev.off()

#on 2 different box plots
#plot 1st half
pdf(paste(optEstPath,"/delv_dif_est_ps1to7.pdf",sep=""),width=40,height=30)
par(cex.lab=2, cex.axis=1.5, cex.main=2, cex.sub=2, mar=c(9,10,1,1)+0.1) #mar:bottom,left,top,right
boxplot(est_delv_dif[,1:round(dim(est_delv_dif)[2]/2)], las=2, col=colours[1:round(length(colours)/2)], at=grouping[1:round(length(grouping)/2)], names=bp_names[1:round(length(bp_names)/2)])
mtext("DELIVERIES IN - DELIVERIES OUT   (kTons)", 2, line=5, cex=2)
loc = par("usr") 
text(loc[1], loc[3], "\nPS \n& DATE\n(yy-mm)", adj=c(0.5,1), xpd = T,cex=2) 
dev.off()

#plot 2nd half

pdf(paste(optEstPath,"/delv_dif_est_ps8to14.pdf",sep=""),width=40,height=30)
par(cex.lab=2, cex.axis=1.5, cex.main=2, cex.sub=2, mar=c(9,10,1,1)+0.1) #mar:bottom,left,top,right
boxplot(est_delv_dif[,round(dim(est_delv_dif)[2]/2+1):dim(est_delv_dif)[2]], las=2, col=colours[round(length(colours)/2+1):length(colours)], at=grouping[round(length(grouping)/2+1):length(grouping)], names=bp_names[round(length(bp_names)/2+1):length(bp_names)])
mtext("DELIVERIES IN - DELIVERIES OUT   (kTons)", 2, line=5, cex=2)
loc = par("usr") 
text(loc[1], loc[3], "\nPS \n& DATE\n(yy-mm)", adj=c(0.5,1), xpd = T,cex=2) 
dev.off()

####create box plots - burndif
#all on a single box plot
pdf(paste(optEstPath,"/burn_dif_all.pdf",sep=""),width=40,height=30)
par(cex.lab=2, cex.axis=1.5, cex.main=2, cex.sub=2, mar=c(9,10,1,1)+0.1) #mar:bottom,left,top,right
boxplot(est_burn_dif, las=2, col=colours, at=grouping, names=bp_names)
mtext("BURN IN - BURN OUT   (kTons)", 2, line=5, cex=2)
loc = par("usr") 
text(loc[1], loc[3], "\nPS \n& DATE\n(yy-mm)", adj=c(0.5,1), xpd = T,cex=2) 
dev.off()

#on 2 different box plots
#plot 1st half
pdf(paste(optEstPath,"/burn_dif_est_ps1to7.pdf",sep=""),width=40,height=30)
par(cex.lab=2, cex.axis=1.5, cex.main=2, cex.sub=2, mar=c(9,10,1,1)+0.1) #mar:bottom,left,top,right
boxplot(est_burn_dif[,1:round(dim(est_burn_dif)[2]/2)], las=2, col=colours[1:round(length(colours)/2)], at=grouping[1:round(length(grouping)/2)], names=bp_names[1:round(length(bp_names)/2)])
mtext("BURN IN - BURN OUT   (kTons)", 2, line=5, cex=2)
loc = par("usr") 
text(loc[1], loc[3], "\nPS \n& DATE\n(yy-mm)", adj=c(0.5,1), xpd = T,cex=2) 
dev.off()

#plot 2nd half
pdf(paste(optEstPath,"/burn_dif_est_ps8to14.pdf",sep=""),width=40,height=30)
par(cex.lab=2, cex.axis=1.5, cex.main=2, cex.sub=2, mar=c(9,10,1,1)+0.1) #mar:bottom,left,top,right
boxplot(est_burn_dif[,round(dim(est_burn_dif)[2]/2+1):dim(est_burn_dif)[2]], las=2, col=colours[round(length(colours)/2+1):length(colours)], at=grouping[round(length(grouping)/2+1):length(grouping)], names=bp_names[round(length(bp_names)/2+1):length(bp_names)])
mtext("BURN IN - BURN OUT   (kTons)", 2, line=5, cex=2)
loc = par("usr") 
text(loc[1], loc[3], "\nPS \n& DATE\n(yy-mm)", adj=c(0.5,1), xpd = T,cex=2) 
dev.off()

