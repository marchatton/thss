delv_emer <- psc_template
delv_canc <- psc_template
delv_emer[,] <- 0
delv_canc[,] <- 0
SPvar <- 0 
dv_delv[,] <- dv_delv_base

leadtime_ul <- 3
leadtime_ll <- 1
leadtime_mean <- 2

leadtime_rand <- function(){
  sample(leadtime_ll:leadtime_ul, size=1)
}

#create data.frame from the same structure as psc_template, but
#with more "leadtime_ul" more rows than psc_template.
#do this for delv and canc.
#delv future
delv_emer.future <- data.frame(matrix(0, nrow=(interval_num+leadtime_ul), ncol=psc_tot))
colnames(delv_emer.future) <- colnames(psc_template)
#canc future
delv_canc.future <- data.frame(matrix(0, nrow=(interval_num+leadtime_ul), ncol=psc_tot))
colnames(delv_canc.future) <- colnames(psc_template)

iii <- 0
jjj <- 1
