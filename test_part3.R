iii <- iii+1
print(c("i=", iii))
print(c("j=", jjj))

lwPos <- jjj + psc_tot
uwPos <- jjj + 2*psc_tot

if (mode=="x"){ #for x's
  lw_ <- x[kkk, lwPos]
  uw_ <- x[kkk, uwPos]
}else if (mode=="mu"){ #for mu's
  lw_ <- mus[lwPos]
  uw_ <- mus[uwPos]
}  

#difference in future emergency deliveries and cancellation of deliveries
delv_var.future <- sum(delv_emer.future[(iii+1):nrow(delv_emer.future), jjj]) -
  sum(delv_canc.future[(iii+1):nrow(delv_canc.future), jjj])

#SP var = actual - desired + future_delv
SPvar <- abs(
  psc_SPvol[iii,jjj] - 
    dv_SPinitial[jjj]
)

SPvar.future <- abs(
  psc_SPvol[iii,jjj] - 
    dv_SPinitial[jjj] + 
    delv_var.future
)

if ((psc_SPvol[iii,jjj] + delv_var.future) < lw_){ # if actual < LowerWarning
  leadtime <- leadtime_rand()
  delv_emer.future[iii+leadtime, jjj] <- SPvar.future
  if (dv_delv[iii+1,jjj] + delv_emer.future[iii+1,jjj] > ulim_delv[jjj]){
    delv_emer.future[iii+1,jjj] <- ulim_delv[jjj] - SPvar
    print("error - over delv limit")
  }
}

if ((psc_SPvol[iii,jjj] + delv_var.future) > uw_){ # if actual < UpperWarning
  leadtime <- leadtime_rand()
  delv_canc.future[iii+leadtime, jjj] <- SPvar.future
  if (dv_delv[iii+1,jjj] - delv_canc.future[iii+1,jjj] < llim_delv[jjj]){
    delv_canc.future[iii+1,jjj] <- llim_delv[jjj] + SPvar
    print("error - under delv limit")
  }
}


dv_delv[iii+1,] <- dv_delv[iii+1,] + delv_emer.future[iii+1,] - delv_canc.future[iii+1,]
setDBvalues(values_ = dv_delv, param_ = 'COAL_DELIVERY_IN')
source(paste(Rcode_path,"RunCSPS.R",sep=.Platform$file.sep))
psc_SPvol <- getDBvalues(param_ = 'STOCKPILE_VOL', paramkind_ = 'RES')

