delv_emer <<- psc_template
delv_canc <<- psc_template
delv_emer[,] <<- 0
delv_canc[,] <<- 0
SPvar <<- 0 
SPvar_emer <<- psc_template
dv_delv[,] <<- dv_delv_base

leadtime_ul <<- 3
leadtime_ll <<- 1
leadtime_mean <<- 2
leadtime <<- NA

leadtime_rand <<- function(){
  sample(leadtime_ll:leadtime_ul, size=1)
}

#create data.frame from the same structure as psc_template, but
#with more "leadtime_ul" more rows than psc_template.
#do this for delv and canc.
#delv future
delv_emer.future <<- data.frame(matrix(0, nrow=(interval_num-1+leadtime_ul), ncol=psc_tot))
colnames(delv_emer.future) <<- colnames(psc_template)
#canc future
delv_canc.future <<- data.frame(matrix(0, nrow=(interval_num-1+leadtime_ul), ncol=psc_tot))
colnames(delv_canc.future) <<- colnames(psc_template)

for (iii in 1:(interval_num-1)){
  for (jjj in 1:psc_tot){
    
    lwPos <<- jjj + psc_tot
    uwPos <<- jjj + 2*psc_tot
    
    if (mode=="x"){ #for x's
      lw_ <<- x[kkk, lwPos]
      uw_ <<- x[kkk, uwPos]
    }else if (mode=="mu"){ #for mu's
      lw_ <<- mus[lwPos]
      uw_ <<- mus[uwPos]
    }  
    
    #difference in future emergency deliveries and cancellation of deliveries
    delv_var.future <<- sum(delv_emer.future[(iii+1):nrow(delv_emer.future), jjj]) -
      sum(delv_canc.future[(iii+1):nrow(delv_canc.future), jjj])
    
    #SP var = actual - desired + future_delv
    SPvar <<- abs(
      psc_SPvol[iii,jjj] - 
        dv_SPinitial[jjj]
    )
    
    SPvar.future <<- abs(
      psc_SPvol[iii,jjj] - 
        dv_SPinitial[jjj] + 
        delv_var.future
    )
    
    #used for calculation of emergency costs
    SPvar_emer[iii,jjj] <<- ifelse(dv_SPinitial[jjj] - psc_SPvol[iii,jjj] > 0,
                                  dv_SPinitial[jjj] - psc_SPvol[iii,jjj],
                                  0                    
    )
    
    # emer delv
    if ((psc_SPvol[iii,jjj] + delv_var.future) < lw_){ # if actual < LowerWarning
      leadtime <<- leadtime_rand()
      delv_emer.future[iii+leadtime, jjj] <<- SPvar.future
    }
    
    # canc delv
    if ((psc_SPvol[iii,jjj] + delv_var.future) > uw_){ # if actual < UpperWarning
      leadtime <<- leadtime_rand()
      delv_canc.future[iii+leadtime, jjj] <<- SPvar.future
    }
    
    # check: over delv limit
    if (dv_delv[iii+1, jjj] + delv_emer.future[iii+1, jjj] - delv_canc.future[iii+1, jjj] > ulim_delv[jjj]){
      delv_emer.future[iii+1,jjj] <<- ulim_delv[jjj] - dv_delv[iii+1, jjj] + delv_canc.future[iii+1,jjj]
    }
    
    # check: under delv limit
    if (dv_delv[iii+1, jjj] + delv_emer.future[iii+1, jjj] - delv_canc.future[iii+1, jjj] < llim_delv[jjj]){
      delv_canc.future[iii+1,jjj] <<- llim_delv[jjj] + dv_delv[iii+1, jjj] + delv_emer.future[iii+1, jjj] 
    }
    
  }
  dv_delv[iii+1,] <<- dv_delv[iii+1,] + delv_emer.future[iii+1,] - delv_canc.future[iii+1,]
  setDBvalues(values_ = dv_delv, param_ = 'COAL_DELIVERY_IN')
  source(paste(Rcode_path,"RunCSPS.R",sep=.Platform$file.sep), local=TRUE)
  psc_SPvol <<- getDBvalues(param_ = 'STOCKPILE_VOL', paramkind_ = 'RES')
  
  
  
}


delv_emer[,] <<- delv_emer.future[1:interval_num, ]
delv_canc[,] <<- delv_canc.future[1:interval_num, ]
SPvar_emer[interval_num, ] <<- 0
