corr_form_dv_SPinitial <- psc_template #getting dv_SPinitial into the correct format for writing to the database.
corr_form_dv_SPinitial[,] <- 0
corr_form_dv_SPinitial[1,] <- dv_SPinitial

# give any stockpiles not being optimised an arbitrarily large initial stockpile. (e.g. 10000)
corr_form_dv_SPinitial[1, -options.ps] <- 10000 #notice the "-"

setDBvalues(values_ = corr_form_dv_SPinitial, param_ = 'INITIALSTOCK')

# only change deliveries if working with all 3 decision variables
if (options.dv == 3){
  setDBvalues(values_ = dv_delv, param_ = 'COAL_DELIVERY_IN')
}

source(paste(Rcode_path,"RunCSPS.R",sep=.Platform$file.sep), local=TRUE)

psc_SPvol <- getDBvalues(param_ = 'STOCKPILE_VOL', paramkind_ = 'RES')
