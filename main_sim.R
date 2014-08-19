corr_form_dv_SPinitial <- psc_template #getting dv_SPinitial into the correct format for writing to the database.
corr_form_dv_SPinitial[,] <- 0
corr_form_dv_SPinitial[1,] <- dv_SPinitial

corr_form_dv_SPinitial[1, -1] <- 10000   #@@ only looking at Arnot


setDBvalues(values_ = corr_form_dv_SPinitial, param_ = 'INITIALSTOCK')

setDBvalues(values_ = dv_delv, param_ = 'COAL_DELIVERY_IN')

source(paste(Rcode_path,"RunCSPS.R",sep=.Platform$file.sep))

# psc_SPinitial <- getDBvalues(param_ = 'INITIALSTOCK', paramkind_='INP')

psc_SPvol <- getDBvalues(param_ = 'STOCKPILE_VOL', paramkind_ = 'RES')
