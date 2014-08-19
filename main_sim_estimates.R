source(paste(Rcode_path,"RunCSPS.R",sep=.Platform$file.sep))

psc_delvin <- getDBvalues(param_ = 'COAL_DELIVERY_IN', paramkind_='INP')
psc_delvout <- getDBvalues(param_ = 'COAL_DELIVERY_OUT', paramkind_ = 'RES')
psc_burnin <- getDBvalues(param_ = 'COAL_BURN_IN', paramkind_='INP')
psc_burnout <- getDBvalues(param_ = 'COAL_BURN_OUT', paramkind_ = 'RES')

