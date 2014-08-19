###### GENERATES TEMPLATES USED FOR READING FROM AND WRITING TO DATABASES
#TEMPLATES ARE IN THE FORM OF DATA.FRAME's
#USES POWERSTATION CAPACITY TO CREATE THE TEMPLATES 

sink(opt_log, append=T) #Suppress output to console. Send all output to a log file.

###PSC - Power Station Coal
efsGetTypeDef(con, 'POWERSTATION') # List all typedefs of Entity POWERSTATION 
gentypedef <- efsGetTypeDef(con, 'POWERSTATION', 'COAL_PS') # Choose specific typedef by entity and key
print(gentypedef)
Listalltypedef <- efsGetParamDef(con, gentypedef$ID) # List all typedef parameters for selected typedef
genparamdef <- efsGetParamDef(con, gentypedef$ID,'CAPACITY') # Choose a INPUT parameter by key
print(genparamdef)
efsQueryInstance(con, gentypedef) # NOTE: DATATYPE_KEY = NUMBER (So no INDX is used in the data table)
psc <- efsQueryInstance(con, gentypedef) # List all typedef instances
psc_tot  <- dim(psc)[1]
psc_template <- data.frame(matrix(ncol=psc_tot, nrow=interval_num))
psc_template[,] <- NA
colnames(psc_template) <- psc[,2]
instance <- efsGetInstance(con, gentypedef,psc[1,2])
result <- efsReadParam(con,gentypedef,instance$ID, genparamdef,startdate,enddate,interval)
rownames(psc_template) <- as.Date(time(result))

###PSG - Power Station General
efsGetTypeDef(con, 'POWERSTATION') # List all typedefs of Entity POWERSTATION 
gentypedef <- efsGetTypeDef(con, 'POWERSTATION', 'GEN_PS') # Choose specific typedef by entity and key
print(gentypedef)
efsGetParamDef(con, gentypedef$ID) # List all typedef parameters for selected typedef
genparamdef <- efsGetParamDef(con, gentypedef$ID,'CAPACITY') # Choose a INPUT parameter by key
print(genparamdef)
efsQueryInstance(con, gentypedef) # NOTE: DATATYPE_KEY = NUMBER (So no INDX is used in the data table)
psg <- efsQueryInstance(con, gentypedef)
psg_tot  <- dim(psg)[1] 
psg_template <- data.frame(matrix(ncol=psg_tot, nrow=interval_num))
psg_template[,] <- NA
colnames(psg_template) <- psg[1:psg_tot,2] 
instance <- efsGetInstance(con, gentypedef,psg[1,2])
result <- efsReadParam(con,gentypedef,instance$ID, genparamdef,startdate,enddate,interval)
rownames(psg_template) <- as.Date(time(result))

###PSA - Power Station All
psa_template <- cbind(psc_template,psg_template)
psa <- rbind(psc,psg)
psa_tot <- dim(psa)[1]

sink() #undo suppression of output
print("created psc_template, psg_template, psa_template.")
print("psc, psc_tot, psg, psg_tot, psa, psa_tot were also created.")
