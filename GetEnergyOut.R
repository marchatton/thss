########### ENERGY SCHEDULED AND ENERGY PICKUP ####################

sink(opt_log,append = TRUE) #suppress all output to speed up processing time

########ENERGY SCHEDULED
### COAL
efsGetTypeDef(con, 'POWERSTATION') # List all typedefs of Entity POWERSTATION 
gentypedef <- efsGetTypeDef(con, 'POWERSTATION', 'COAL_PS') # Choose specific typedef by entity and key
print(gentypedef)
efsGetParamDef(con, gentypedef$ID) # List all typedef parameters for selected typedef
genparamdef <- efsGetParamDef(con, gentypedef$ID,'ENERGY_SCHED') # Choose a INPUT parameter by key
print(genparamdef)
efsQueryInstance(con, gentypedef) # NOTE: DATATYPE_KEY = NUMBER (So no INDX is used in the data table)
psc <- efsQueryInstance(con, gentypedef) # List all typedef instances
psc_tot  <- dim(psc)[1]

psc_ener_sched <- data.frame(matrix(ncol=psc_tot, nrow=interval_num))
colnames(psc_ener_sched) <- psc[,2]
instance <- efsGetInstance(con, gentypedef,psc[1,2])
result <- efsReadParam(con,gentypedef,instance$ID, genparamdef,startdate,enddate,interval,index = 'MEAN')
rownames(psc_ener_sched) <- as.Date(time(result))

# Choose Typedef instance by instance name in I_COALSUPPLY 
for (i in 1:psc_tot){
  instance <- efsGetInstance(con, gentypedef,psc[i,2])
  result <- efsReadParam(con,gentypedef,instance$ID, genparamdef,startdate,enddate,interval,index = 'MEAN')
  psc_ener_sched[as.character((instance$CAPTION))] <- result
}

### GEN
efsGetTypeDef(con, 'POWERSTATION') # List all typedefs of Entity POWERSTATION 
gentypedef <- efsGetTypeDef(con, 'POWERSTATION', 'GEN_PS') # Choose specific typedef by entity and key
print(gentypedef)
efsGetParamDef(con, gentypedef$ID) # List all typedef parameters for selected typedef
genparamdef <- efsGetParamDef(con, gentypedef$ID,'ENERGY_SCHED') # Choose a INPUT parameter by key
print(genparamdef)
efsQueryInstance(con, gentypedef) # NOTE: DATATYPE_KEY = NUMBER (So no INDX is used in the data table)

psg <- efsQueryInstance(con, gentypedef)
psg_tot  <- dim(psg)[1] #not exclude medupi

psg_ener_sched <- data.frame(matrix(ncol=psg_tot, nrow=interval_num))
colnames(psg_ener_sched) <- psg[1:psg_tot,2] #not exclude medupi
instance <- efsGetInstance(con, gentypedef,psg[1,2])
result <- efsReadParam(con,gentypedef,instance$ID, genparamdef,startdate,enddate,interval,index = 'MEAN')
rownames(psg_ener_sched) <- as.Date(time(result))

# Choose Typedef instance by instance name in I_COALSUPPLY 
for (i in 1:psg_tot){
  instance <- efsGetInstance(con, gentypedef,psg[i,2])
  result <- efsReadParam(con,gentypedef,instance$ID, genparamdef,startdate,enddate,interval,index = 'MEAN')
  psg_ener_sched[as.character((instance$CAPTION))] <- result
}

psall_ener_sched  <- cbind(psc_ener_sched, psg_ener_sched)


########ENERGY PICKUP
### COAL
efsGetTypeDef(con, 'POWERSTATION') # List all typedefs of Entity POWERSTATION 
gentypedef <- efsGetTypeDef(con, 'POWERSTATION', 'COAL_PS') # Choose specific typedef by entity and key
print(gentypedef)
efsGetParamDef(con, gentypedef$ID) # List all typedef parameters for selected typedef
genparamdef <- efsGetParamDef(con, gentypedef$ID,'ENERGY_PICKUP') # Choose a INPUT parameter by key
print(genparamdef)
efsQueryInstance(con, gentypedef) # NOTE: DATATYPE_KEY = NUMBER (So no INDX is used in the data table)
psc <- efsQueryInstance(con, gentypedef) # List all typedef instances
psc_tot  <- dim(psc)[1]

psc_ener_pickup <- data.frame(matrix(ncol=psc_tot, nrow=interval_num))
colnames(psc_ener_pickup) <- psc[,2]
instance <- efsGetInstance(con, gentypedef,psc[1,2])
result <- efsReadParam(con,gentypedef,instance$ID, genparamdef,startdate,enddate,interval,index = 'MEAN')
rownames(psc_ener_pickup) <- as.Date(time(result))

# Choose Typedef instance by instance name in I_COALSUPPLY 
for (i in 1:psc_tot){
  instance <- efsGetInstance(con, gentypedef,psc[i,2])
  result <- efsReadParam(con,gentypedef,instance$ID, genparamdef,startdate,enddate,interval,index = 'MEAN')
  psc_ener_pickup[as.character((instance$CAPTION))] <- result
}

### GEN
efsGetTypeDef(con, 'POWERSTATION') # List all typedefs of Entity POWERSTATION 
gentypedef <- efsGetTypeDef(con, 'POWERSTATION', 'GEN_PS') # Choose specific typedef by entity and key
print(gentypedef)
efsGetParamDef(con, gentypedef$ID) # List all typedef parameters for selected typedef
genparamdef <- efsGetParamDef(con, gentypedef$ID,'ENERGY_PICKUP') # Choose a INPUT parameter by key
print(genparamdef)
efsQueryInstance(con, gentypedef) # NOTE: DATATYPE_KEY = NUMBER (So no INDX is used in the data table)

psg <- efsQueryInstance(con, gentypedef)
psg_tot  <- dim(psg)[1] #not exclude medupi

psg_ener_pickup <- data.frame(matrix(ncol=psg_tot, nrow=interval_num))
colnames(psg_ener_pickup) <- psg[1:psg_tot,2] #not exclude medupi
instance <- efsGetInstance(con, gentypedef,psg[1,2])
result <- efsReadParam(con,gentypedef,instance$ID, genparamdef,startdate,enddate,interval,index = 'MEAN')
rownames(psg_ener_pickup) <- as.Date(time(result))

# Choose Typedef instance by instance name in I_COALSUPPLY 
for (i in 1:psg_tot){
  instance <- efsGetInstance(con, gentypedef,psg[i,2])
  result <- efsReadParam(con,gentypedef,instance$ID, genparamdef,startdate,enddate,interval,index = 'MEAN')
  psg_ener_pickup[as.character((instance$CAPTION))] <- result
}

psall_ener_pickup  <- cbind(psc_ener_pickup, psg_ener_pickup)



sink() #undo suppression of output
print("got energy sent out data. saved to psc_ener_sched & psc_ener_pickup")

