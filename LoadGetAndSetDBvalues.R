getDBvalues <- function(param_=NA, paramkind_=NA, type_='COAL_PS', ent_='POWERSTATION'){
  if (is.na(paramkind_)){
    stop("Please enter a value for paramkind_") 
  }
  if (!(paramkind_ == 'RES'|| paramkind_ == 'INP')){
    stop("Please enter paramkind_ = RES or INP") 
  }
  if (is.na(param_)){
    stop("Please enter a value for param_") 
  }

  sink("NULL") #suppress all output to speed up processing time
  
  efsGetTypeDef(con, ent_) #list all typedefs of entity
  gentypedef <- efsGetTypeDef(con, ent_, type_) # Choose specific typedef by entity and key
  #print(gentypedef)
  list_param <- efsGetParamDef(con, gentypedef$ID) #list all typedef parameters for selected typedef
  genparamdef <- efsGetParamDef(con, gentypedef$ID, param_) # Choose a INPUT (parameter)
  #print(genparamdef)
  unit_ <- genparamdef$UNIT_OF_MEASURE_KEY
  
  instances_ <- efsQueryInstance(con, gentypedef) # List all typedef instances
  instances_tot  <- dim(instances_)[1]
  
  if (paramkind_ == "INP"){ #if the parameter is an input "INP" we dont need to assign an index. see "result  <- ..."
    #create row and column names for dataframe
    temp_ <- data.frame(matrix(ncol=instances_tot, nrow=interval_num))
    colnames(temp_) <- instances_[,2]
    instance <- efsGetInstance(con, gentypedef,instances_[1,2])
    result <- efsReadParam(con,gentypedef,instance$ID, genparamdef,startdate,enddate,interval) #no index required
    
    rownames(temp_) <- as.Date(time(result))
    #create dataframe
    for (i in 1:instances_tot){
      instance <- efsGetInstance(con, gentypedef,instances_[i,2])
      result <- efsReadParam(con,gentypedef,instance$ID, genparamdef,startdate,enddate,interval)
      temp_[as.character((instance$CAPTION))] <- result
    }
  }
    
  if (paramkind_ == "RES"){ #if the parameter is an output(result "RES") we need to assign an index. see "result  <- ..."
    #create row and column names for dataframe
    temp_ <- data.frame(matrix(ncol=instances_tot, nrow=interval_num))
    colnames(temp_) <- instances_[,2]
    instance <- efsGetInstance(con, gentypedef,instances_[1,2])
    result <- efsReadParam(con,gentypedef,instance$ID, genparamdef,startdate,enddate,interval,index=sim_index[3]) #index required. 3 = MEAN. go to main_settings for more info.
    rownames(temp_) <- as.Date(time(result))
    
    #create dataframe
    for (i in 1:instances_tot){
      instance <- efsGetInstance(con, gentypedef,instances_[i,2])
      result <- efsReadParam(con,gentypedef,instance$ID, genparamdef,startdate,enddate,interval, index=sim_index[3]) #index required. 3 = MEAN
      temp_[as.character((instance$CAPTION))] <- result
    }
  }
  
  sink() #undo suppression of output
  
  print(paste("got cm_entity=", ent_,
              ", cm_typedef=", type_, 
              ", cm_paramdef=", param_, 
              ", cm_paramkind=", paramkind_, 
              ". UNIT OF MEASUREMENT=",unit_,  
              ".", sep="")
  ) 
  return(temp_)
}  


setDBvalues <- function(values_=NA, param_=NA, type_='COAL_PS', ent_='POWERSTATION'){
  if (is.na(param_)){
    stop("Please enter a value for param_") 
  }
  if (is.na(param_)){
    stop("Please enter a values_") 
  }
  if (dim(values_)[1]!=interval_num){
    stop("Please the correct type of input values_") 
  }
  
  
  sink("NULL") #suppress all output to speed up processing time
  
  efsGetTypeDef(con, ent_) #list all typedefs of entity
  gentypedef <- efsGetTypeDef(con, ent_, type_) # Choose specific typedef by entity and key
  #print(gentypedef)
  list_param <- efsGetParamDef(con, gentypedef$ID) #list all typedef parameters for selected typedef
  genparamdef <- efsGetParamDef(con, gentypedef$ID, param_) # Choose a INPUT (parameter)
  #print(genparamdef)
  unit_ <- genparamdef$UNIT_OF_MEASURE_KEY
  
  instances_ <- efsQueryInstance(con, gentypedef) # List all typedef instances
  instances_tot  <- dim(instances_)[1]
  
  ### Clean Parameters ###
  #so that there will not be duplicates when re-writing
  for (i in 1:instances_tot){
    instance <- efsGetInstance(con, gentypedef,instances_[i,2])
    status <- efsClearParams(entity=gentypedef$ENTITY, type.key=gentypedef$KEY, 
                             param.keys=genparamdef$KEY, instance.id=instance$ID,con=con)
    print(status)
  }
  
  ### VALUES TO WRITE TO DATABASE, WHICH ARE OFTEN THE DECISION VARIABLES ###
  temp_ <- values_
  
  ### WRITE PARAMETERS ###
  for (i in 1:psc_tot){
    instance <- efsGetInstance(con, gentypedef,psc[i,2])
    param.value <- zoo(temp_[,i],dates)
    date.seq <- seq(range(time(param.value))[1], range(time(param.value))[2], by=interval)
    param.value <- na.locf(param.value, xout=date.seq)
    r = efsWriteParam(con, gentypedef, instance$ID, genparamdef, param.value,compress=FALSE) 
  }
  
  sink()
  
  print(paste("set cm_entity=", ent_,
              ", cm_typedef=", type_, 
              ", cm_paramdef=", param_,  
              " to values_",
              sep=sep_)
  )
}