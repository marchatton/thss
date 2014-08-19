## Make sure these R packages is installed
library(rjson)
library(zoo)
library(RH2)
library(gsubfn)
library(zoo)
library(stringr)
library(RCurl)

# Set Timezone
Sys.setenv(TZ="GMT")
options(tz="GMT")

# !!Adjust these paths to the folder where EFS is running!!
# First Start DIAS then Run this in RStudio
THEPATH = "C:\\Users\\vharmegl\\Desktop\\EFS APP"
THEDBPATH = "C:\\Users\\vharmegl\\Desktop\\EFS APP\\e-breadboard\\resources\\za.co.enerweb_energy-flow-simulator3-var\\dbs"

# source EFS API library
source(paste(THEPATH,"efslib.r",sep=.Platform$file.sep))

########## EFS API examples ##############
## connect to the EFS database
setwd(THEDBPATH)
con = efsConnection("work");


########### COST OF SUPPLY ####################
# List all typedefs of Entity POWERSTATION 
efsGetTypeDef(con, 'POWERSTATION');
## Choose specific typedef by entity and key
gentypedef <- efsGetTypeDef(con, 'POWERSTATION', 'COAL_PS')
print(gentypedef)

## List all typedef parameters for selected typedef
efsGetParamDef(con, gentypedef$ID)

## Choose a INPUT parameter by key: COSTOFSUPPLY (INPUT)
genparamdef <- efsGetParamDef(con, gentypedef$ID,'COSTOFSUPPLY')
print(genparamdef)
# NOTE: DATATYPE_KEY = NUMBER (So no INDX is used in the data table)

## List all typedef instances
efsQueryInstance(con, gentypedef)

## Choose Typedef instance by instance name in I_COALSUPPLY 
instance <- efsGetInstance(con, gentypedef,'Matimba')
print(instance)

######## Clean Parameters #############
#status = efsClearParams(entity=gentypedef$ENTITY, type.key=gentypedef$KEY, param.keys=genparamdef$KEY, instance.id=instance$ID,con=con);
#print(status)

######## Read parameters ########
startdate <- as.POSIXct('2010-05-01') 
enddate <- as.POSIXct('2015-06-01')   
interval <- 'month' # Monthly
result = efsReadParam(con,gentypedef,instance$ID, genparamdef,startdate,enddate,interval) 
print(result)

status = efsClearParams(entity=gentypedef$ENTITY, type.key=gentypedef$KEY, param.keys=genparamdef$KEY, instance.id=instance$ID,con=con);
print(status)

######## Write parameters #######
param.value <- zoo(c(83.5,83.5,83.5,83.5,83.5,83.5,83.5), c(as.POSIXct('2013-10-01','%Y-%m-%d',tz="GMT"),
                                     as.POSIXct('2013-11-01','%Y-%m-%d',tz="GMT"), 
                                     as.POSIXct('2013-12-01','%Y-%m-%d',tz="GMT"), 
                                     as.POSIXct('2014-01-01','%Y-%m-%d',tz="GMT"),
                                     as.POSIXct('2014-02-01','%Y-%m-%d',tz="GMT"), 
                                     as.POSIXct('2014-03-01','%Y-%m-%d',tz="GMT"), 
                                     as.POSIXct('2014-04-01','%Y-%m-%d',tz="GMT")
                                     
                                     )) 
interval <- 'month'
date.seq <- seq(range(time(param.value))[1], range(time(param.value))[2], by=interval)
param.value <- na.locf(param.value, xout=date.seq)

r = efsWriteParam(con, gentypedef, instance$ID, genparamdef, param.value,compress=FALSE) 
#############################################################
######## Read parameters ########
startdate <- as.POSIXct('2010-05-01') 
enddate <- as.POSIXct('2015-06-01')   
interval <- 'month' # Monthly
result = efsReadParam(con,gentypedef,instance$ID, genparamdef,startdate,enddate,interval) 
print(result)
#################################################################


####### RUN SIMULATION ############
#http://127.0.0.1:16080/dias/ebr-cgi/RunCoalStockpileSimulation
###################################
library(RCurl);
options(RCurlOptions = list(                    
  proxyusername="x",
  proxypassword="x",                    
  proxy = "proxy.enerweb.co.za:3128",
  noproxy = "127.0.0.1",
  verbose=TRUE
));  
res = getURL(paste("http://127.0.0.1:16080/dias/ebr-cgi/RunCoalStockpileSimulation",sep=""));
lres= strsplit(res,'\n')[[1]]
r = do.call(rbind,lapply(lres,function(x) print(x)))


########### SHOW STOCKPILE VOLUMES ######################################
# List all typedefs of the Entity POWERSTATION 
efsGetTypeDef(con, 'POWERSTATION');
## Choose specific typedef by entity and key
gentypedef <- efsGetTypeDef(con, 'POWERSTATION', 'COAL_PS')
print(gentypedef)

## List all typedef parameters for selected typedef
efsGetParamDef(con, gentypedef$ID)
## Choose a OUTPUT parameter by key: STOCKPILE_VOL (OUTPUT)
genparamdef <- efsGetParamDef(con, gentypedef$ID,'STOCKPILE_VOL')
print(genparamdef)
# NOTE: INDX_DATATYPE_KEY = NUMBER (So INDX is not used in the data table)

## List all typedef instances
efsQueryInstance(con, gentypedef)

## Choose Typedef instance by instance name in I_POWERSTATION 
instance <- efsGetInstance(con, gentypedef,'Matimba')
print(instance)

######## Clean Parameters #############
#status = efsClearParams(entity=gentypedef$ENTITY, type.key=gentypedef$KEY, param.keys=genparamdef$KEY, instance.id=instance$ID,con=con,index=1);
#print(status)

######## Read parameters ########
startdate <- as.POSIXct('2010-06-01') 
enddate <- as.POSIXct('2015-06-01')   
interval <- 'month' # Monthly
# Index 1 only
result = efsReadParam(con,gentypedef,instance$ID, genparamdef,startdate,enddate,interval, index='MEAN') 
print(result)

######################################################################


########### SHOW INITIAL STOCKPILE VOLUMES ######################################
# List all typedefs of the Entity POWERSTATION 
efsGetTypeDef(con, 'POWERSTATION');
## Choose specific typedef by entity and key
gentypedef <- efsGetTypeDef(con, 'POWERSTATION', 'COAL_PS')
print(gentypedef)

## List all typedef parameters for selected typedef
params = efsGetParamDef(con, gentypedef$ID)
## Choose a OUTPUT parameter by key: INITIALSTOCK (INPUT)
genparamdef <- efsGetParamDef(con, gentypedef$ID,'INITIALSTOCK')
print(genparamdef)
# NOTE: INDX_DATATYPE_KEY = NUMBER (So INDX is not used in the data table)

## List all typedef instances
efsQueryInstance(con, gentypedef)

## Choose Typedef instance by instance name in I_POWERSTATION 
instance <- efsGetInstance(con, gentypedef,'Matimba')
print(instance)

######## Clean Parameters #############
#status = efsClearParams(entity=gentypedef$ENTITY, type.key=gentypedef$KEY, param.keys=genparamdef$KEY, instance.id=instance$ID,con=con,index=1);
#print(status)

######## Read parameters ########
startdate <- as.POSIXct('2010-06-01') 
enddate <- as.POSIXct('2015-06-01')   
interval <- 'month' # Monthly
# Index 1 only
result = efsReadParam(con,gentypedef,instance$ID, genparamdef,startdate,enddate,interval) 
print(result)

######################################################################



