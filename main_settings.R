###### Make sure these R packages are installed
library(rjson)
library(zoo)
library(RH2)
library(gsubfn)
library(stringr)
library(RCurl)
library(truncnorm)

###### set variable for the character to be used as a separator in paste(...,sep=sep_). Dependant on Operating System.
sep_ <- .Platform$file.sep #for the sake of laziness

###### Set Timezone
Sys.setenv(TZ="GMT")
options(tz="GMT")

###### Range for reading to and writing from the database
#Define the startdate and enddate range, for reading from and writing to the database
startdate <- as.POSIXct('2013-08-01')
enddate <- as.POSIXct('2014-03-01')   
interval <- 'month' #Month
if (interval=='month'){
  interval_num  <- round((as.yearmon(enddate) - as.yearmon(startdate))*12) + 1
}else if (interval=='month'){
  interval_num <- enddate - startdate + 1
}
dates <- seq(startdate,enddate,interval)

###### Source EFS API library - used for communicating between R and the database
source(paste(Rcode_path,"efslib.R",sep=sep_))

###### connect to the EFS database
setwd(THEDBPATH)
con = efsConnection("work");

###### filepaths used for the optimiser (for reading & writing)
# includes settings to store files uniquely so that they arent written over in successive experiments.
start_date_time <- format(tic, "%Yy%mm%dd-%Hh%Mm%Ss")
#optimser path
optPath <- file.path(paste(THEPATH,"CSPS_optimiser_output",sep=sep_)) 
#optimiser estimate path. Used for baseline delivery.
optEstPath <- file.path(paste(optPath, "estimates",sep=sep_))

###### create directories to store results
#create optimser directory
dir.create(optPath, showWarnings = FALSE) 
#create optimser estimate directory
dir.create(optEstPath, showWarnings = FALSE)

###### custom function for writing optimiser outputs. Call write.row(x).
writepath <- file.path(optPath,paste(func_name,"_",start_date_time, sep=""))
write.row <- function(x, file = writepath, append = TRUE, quote = TRUE, sep=",", 
                      eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = FALSE,   
                      qmethod = c("escape", "double"), fileEncoding = ""){
  write.table(as.matrix(t(x)), paste(file,".csv",sep=""), append , quote, sep, 
              eol, na, dec, row.names, col.names, 
              qmethod, fileEncoding)
}

###### simulation settings
sim_index <- c("MIN", "PERC0", "MEAN", "PERC1", "MAX") #(min, 5th perc, mean, 95th perc, max)
CM_sim_settings <- function(){dbGetQuery(con, "select * from CM_SIMULATION")}
sim_iter <- 1000
sim_seed <- runif(maxIterations,-2147483648,2147483647) 

changeSimSet <- function(seed=CM_sim_settings()$SEED,iter=sim_iter){
  dbSendUpdate(con, str_c("UPDATE CM_SIMULATION SET ",
    "ITERATIONS = "    , iter,  ", " ,
    "SEED = "          , seed  
    )) 
}
changeSimSet(iter=20)

###### estimator names
est_names <- c("est_delvin", "est_delvout", "est_burnin", "est_burnout")

###### generate power station templates. makes it easier to read from and write to the database.
source(paste(Rcode_path,"templates_powerstations.R",sep=sep_))

###### load get and set db values. used for creating dataframes for reading from and writing to databases.
source(paste(Rcode_path,"LoadGetAndSetDBvalues.R",sep=sep_))

