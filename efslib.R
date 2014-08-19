library(RH2)
library(gsubfn)
library(zoo)
library(stringr)
#if(Sys.timezone()=="") Sys.setenv(TZ="SAST")

MASTERDB_ <- '/tmp/master'

dbfile.from.url <- function(url) fn$paste('`url`.h2.db')
url.from.dbfile <- function(dbname) sub('.h2.db','',dbname)
entity.to.inst <- function(entity) paste('I_',as.character(entity),sep='')
entity.to.col.id <- function(entity) paste(as.character(entity),'_ID',sep='')
entity.from.col.id <- function(colname) { parts <- strsplit(colname,'_')[[1]]; paste(parts[1:(length(parts)-1)], collapse='_')}

entity.to.par <- function(entity) paste('I_',as.character(entity),'_PAR',sep='')
entity.to.out <- function(entity) paste('I_',as.character(entity),'_OUT',sep='')

entity.to.rel <- function(entity) paste('R_',as.character(entity), sep='')
entity.to.rpar <- function(entity) paste('R_',as.character(entity),'_PAR')
entity.to.rout <- function(entity) paste('R_',as.character(entity),'_OUT')

genparamdef.to.tbl <- function(gentypedef, genparamdef) {
  if(genparamdef$PARAMKIND_KEY=='DENORM') return (genparamdef$PARAM_TABLE)
  
  if(genparamdef$PARAMKIND_KEY=='INPUT') { suffix = '_PAR' } else suffix = '_OUT'
  if('TYPEDEF_ID' %in% names(genparamdef) ) prefix ='I_' else prefix ='R_'
  return(paste(prefix,as.character(gentypedef$ENTITY),suffix,sep="" ))
}

compress.zoo <-function(param.value) {
  if(length(param.value)>1) {
    # compress it
    param.value.diff <- diff(param.value)      
    param.comp <- c(param.value[1], param.value[param.value.diff!=0])
  } else {
    param.comp <- param.value
  }  
  return(param.comp)
}

uncompress.zoo <-function(rs.zoo, startdate, enddate, interval) {
  date.seq <- seq(startdate, enddate, by=interval)
  r <- na.locf(merge(rs.zoo,zoo(NA,date.seq))$rs.zoo, na.rm=FALSE) 
  attr(index(r), "tzone") <- Sys.timezone()   
  
  return(r)
}

#efsMakeMaster <- function(filename){
#        fn$system("sqlite3 `filename` '.read make_efs.sql'")
#}

efsMakeDb <- function(dbname, masterdb, overwrite=FALSE){
  dbname.file <- dbfile.from.url(dbname)
  if(overwrite) unlink(dbname.file)
  masterdb.file <- dbfile.from.url(masterdb)
  
  if(!file.exists(dbname.file)) {
    if(!file.exists(masterdb.file)) fn$stop('The master database file `masterdb.file` could not be found')
    file.copy(masterdb.file, dbname.file)
  }
}

efsConnection <- function(dbname, masterdb=NULL){
  dbname.file <- dbfile.from.url(dbname)
  print(dbname.file)
  if(!file.exists(dbname.file)) {
    if(is.null(masterdb)) fn$stop('The database `dbname` does not exist and no masterdb was specified')
    efsMakeDb(dbname, masterdb)
  }
  
  fn$dbConnect(H2(), 'jdbc:h2:`dbname`;AUTO_SERVER=TRUE','sa','')  
}

############################ TypeDef & RelTypeDef ####################################


efsGetGenDef <- function(con, entity,key=NULL, type=c('TYPEDEF','RELTYPEDEF')) { 
  keysql <- ifelse(is.null(key), "", str_c(" and KEY='",key,"'")) 
  key <- ifelse(is.null(key),'None',key)
  
  rs <- dbGetQuery(con, str_c("select * from CM_",type," where ENTITY='",entity,"' ",keysql))
  if(nrow(rs)==0) fn$stop('No type def found: Invalid entity:`entity` or key:`key` specified')
  return(rs)
}


efsGetTypeDef <- function(con, entity,key=NULL) {  
  return(efsGetGenDef(con, entity, key, 'TYPEDEF'))
}

efsGetRelTypeDef <- function(con, entity,key=NULL) {  
  return(efsGetGenDef(con, entity, key, 'RELTYPEDEF'))
}

efsGetGenDef.byid <- function(con, id, type=c('TYPEDEF','RELTYPEDEF')) { 
  rs <- dbGetQuery(con, str_c("select * from CM_",type," where ID=",id))
  if(nrow(rs)==0) fn$stop('No type def found: Invalid ID:`id` specified')
  return(rs)
}

efsGetTypeDef.byid <- function(con,id) efsGetGenDef.byid(con,id, 'TYPEDEF')
efsGetRelTypeDef.byid <- function(con,id) efsGetGenDef.byid(con,id, 'RELTYPEDEF')


############################ ParamDef & RelParamDef ####################################
efsGetGenParamDef <- function(con, def_id, key=NULL, type=c('PARAMDEF','RELPARAMDEF')) {  
  parent <- ifelse(type=='PARAMDEF', 'TYPEDEF', 'RELTYPEDEF')
  keysql <- ifelse(is.null(key), "", str_c(" and KEY='",key,"'"))
  key <- ifelse(is.null(key),'None',key)
  
  rs <- dbGetQuery(con, str_c("select * from CM_",type," where ",parent,"_ID=",def_id," ",keysql))
  if(nrow(rs)==0) fn$stop('No param def found: Invalid `parent`_id=:`def_id` or key:`key` specified')
  return(rs)
}

efsGetParamDef <- function(con, def_id, key=NULL) {  
  return(efsGetGenParamDef(con, def_id, key, 'PARAMDEF'))
}

efsGetRelParamDef <- function(con, def_id, key=NULL) {  
  return(efsGetGenParamDef(con, def_id, key, 'RELPARAMDEF'))
}


############################ Auxiliary ####################################
efsGetUOM <- function(con, key=NULL) {
  keysql <- ifelse(is.null(key), "", str_c(" where KEY='",key,"'"))
  key <- ifelse(is.null(key),'None',key)
  
  rs <- dbGetQuery(con, str_c("select * from CM_UNITOFMEASURE  ",keysql))
  if(nrow(rs)==0 ) fn$stop('No unit of measure found: key=`key`')
  return(rs)
}

efsGetEnum <- function(con, datatype_key, key=NULL) {
  keysql <- ifelse(is.null(key), "", str_c(" and KEY='",key,"'"))
  key <- ifelse(is.null(key),'None',key)
  
  rs <- dbGetQuery(con, str_c("select * from CM_ENUM where DATATYPE_KEY='",datatype_key,"' ",keysql))
  if(nrow(rs)==0) fn$stop('No enum found: key=`key`')
  return(rs)
}


############################ Instances ####################################
efsClearInstance <- function(con, typedef, instance.id){
  inst.tbl <- entity.to.inst(typedef$ENTITY)  
  r <- dbSendUpdate(con, str_c("delete from ",inst.tbl," where ID=",instance.id))
  return(TRUE)
}

efsNewInstance <- function(con, typedef, caption){
  inst.tbl <- entity.to.inst(typedef$ENTITY)  
  typedef.id <- typedef$ID
  
  r <- dbSendUpdate(con, str_c("insert into ",inst.tbl," (CAPTION, TYPEDEF_ID) values ('",caption,"',",typedef.id,")"))
  r <- dbGetQuery(con,"SELECT IDENTITY() ID")
  
  return(r)
}

efsGetInstance <- function(con, typedef, caption){
  inst.tbl <- entity.to.inst(typedef$ENTITY)  
  typedef.id <- typedef$ID
  
  r <- dbGetQuery(con, str_c("SELECT * FROM ",inst.tbl," where TYPEDEF_ID = ",typedef.id," and CAPTION='",caption,"'"))  
  return(r)
}

efsGetInstance.byid <- function(con, entity, id){
  inst.tbl <- entity.to.inst(entity)  
  
  r <- dbGetQuery(con, str_c("SELECT * FROM ",inst.tbl," where ID = ",id))  
  return(r)
}


efsQueryInstance <- function(con, typedef, caption=NULL, ids=NULL){
  inst.tbl <- entity.to.inst(typedef$ENTITY) 
  typedef.id <- typedef$ID
  
  caption.sql <- ifelse(is.null(caption), '' , str_c("and CAPTION='",caption,"'") )
  ids.sql <- ifelse(is.null(ids), '' , str_c("and ID in (",toString(ids),")"))
  
  r <- dbGetQuery(con, str_c("SELECT * FROM ",inst.tbl," where TYPEDEF_ID = ",typedef.id," ",caption.sql," ",ids.sql))  
  return(r)
}



############################ Relationships ####################################
efsClearRel <- function(con, reltypedef, rel.id){
  rel.tbl <- entity.to.rel(reltypedef$ENTITY)  
  r <- dbSendUpdate(con, str_c("delete from ",rel.tbl," where ID=",rel.id))
  return(TRUE)
}

efsGetRelInfo <- function(con, reltypedef, ids) {
  reltypedef.id <- reltypedef$ID
  
  from.typedef <-efsGetTypeDef.byid(con, reltypedef$FROM_ID)
  to.typedef <-efsGetTypeDef.byid(con, reltypedef$TO_ID)
  
  from.entity <- as.character(from.typedef$ENTITY)
  to.entity <-  as.character(to.typedef$ENTITY)
  
  from.col <- entity.to.col.id(from.entity)
  to.col <- entity.to.col.id(to.entity)
  
  if(!from.entity %in% names(ids)) fn$stop('No id specified for `from.entity`')
  if(!to.entity %in% names(ids)) fn$stop('No id specified for `to.entity`')
  
  from.id <- ids[from.entity]
  to.id <- ids[to.entity]
  
  list(from.col=from.col, to.col=to.col, from.id=from.id, to.id=to.id)
  
}

efsNewRel <- function(con, reltypedef, ids){
  rel.tbl <- entity.to.rel(reltypedef$ENTITY)  
  reltypedef.id <- reltypedef$ID
  info <- efsGetRelInfo(con, reltypedef, ids)
  
  r <- dbSendUpdate(con, str_c("insert into ",rel.tbl," (RELTYPEDEF_ID,",info$from.col,",",info$to.col,") values (",reltypedef.id,",",info$from.id,",",info$to.id,")"))
  r <- dbGetQuery(con,"SELECT IDENTITY() ID")
  
  return(r)
}

efsGetRel <- function(con, reltypedef, ids){
  rel.tbl <- entity.to.rel(reltypedef$ENTITY)  
  reltypedef.id <- reltypedef$ID
  info <- efsGetRelInfo(con, reltypedef, ids)
  
  r <- dbGetQuery(con, str_c("select * from  ",rel.tbl," where ",info$from.col,"=",info$from.id," and ",info$to.col,"=",info$to.id)) 
  
  return(r)
}

efsQueryRel <- function(con, reltypedef){
  rel.tbl <- entity.to.rel(reltypedef$ENTITY) 
  reltypedef.id <- reltypedef$ID
  
  r <- dbGetQuery(con, str_c("SELECT * FROM ",rel.tbl," where RELTYPEDEF_ID = ",reltypedef.id))  
  return(r)
}

efsLookup.id <- function(rels, convert=FALSE, ...) {
  relcols <- names(rels)[-which(names(rels) %in% c('ID','RELTYPEDEF_ID'))]
  
  ins <- list(...)
  if(length(ins)!=1) stop('Specify exactly one lookup key value pair')
  
  keyname <- entity.to.col.id(names(ins))
  keyval <- ins[[1]]
  lookupkey <- relcols[-which(relcols==keyname)]
  
  r <- rels[rels[,keyname]==as.numeric(keyval),]
  result <- r[, lookupkey]
  
  if(convert) result <- as.character(result)
  result <- list(result)
  names(result) <- entity.from.col.id(lookupkey) 
  
  return(result)
  
}



############################ Gen = Abstraction of Instance and Relationships #######
efsClearGen <- function(con, gentypedef, gen.id, type=c('TYPEDEF','RELTYPEDEF')){
  if(type[1]=='TYPEDEF') {
    efsClearInstance(con, gentypedef, gen.id)
  } else {
    efsClearRel(con, gentypedef, gen.id)
  }     
}

############################ Parameters ####################################

efsClearGenParam <- function(con, gentypedef, id, genparamdef, index=NULL, all.index=FALSE, type=c('PARAMDEF','RELPARAMDEF')) {
  index.sql <- ifelse(is.null(index), "and INDX is null ",str_c("and INDX ='",index,"'"))
  index.sql <- ifelse(all.index,'', index.sql)
  par.tbl <- genparamdef.to.tbl(gentypedef, genparamdef)
  inst.col <- entity.to.col.id(gentypedef$ENTITY)
  param.key <- as.character(genparamdef$KEY)
  
  if(genparamdef$PARAMKIND_KEY =='DENORM') {
    fn$stop('Cannot clear a parameter in a denormalised table: `par.tbl`')
  } else {
    r <- dbSendUpdate(con, str_c("delete from ",par.tbl," where ",inst.col,"=",id," and KEY='",param.key,"' ",index.sql))
  }
  return(TRUE)
}


efsClearParam <- function(con, typedef, instance.id, paramdef, index=NULL, all.index=FALSE) efsClearGenParam(con, typedef, instance.id, paramdef, index, all.index, type='PARAMDEF')
efsClearRelParam <- function(con, reltypedef, rel.id, relparamdef, index=NULL, all.index=FALSE) efsClearGenParam(con, reltypedef, rel.id, relparamdef, index, all.index, type='RELPARAMDEF')

efsWriteGenParam <- function(con, gentypedef, id, genparamdef, param.value, index=NULL, type=c('PARAMDEF','RELPARAMDEF'), compress=TRUE, csv.state = NULL) {
  insert.it <- function(date, val, index.sql) {
    datestr <- format(date, format='%Y-%m-%d %H:%M:%S') #, tz='SAST')
    sql <- paste("insert into ",par.tbl," (APPLDATETIME,",parent,"_ID, KEY,VAL, INDX, ",inst.col,") values ('",datestr,"',",typedef.id,",'",param.key,"','",val,"',",index.sql,",  ",id,")", sep="")  
    print(sql)
    r <- dbSendUpdate(con,sql)
    return(TRUE)
  }
  
  if(class(param.value)!='zoo') stop('Only objects of class zoo supported')
  
  parent <- ifelse(type=='PARAMDEF', 'TYPEDEF', 'RELTYPEDEF')
  par.tbl <- genparamdef.to.tbl(gentypedef, genparamdef)
  inst.col <- entity.to.col.id(gentypedef$ENTITY)
  param.key <- as.character(genparamdef$KEY)
  typedef.id <- gentypedef$ID 
  
  if(genparamdef$PARAMKIND_KEY =='DENORM') fn$stop('Cannot write a single parameter into a DENORMALISED table:`par.tbl`')
  
  
  if(genparamdef$DATATYPE_KEY %in% c('NUMBER','TEXT', 'INTEGER', 'BOOLEAN','NUM_ARRAY')) {
    if(length(param.value)>1 & compress) {
      # compress it
      param.value.diff <- diff(param.value)      
      param.comp <- c(param.value[1], param.value[param.value.diff!=0])
    } else {
      param.comp <- param.value
    }  
     
    if(is.null(csv.state)){
      index.sql <- ifelse(is.null(index),'NULL',str_c("'",index,"'"))
      lapply(1:length(param.comp),function(x) insert.it(index(param.comp[x]), param.comp[x], index.sql)) 
    } else {
      index.val <- ifelse(is.null(index),NA,index)
      df <- data.frame(format(index(param.comp),format='%Y-%m-%d %H:%M:%S'), typedef.id, param.key, param.comp,index.val, id)
      write.table(df, file=csv.state$file, append=csv.state$op != 'NEW', row.names=FALSE, col.names=FALSE, na="NULL", sep=",")
      if( csv.state$op == 'UPDATEDB') {
        close(csv.state$file)
        colnames <- str_c("APPLDATETIME,",parent,"_ID,KEY,VAL,INDX,",inst.col)
        dbSendUpdate(con,paste("insert into ",par.tbl," (",colnames,") select * from CSVREAD('",csv.state$filename,"','",colnames,"')",sep="") )     
      }
      rep('TRUE',nrow(df))
      
    }
  }  else {
    datatype <- genparamdef$DATATYPE_KEY  
    fn$stop('Data type not supported:`datatype`')
  }    
}

efsWriteParam <- function(con, typedef, instance.id, paramdef, param.value, index=NULL, compress=TRUE, csv.state = NULL)  {
  efsWriteGenParam(con, typedef, instance.id, paramdef, param.value, index=index, type='PARAMDEF', compress=compress, csv.state=csv.state)
}

efsWriteRelParam <- function(con, reltypedef, rel.id, relparamdef, param.value, index=NULL, compress=TRUE, csv.state = NULL) {
  efsWriteGenParam(con, reltypedef, rel.id, relparamdef, param.value, index=index, type='RELPARAMDEF', compress=compress, csv.state=csv.state)
}

efsReadGenParam <- function(con, gentypedef, id, genparamdef, startdate, enddate, interval, index=NULL,uncompress=TRUE, type=c('PARAMDEF','RELPARAMDEF')) {
  result.to.zoo <- function(rs){    
    if(nrow(rs)>0) {
      rs.zoo <- zoo(as.numeric(rs[,2]),as.POSIXct(rs[,1], format='%Y-%m-%d %H:%M:%S'))      
      if(uncompress){
        r <- na.locf(merge(rs.zoo,zoo(NA,date.seq))$rs.zoo, na.rm=FALSE) 
        attr(index(r), "tzone") <- Sys.timezone()         
      } else {
        r <- rs.zoo 
      }  
      r <- subset(r, index(r)>=startdate & index(r) <=enddate)
      
    } else {
      r <- zoo(rep(NA,length(date.seq)), date.seq)
    } 
    return(r)
  }
  
  
  readQuery <- function(con,sql, use.csv){
    if(use.csv){
      tmpfile <- tempfile()
      new.sql <- paste("CALL CSVWRITE('",tmpfile,"', '",gsub("'","''",sql),"');",sep="")  
      print(new.sql)
      dbSendUpdate(con, new.sql)
      rs <- read.csv(tmpfile)
      unlink(tmpfile)
    } else {
      rs <- dbGetQuery(con,sql)
    }
    return(rs)
  }
  
  
  if(is.null(index)) {
    index.sql <- "and INDX is NULL"
  } else if (index=='ALL'){
    index.sql <- "" 
  } else {
    index.sql <- str_c("and INDX ='",index,"'")
  }
  
  if(uncompress){
    start.sql <- ""
  } else {     
    start.sql <- str_c(" and APPLDATETIME >= '",format(startdate, format='%Y-%m-%d %H:%M:%S'),"'")
  }
  
  date.seq <- seq(startdate, enddate, by=interval)
  
  par.tbl <- genparamdef.to.tbl(gentypedef, genparamdef)
  inst.col <- entity.to.col.id(gentypedef$ENTITY)
  param.key <- as.character(genparamdef$KEY)
  
  if(genparamdef$DATATYPE_KEY %in% c('NUMBER', 'INTEGER', 'BOOLEAN','NUM_ARRAY')) {
    if(genparamdef$PARAMKIND_KEY =='DENORM') {
      sql <- str_c("select distinct FORMATDATETIME(APPLDATETIME,'yyyy-MM-dd HH:mm:ss'), ",param.key," as VAL, INDX from  ",par.tbl," where ",inst.col,"=",id,"  ",index.sql,start.sql," and APPLDATETIME <= '",format(enddate, format='%Y-%m-%d %H:%M:%S'),"' order by 1")
      rs <- readQuery(con,sql, genparamdef$DATATYPE_KEY=='NUM_ARRAY')
    } else { 
      #sql <- str_c("select distinct FORMATDATETIME(APPLDATETIME,'yyyy-MM-dd HH:mm:ss'), CONVERT(VAL,double) VAL, CONVERT(INDX,integer) INDX from  ",par.tbl," where ",inst.col,"=",id," and KEY='",param.key,"' ",index.sql,start.sql," and APPLDATETIME <= '",format(enddate, format='%Y-%m-%d %H:%M:%S'),"' order by 1")
      sql <- str_c("select distinct FORMATDATETIME(APPLDATETIME,'yyyy-MM-dd HH:mm:ss'), CONVERT(VAL,double) VAL, INDX from  ",par.tbl," where ",inst.col,"=",id," and KEY='",param.key,"' ",index.sql,start.sql," and APPLDATETIME <= '",format(enddate, format='%Y-%m-%d %H:%M:%S'),"' order by 1")
      rs <- readQuery(con,sql, genparamdef$DATATYPE_KEY=='NUM_ARRAY')      
    }    
    print(sql);
    print(rs);
    debugdf4 <<- rs;  
    
    
    if(genparamdef$DATATYPE_KEY %in% c('NUM_ARRAY')) {
      indx.u <- unique(rs[,3])
      r <- lapply(indx.u, function(x) result.to.zoo(subset(rs,INDX==x)))
    } else {
      r <- result.to.zoo(rs)
    }
    
    
  }  else {
    datatype <- genparamdef$DATATYPE_KEY  
    fn$stop('Data type not supported:`datatype`')
  }    
  
  return(r)   
  
}

efsReadParam <- function(con, gentypedef, id, paramdef, startdate, enddate, interval, index=NULL) {
  efsReadGenParam(con, gentypedef, id, paramdef, startdate, enddate, interval, index=index, type='PARAMDEF')
}
efsReadRelParam <- function(con, reltypedef, rel.id, relparamdef, startdate, enddate, interval, index=NULL) {
  efsReadGenParam(con, reltypedef, rel.id, relparamdef, startdate, enddate, interval, index=index, type='RELPARAMDEF')
}



############################ High Level ####################################
efsInfo <- function(type=c('TYPEDEF','RELTYPEDEF'),entity, type.key, con=NULL, dbname=NULL, gen.id=NULL, descriptor=NULL, create.new=FALSE, gentypedef=NULL,genparamdefs=NULL ) {
  if(is.null(con)) {
    if(is.null(dbname)) stop('Either con or dbname need to be specified. Both empty')
    con <- efsConnection(dbname, MASTERDB_)
    close.con <- TRUE
  } else {
    close.con <- FALSE    
  }
  
  if(is.null(gentypedef)) gentypedef <- efsGetGenDef(con, entity,type.key, type)
  if(is.null(gen.id)) {
    if(is.null(descriptor)) stop('Either id or descriptor need to be specified.  Both empty')
    if(type=='TYPEDEF') {            
      r <- efsGetInstance(con, gentypedef, descriptor)
      if(nrow(r)==0) {
        if(create.new) r <- efsNewInstance(con,gentypedef, descriptor)
        else fn$stop('No id supplied and 0 rows for caption : `descriptor`')
      }
    } else {
      r <- efsGetRel(con, gentypedef, descriptor)
      if(nrow(r)==0) {
        if(create.new) r <- efsNewRel(con,gentypedef, descriptor)
        else fn$stop('No id supplied and 0 rows for ids : `descriptor`')
      }
    }
    gen.id <- r$ID 
  }
  
  par.type <- ifelse(type=='TYPEDEF','PARAMDEF','RELPARAMDEF')
  if(is.null(genparamdefs)) genparamdefs <- efsGetGenParamDef(con, gentypedef$ID,type=par.type)
  
  
  return(list(con=con, close.con=close.con, gentypedef=gentypedef, gen.id = gen.id, genparamdefs=genparamdefs, par.type=par.type))
  
}


efsGenClearParams <- function(type=c('TYPEDEF','RELTYPEDEF'),entity, type.key, param.keys, con=NULL, dbname=NULL, gen.id=NULL, descriptor=NULL,gentypedef=NULL, index=NULL, all.index=FALSE) {
  
  info <- efsInfo(type, entity, type.key, con, dbname, gen.id, descriptor, gentypedef=gentypedef)   
  
  if(sum(param.keys %in% info$genparamdefs$KEY) != length(param.keys)) {
    not.found <- param.keys[!(param.keyss %in% info$genparamdefs$KEY)]
    not.found <- toString(not.found)
    fn$stop('The following names could not be matched to paramdef:`not.found`')             
  }
  
  for(i in 1:nrow(info$genparamdefs)){
    
    paramdef <- info$genparamdefs[i,]
    if(paramdef$KEY %in% param.keys) {   
      if(paramdef$PARAMKIND_KEY == 'DENORM') {
        inst.col <- entity.to.col.id(info$gentypedef$ENTITY)
        dbSendUpdate(info$con,paste('delete from ', paramdef$PARAM_TABLE,' where ',inst.col,'=',info$gen.id))
      } else {
        efsClearGenParam(info$con, info$gentypedef, info$gen.id, paramdef, index, all.index, info$par.type)   
      } 
    } 
    
  }
  
  if(info$close.con) dbDisconnect(info$con)
  return(TRUE)
}

efsClearParams <- function(entity, type.key, param.keys, con=NULL, dbname=NULL, instance.id=NULL, instance.caption=NULL, typedef=NULL, index=NULL, all.index=FALSE) {
  efsGenClearParams('TYPEDEF',entity, type.key, param.keys, con, dbname, instance.id, instance.caption, gentypedef=typedef, index, all.index)
}

efsRelClearParams <- function(entity, type.key, param.keys, con=NULL, dbname=NULL, rel.id=NULL, ids=NULL, reltypedef=NULL, index=NULL, all.index=FALSE) {
  efsGenClearParams('RELTYPEDEF',entity, type.key, param.keys, con, dbname, rel.id, ids, gentypedef=reltypedef,index, all.index)
}


efsGenClear <- function(type=c('TYPEDEF','RELTYPEDEF'),entity, type.key, con=NULL, dbname=NULL, gen.id=NULL, descriptor=NULL,gentypedef=NULL, index=NULL, all.index=FALSE) {
  
  info <- efsInfo(type, entity, type.key, con, dbname, gen.id, descriptor, gentypedef=gentypedef)   
  
  param.keys <- info$genparamdefs$KEY
  
  efsGenClearParams(type, entity, type.key, param.keys, con, dbname, gen.id, descriptor, gentypedef, index, all.index)
  
  efsClearGen(info$con, info$gentypedef, info$gen.id, type)
  if(info$close.con) dbDisconnect(info$con)
  return(TRUE)
}

efsClear <- function(entity, type.key, con=NULL, dbname=NULL, instance.id=NULL, instance.caption=NULL, typedef=NULL, index=NULL, all.index=FALSE) {
  efsGenClear('TYPEDEF',entity, type.key, con, dbname, instance.id, instance.caption, gentypedef=typedef, index, all.index)
}

efsRelClear <- function(entity, type.key, con=NULL, dbname=NULL, rel.id=NULL, ids=NULL, reltypedef=NULL, index=NULL, all.index=FALSE) {
  efsGenClear('RELTYPEDEF',entity, type.key, con, dbname, rel.id, ids, gentypedef=reltypedef, index, all.index)
}


efsGenWrite <- function(type=c('TYPEDEF','RELTYPEDEF'),data, entity, type.key, con=NULL, dbname=NULL, gen.id=NULL, descriptor=NULL, index=NULL, gentypedef=NULL,genparamdefs=NULL, compress=TRUE, csv.state = NULL){
  
  info <- efsInfo(type, entity, type.key, con, dbname, gen.id, descriptor, create.new=TRUE, gentypedef=gentypedef,genparamdefs=genparamdefs) 
  
  data.names <- names(data)
  if(sum(data.names %in% info$genparamdefs$KEY) != length(data.names)) {
    not.found <- data.names[!(data.names %in% info$genparamdefs$KEY)]
    not.found <- toString(not.found)
    fn$stop('The following names could not be matched to paramdef:`not.found`')             
  }
  
  
  for(i in 1:length(data.names)){
    paramdef <- subset(info$genparamdefs, KEY==data.names[i])
    r <- efsWriteGenParam(info$con, info$gentypedef, info$gen.id, paramdef, data[[data.names[i]]], index, info$par.type, compress=compress, csv.state=csv.state)
  }
  if(info$close.con) dbDisconnect(info$con)
  return(info$gen.id)
}

efsWrite <- function(data, entity, type.key, con=NULL, dbname=NULL, instance.id=NULL, instance.caption=NULL, index=NULL, typedef=NULL, paramdefs=NULL, compress=TRUE, csv.state = NULL) {
  efsGenWrite(type='TYPEDEF',data, entity, type.key, con, dbname, instance.id, instance.caption, index, gentypedef=typedef,genparamdefs=paramdefs, compress=compress, csv.state = csv.state)
}

efsRelWrite <- function(data, entity, type.key, con=NULL, dbname=NULL, rel.id=NULL, ids=NULL, index=NULL, reltypedef=NULL, relparamdefs=NULL , compress=TRUE, csv.state = NULL) {
  efsGenWrite(type='RELTYPEDEF',data, entity, type.key, con, dbname, rel.id, ids, index, gentypedef=reltypedef,genparamdefs=relparamdefs, compress=compress, csv.state = csv.state)
}


efsGenRead <- function(type=c('TYPEDEF','RELTYPEDEF'),startdate, enddate, interval, entity, type.key, con=NULL, dbname=NULL, gen.id=NULL, descriptor=NULL, index=NULL, gentypedef=NULL,genparamdefs=NULL,uncompress=TRUE){
  info <- efsInfo(type, entity, type.key, con, dbname, gen.id, descriptor, gentypedef=gentypedef,genparamdefs=genparamdefs)   
  
  data <- list()
  for(i in 1:nrow(info$genparamdefs)){
    paramdef <- info$genparamdefs[i,]
    param.read <- efsReadGenParam(info$con, info$gentypedef, info$gen.id, paramdef, startdate, enddate, interval,index=index,uncompress=uncompress ,info$par.type)
    data[[as.character(paramdef$KEY)]] <- param.read
  }
  if(info$close.con) dbDisconnect(info$con)
  return(data)
}

efsRead <- function(startdate, enddate, interval, entity, type.key, con=NULL, dbname=NULL, instance.id=NULL, instance.caption=NULL, index=NULL, typedef=NULL,paramdefs=NULL,uncompress=TRUE){
  efsGenRead(type='TYPEDEF',startdate, enddate, interval, entity, type.key, con, dbname, instance.id, instance.caption, index, typedef,paramdefs, uncompress)
}

efsRelRead <- function(startdate, enddate, interval, entity, type.key, con=NULL, dbname=NULL, rel.id=NULL, ids=NULL, index=NULL, reltypedef=NULL,relparamdefs=NULL,uncompress=TRUE){
  efsGenRead(type='RELTYPEDEF',startdate, enddate, interval, entity, type.key, con, dbname, rel.id, ids, index,reltypedef,relparamdefs, uncompress)
}

efsGenWriteDenorm <- function(type=c('TYPEDEF','RELTYPEDEF'),data, entity, type.key, con=NULL, dbname=NULL, gen.id=NULL, descriptor=NULL, index=NULL, gentypedef=NULL,genparamdefs=NULL) {
  info <- efsInfo(type, entity, type.key, con, dbname, gen.id, descriptor, create.new=TRUE, gentypedef=gentypedef,genparamdefs=genparamdefs) 
  
  data.names <- colnames(data)
  denorm.name <- NULL
  gentypeid <- info$gentypedef$ID
  inst.col <- entity.to.col.id(info$gentypedef$ENTITY)
  
  
  for(dn in data.names) {
    if(! (dn %in% c('APPLDATETIME', 'INDX', str_c(type,'_ID'),inst.col))) { 
      if(!(dn %in% info$genparamdefs$KEY)) fn$stop('The following name could not be matched to paramdef:`dn`')             
      paramdef <- subset(info$genparamdefs, KEY==dn)
      if(is.null(denorm.name)) { 
        denorm.name <- paramdef$PARAM_TABLE
      } else { 
        if(paramdef$PARAM_TABLE != denorm.name ) fn$stop('`dn` not in denorm table:`denorm.name`')                         
      }
    } 
  }
  if(!'APPLDATETIME' %in% data.names) stop('APPLDATETIME not specified')
  if(is.null(denorm.name)) stop('Not a denorm table')
  
  
  filename <- tempfile()
  df <- data.frame(data, info$gen.id, gentypeid)
  names(df)[ncol(df)-1] <- inst.col
  names(df)[ncol(df)] <- str_c(type,'_ID')
  
  df$APPLDATETIME <-  format(df$APPLDATETIME,'%Y-%m-%d %H:%M:%S')
  
  write.table(df, file=filename, append=FALSE, row.names=FALSE, col.names=FALSE, na="NULL", sep=",")
  
  names.sql <- str_c(names(df),collapse=",")  
  
  dbSendUpdate(info$con, str_c('insert into ',denorm.name,' (',names.sql,") select * from CSVREAD('", filename,"','",names.sql,"')"))
  dbSendUpdate(info$con, 'commit')
  if(info$close.con) dbDisconnect(info$con)
  return(info$gen.id)
}
efsWriteDenorm <- function(data, entity, type.key, con=NULL, dbname=NULL, instance.id=NULL, instance.caption=NULL, index=NULL, typedef=NULL,paramdefs=NULL){
  efsGenWriteDenorm('TYPEDEF',data, entity, type.key, con, dbname, gen.id=instance.id, descriptor=instance.caption, index, gentypedef=typedef,genparamdefs=paramdefs)
}

efsRelWriteDenorm <- function(data, entity, type.key, con=NULL, dbname=NULL, gen.id=NULL, ids=NULL, index=NULL, reltypedef=NULL,relparamdefs=NULL){
  efsGenWriteDenorm('RELTYPEDEF',data, entity, type.key, con, dbname, gen.id=rel.id, descriptor=ids, index, gentypedef=reltypedef,genparamdefs=relparamdefs)
}

efsGenReadDenorm <- function(type=c('TYPEDEF','RELTYPEDEF'),startdate, enddate, entity, type.key, param.key, con=NULL, dbname=NULL, gen.id=NULL, descriptor=NULL, index=NULL, gentypedef=NULL,genparamdefs=NULL){
  info <- efsInfo(type, entity, type.key, con, dbname, gen.id, descriptor, gentypedef=gentypedef,genparamdefs=genparamdefs)  
  
  gentypeid <- info$gentypedef$ID
  inst.col <- entity.to.col.id(info$gentypedef$ENTITY)
  
  if(param.key %in% info$genparamdefs$KEY){
    paramdef <- subset(info$genparamdefs, KEY==param.key)
    if(paramdef$PARAMKIND_KEY!='DENORM') stop('Not a denorm table')
    denorm.name <- paramdef$PARAM_TABLE 
    
    denorm.cols <- subset(info$genparamdefs, PARAM_TABLE==denorm.name)$KEY
    denorm.cols.str <- toString(denorm.cols)
  } else {
    stop(fn$paste('Could not find parameter "`param.key`"'))
  }
  
  if(is.null(index)) {
    index.sql <- "and INDX is NULL"
  } else if (index=='ALL'){
    index.sql <- "" 
  } else {
    index.sql <- str_c("and INDX ='",index,"'")
  }  
  if(!is.null(startdate)) start.sql <- str_c(" and APPLDATETIME >= '",format(startdate, format='%Y-%m-%d %H:%M:%S'),"'")
  if(!is.null(enddate)) end.sql <- str_c(" and APPLDATETIME <= '",format(enddate, format='%Y-%m-%d %H:%M:%S'),"'")
  
  
  id <- info$gen.id
  ids.sql <- str_c(inst.col," in (",toString(id),")")
  
  
  sql <- str_c("select FORMATDATETIME(APPLDATETIME,'yyyy-MM-dd HH:mm:ss') APPLDATETIME, CONVERT(INDX,integer) INDX,",denorm.cols.str,",",inst.col,"  from  ",denorm.name," where ",ids.sql," ",index.sql,start.sql,end.sql,' order by CONVERT(INDX,integer), APPLDATETIME')
  
  tmpfile <- tempfile()
  new.sql <- paste("CALL CSVWRITE('",tmpfile,"', '",gsub("'","''",sql),"');",sep="")  
  
  dbSendUpdate(info$con, new.sql)
  rs <- read.csv(tmpfile)
  unlink(tmpfile)
  
  if(info$close.con) dbDisconnect(info$con)
  return(rs)
}

efsReadDenorm <- function(startdate, enddate, entity, type.key, param.key, con=NULL, dbname=NULL, instance.id=NULL, instance.caption=NULL, index=NULL, typedef=NULL,paramdefs=NULL){
  efsGenReadDenorm('TYPEDEF',startdate, enddate, entity, type.key, param.key, con=con, dbname=dbname, gen.id=instance.id, descriptor=instance.caption, index=index, gentypedef=typedef,genparamdefs=paramdefs)
}

efsRelReadDenorm <- function(startdate, enddate, entity, type.key, param.key, con=NULL, dbname=NULL, rel.id=NULL, ids=NULL, index=NULL, reltypedef=NULL,relparamdefs=NULL){
  efsGenReadDenorm('RELTYPEDEF',startdate, enddate, entity, type.key, param.key, con=con, dbname=dbname, gen.id=rel.id, descriptor=ids, index=index, gentypedef=reltypedef,genparamdefsrel=paramdefs)
}
############################ Utility ####################################
efsGetMetaData <- function(con, entity, type.key, param.key=NULL, class=c('I','R')) {
  class.type <- ifelse(class[1]=='I', 'TYPEDEF','RELTYPEDEF')
  typedef <- efsGetGenDef(con,entity,type.key, class.type)
  entity <- as.character(typedef$ENTITY)
  type.key <- as.character(typedef$KEY)
  typedef.id <- typedef$ID
  
  param.type <- ifelse(class[1]=='I', 'PARAMDEF','RELPARAMDEF')
  paramdefs <- efsGetGenParamDef(con, typedef$ID, param.key, param.type)
  param.key <- as.character(paramdefs$KEY)
  return(list(typedef=typedef, paramdefs=paramdefs, entity=entity, type.key=type.key, param.key=param.key,typedef.id=typedef.id))   
} 

efsReadKey.zoo <- function(con, entity, type.key, param.key, startdate, enddate, interval,instances=NULL, index=NULL) {
  typedef <- efsGetTypeDef(con,entity,type.key)
  paramdefs <- efsGetParamDef(con, typedef$ID)
  if(is.null(instances)) r <- efsQueryInstance(con, typedef)$ID else r<- instances
  
  result.list <- lapply(1:length(r), function(x) efsRead(startdate, enddate, interval, instance.id=r[x], con=con,typedef=typedef, paramdefs=subset(paramdefs, KEY==param.key), index=index))
  l <- do.call(cbind,lapply(result.list, function(x) x[[param.key]]))
  names(l) <- r
  l.merge <- merge(l,check.names=F)
  
  rs <- efsQueryInstance(con, typedef, ids=r)
  attr(l.merge, 'CAPTION') <- rs$CAPTION
  
  return(l.merge)
}

efsReadInstance.zoo <- function(con, entity, type.key, startdate, enddate, interval, paramdef.keys=NULL, instances=NULL,index=NULL, uncompress=TRUE) {
  typedef <- efsGetTypeDef(con,entity,type.key)
  paramdefs <- efsGetParamDef(con, typedef$ID)
  if(!is.null(paramdef.keys)) paramdefs <- subset(paramdefs, KEY %in%  paramdef.keys)
  
  if(is.null(instances)) r <- efsQueryInstance(con, typedef)$ID else r<- instances
  
  result.list <- lapply(1:length(r), function(x) efsRead(startdate, enddate, interval, instance.id=r[x], con=con,typedef=typedef, paramdefs=paramdefs, index=index, uncompress=uncompress))
  l.merge <- lapply(result.list, function(x) do.call(cbind,x))
  names(l.merge) <- r
  
  rs <- efsQueryInstance(con, typedef, ids=r)
  attr(l.merge, 'CAPTION') <- rs$CAPTION
  
  
  return(l.merge)
}


efsClearOut <- function(con, entity, type.key, truncate=FALSE) {
  metadata <- efsGetMetaData(con,entity, type.key)
  
  # Clear existing instances
  result.instances <- efsQueryInstance(con,metadata$typedef)
  if(nrow(result.instances)>0) {
    if(truncate) fn$dbSendUpdate(con, "truncate table `entity.to.out(entity)`")
    lapply(1:nrow(result.instances), function(x) efsClearInstance(con, metadata$typedef, result.instances$ID[x]))
  }
  
  
}

efsWriteOut <- function(con, entity, type.key, result.list, instance.caption=NULL,compress=FALSE) {
  metadata <- efsGetMetaData(con,entity,type.key)
  
  # Make a new instance
  if(is.null(instance.caption)) instance.caption <- fn$paste("`entity`-`type.key` `date()`")
  instance.id <- efsNewInstance(con, metadata$typedef,instance.caption)
  
  for(param.key in names(result.list)) {
    result.out <- result.list[[param.key]]
    # Write using csv
    csv.state <- list()
    csv.state$filename <- tempfile()
    csv.state$file <- file(csv.state$filename, "w")
    
    for(i in 1:iterations) {
      csv.state$op <- 'APPEND'
      if (i==iterations) csv.state$op <- 'UPDATEDB'
      
      data <- list()
      data[[param.key]] <- result.out[[i]]
      
      instance.id <- efsWrite(data, typedef=metadata$typedef, paramdefs=metadata$paramdefs, con=con, instance.id=instance.id, index=i,compress=compress, csv.state=csv.state)        
    }
  }
  unlink(csv.state$filename)
  return(instance.id)
}

efsInstancesFromRel <- function(con, relentity, rel.typekey, param.keys, startdate, enddate, interval, ...) {
  lnreltype <- efsGetRelTypeDef(con, relentity,rel.typekey)
  lnrels <- efsQueryRel(con, lnreltype)
  lookup.instances <- efsLookup.id(lnrels, ...)
  
  from.typedef <- efsGetTypeDef.byid(con, lnreltype$FROM_ID)
  to.typedef <- efsGetTypeDef.byid(con, lnreltype$TO_ID)
  
  entity <- names(lookup.instances)[1]
  if(entity == from.typedef$ENTITY) typedef <- from.typedef else typedef <- to.typedef 
  
  l.zoo <- efsReadInstance.zoo(con, entity=typedef$ENTITY, type.key=typedef$KEY, startdate, enddate, interval, paramdef.keys=param.keys,  lookup.instances[[1]])
  
  return(l.zoo)
}

readseries <- function(con, entityname, typedefname, instancename, paramname) {
  interval <- 'month'    
  typedef <- efsGetTypeDef(con, entityname, typedefname)
  paramdef <- efsGetParamDef(con, typedef$ID, paramname)
  instance <- efsGetInstance(con, typedef, instancename)
  print(paste("debug: ", instance$ID, typedef$ID, paramdef$KEY))
  
  qry <- str_c("SELECT CONVERT(VAL, DOUBLE) FROM ", paramdef$PARAM_TABLE, 
               " WHERE TYPEDEF_ID = ", typedef$ID, 
               " AND ", typedef$ENTITY, 
               "_ID = ", instance$ID, 
               " ORDER BY APPLDATETIME")
  print(qry);
  return(dbGetQuery(con,  qry))
}

writeseries <- function(con, entityname, typedefname, caption, values.zoo) {
  typedef <- efsGetTypeDef(con, entityname, typedefname)
  instance <- efsGetInstance(con, typedef, caption)    
  efsWrite(values.zoo, entityname, typedefname, con=con, instance.id=instance$ID)
}




