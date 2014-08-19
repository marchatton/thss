########### LOAD ####################
# List all typedefs of Entity POWERSTATION 
sink("NUL")

efsGetTypeDef(con, 'LOAD');
gentypedef <- efsGetTypeDef(con, 'LOAD', 'NODE_LOAD') #Choose specific typedef by entity and key
print(gentypedef)
Listalltypedef <- efsGetParamDef(con, gentypedef$ID) #List all typedef parameters for selected typedef
genparamdef <- efsGetParamDef(con, gentypedef$ID,'EXPECTED_LOAD') #Choose a INPUT parameter by key: 
print(genparamdef)
# NOTE: DATATYPE_KEY = NUMBER (So no INDX is used in the data table)
efsQueryInstance(con, gentypedef) #List all typedef instances

nodes <- efsQueryInstance(con, gentypedef) # List all typedef instances
nodes_tot  <- dim(nodes)[1]

node_load <- data.frame(matrix(ncol=nodes_tot, nrow=interval_num))
colnames(node_load) <- nodes[,2]
instance <- efsGetInstance(con, gentypedef,nodes[1,2])
result <- efsReadParam(con,gentypedef,instance$ID, genparamdef,startdate,enddate,interval)
rownames(node_load) <- as.Date(time(result))

# Choose Typedef instance by instance name in I_COALSUPPLY 
for (i in 1:nodes_tot){
  instance <- efsGetInstance(con, gentypedef,nodes[i,2])
  result <- efsReadParam(con,gentypedef,instance$ID, genparamdef,startdate,enddate,interval)
  node_load[as.character((instance$CAPTION))] <- result
}

# 
# ## Choose Typedef instance by instance name
# instance <- efsGetInstance(con, gentypedef,'Central Load')
# #print(instance)
# l1 <- efsReadParam(con,gentypedef,instance$ID, genparamdef,startdate,enddate,interval) 
# 
# ## Choose Typedef instance by instance name
# instance <- efsGetInstance(con, gentypedef,'Cape Load')
# l2 <- efsReadParam(con,gentypedef,instance$ID, genparamdef,startdate,enddate,interval)
# 
# ## Choose Typedef instance by instance name
# instance <- efsGetInstance(con, gentypedef,'KZN Load')
# l3 <- efsReadParam(con,gentypedef,instance$ID, genparamdef,startdate,enddate,interval) 
# 
# load <- merge(l1, l2, l3)
# colnames(load) <- c("Central","Cape", "Kzn")

sink()
print("Got Load. Saved to 'node_load'")
