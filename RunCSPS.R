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

