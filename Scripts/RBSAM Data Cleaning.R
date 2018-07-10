library(dplyr)

# creating end-use data from RBSAM data
path<-"/volumes/Projects/401013 - PG&E RBSA/Metering QC/RBSAM Data/RBSAM"
column<-"Dryer.kWh..Appliance."

fulldata<-NULL
for(i in 1:9){
  print(i)
  data<-read.csv(paste(path,i,".csv",sep=""),stringsAsFactors = FALSE)
  dataout<-select(data,c(siteid,time,(1:length(colnames(data)))[colnames(data)==column]))
  fulldata<-bind_rows(fulldata,dataout)
  rm(data)
}

# write.csv(paste(path,"dryers_only.csv",sep="/"))

i<-1
