library(dplyr)
library(ggplot2)

# read data (read.csv()) -- SPS measure level data, strings should not be read as factors for simplicity.
## read [the file] and do not read strings as factors
SPSmeasures<-read.csv("/volumes/Projects/457001 - New Mexico/Data/SPS/2017 Final Salesforce Data/2017 NM DSM Evals - Measure Level Data.csv",stringsAsFactors = FALSE)
SPScontact<-read.csv("/volumes/Projects/457001 - New Mexico/Data/SPS/Business Comprehensive/Business Comprehensive - Contact Data.csv",stringsAsFactors = FALSE)

#clean up column names by removing prefix "Opportunity"
## the column names of SPSmeasures is the colnames of SPSmeasures where nothing ("") has been substituted for (the first instence of exactly) "Opportunity.."
colnames(SPSmeasures)<-sub("Opportunity..","",colnames(SPSmeasures),fixed = TRUE)

# look at only residential measures (subset()) -- subset data to where the segment variable is Business
## BusMeasures is a subset of SPSmeasures where Integration.Program.Segment equals Business
BusMeasures<-subset(SPSmeasures,Integration.Program.Segment=="Business")

# aggregate the measure-level data by measure type to find the count of each measure, the gross savings, and the number of unique accounts that had that measure type.
## MeasureSavings is BusMeasures then grouped by Product then aggregated such that n is the number of instances (rows) of the group, kWh is sum of Customer.kWh of the group ignoring NAs, and numaccounts is the count of unique Account.Number of the group
MeasureSavings<-BusMeasures%>%group_by(Product)%>%summarise(n=n(),kWh=sum(Customer.kWh,na.rm=TRUE),numaccounts=n_distinct(Account.Number))

# use a for loop to create a measure type group (custom, lighting, other) starting with a default of NA
##BusMeasures MeasureGroup is NA
## for i going from 1 to the number of rows in BusMeasures, if Product at row i contains "custom" (ignoring case),MeasureGroup at row i is "Custom"...
BusMeasures$MeasureGroup<-NA
for(i in 1:nrow(BusMeasures)){
  if(grepl("custom",BusMeasures$Product[i],ignore.case = TRUE)){
    BusMeasures$MeasureGroup[i]<-"Custom"
  }else if(grepl("lighting",BusMeasures$Product[i],ignore.case = TRUE)){
    BusMeasures$MeasureGroup[i]<-"Lighting"
  } else {
    BusMeasures$MeasureGroup[i]<-"Other"
  }
}

# extract application date from the measure data
## BusMeasure date is Application.Recieved.Date as a date formatted as month (mm)/day (dd)/year (yyyy)
BusMeasures$date<-as.Date(BusMeasures$Application.Received.Date,format = "%m/%d/%Y")

# aggregate measure data to project/account level
## BusProjects is BusMeasures grouped by Account.Number then aggregated so that CustSave is the sum of Customer.kWh where MeasureGroup is "Custom"...
BusProjects<-BusMeasures%>%group_by(Account.Number)%>%summarise(CustSave=sum(Customer.kWh[MeasureGroup=="Custom"]),LightSave=sum(Customer.kWh[MeasureGroup=="Lighting"]),OtherSave=sum(Customer.kWh[MeasureGroup=="Other"]),TotalSave=sum(Customer.kWh))

# run a simple frequency of to check the results
table(BusMeasures$MeasureGroup)

# clean up contact data to only 1 usable contact per account
## cleancontact is SPScontact filter so that Phone is never blank then group by Account number and aggregate so that Phone is the first value of Phone...
cleancontact<-SPScontact%>%filter(Ok.To.Sample.Flag!=1&Phone!=""&Premise..Premise.City!=""&Premise..Premise.Street.Name!="_")%>%group_by(Account.Number)%>%summarise(Phone=first(Phone),Address=first(Premise..Premise.Street.Name),City=first(Premise..Premise.City),Employees=unique(Employees))

# merge business information on to project level savings and drop customers with out contact information
## ContactSavings is cleancontact joined TO BusProjects by Account.Number and filtered to keep rows where Phone is not NA
ContactSavings<-left_join(BusProjects,cleancontact,by="Account.Number")%>%filter(!is.na(Phone))

#create plots to analyze the data
#is number of Employees related to Total Savings? Does this vary by city?
##plot ContactSavings with less than 1000 Employees
##add a point plot where x is Employees, y is TotalSave and color represents City
ggplot(ContactSavings%>%filter(Employees<1000))+
  geom_point(aes(x=Employees,y=TotalSave,color=City))+
  labs(y="Total Savings")+
  ggtitle("Savings by Number of Employees and City")

#When were individual measures installed how large were they and what type were they?
##plot BusMeasures
##add a jitter point plot where x is date y is the log of Customer.kWh and color represents MeasureGroup
ggplot(BusMeasures)+
  geom_jitter(aes(x=date,y=log(Customer.kWh),color=MeasureGroup))+
  labs(x="Date",y="Log of Savings", color="Measure Type")+
  ggtitle("Measure Installation over Time by Type")

#How much savings over time did each measure group account for and what was the total savings over time?
##BusMeasuresGroup is BusMeasures ordered by date filtering out row with NA Customer.kWh then grouped by MeasureGroup then add a new column, RunSave which is the cumulative total of Customer.kWh for the MeasureGroup
BusMeasuresGroup<-BusMeasures%>%arrange(date)%>%filter(!is.na(Customer.kWh))%>%group_by(MeasureGroup)%>%mutate(RunSave=cumsum(Customer.kWh))

##plot BusMeasuresGroup
##add a (red) line plot where the data is the subset of BusMeasuresGroup where MeasureGroup is "Custom" and x is date and y is RunSave...
ggplot(BusMeasuresGroup)+
  geom_line(data=subset(BusMeasuresGroup,MeasureGroup=="Custom"),color="red",aes(x=date,y=RunSave))+
  geom_line(data=subset(BusMeasuresGroup,MeasureGroup=="Lighting"),color="green",aes(x=date,y=RunSave))+
  geom_line(data=subset(BusMeasuresGroup,MeasureGroup=="Other"),color="blue",aes(x=date,y=RunSave))+
  geom_line(color="black",aes(x=date,y=cumsum(Customer.kWh)))+
  labs(x="Date",y="Total Savings")+
  ggtitle("Total Savings over Time by Type")

#When did the savings from each measure group occur? i.e. at what point was 70% of lighting savings installed
ggplot(BusMeasuresGroup)+
  geom_line(data=subset(BusMeasuresGroup,MeasureGroup=="Custom"),color="red",aes(x=date,y=RunSave/sum(Customer.kWh)))+
  geom_line(data=subset(BusMeasuresGroup,MeasureGroup=="Lighting"),color="green",aes(x=date,y=RunSave/sum(Customer.kWh)))+
  geom_line(data=subset(BusMeasuresGroup,MeasureGroup=="Other"),color="blue",aes(x=date,y=RunSave/sum(Customer.kWh)))+
  geom_line(color="black",aes(x=date,y=cumsum(Customer.kWh)/sum(Customer.kWh)))+
  labs(x="Date",y="Percent of Total Savings")+
  ggtitle("Percent of Savings over Time by Type")

 # apropos, for, if, piping, summarise, mutate, group_by, left_join, ggplot, weighted.mean, |, &, length, unique, select, n(), is.na, gsub, %in%
