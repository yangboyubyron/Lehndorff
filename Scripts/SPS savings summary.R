# SPS savings summary
library(dplyr)
library(xlsx)

# Home Energy Services - NM
EE<-read.csv("/volumes/Projects/457001 - New Mexico/Data/SPS/2018/Home Energy Services/Final year end data/HES 2018 EMV_TT.csv",stringsAsFactors = FALSE) %>% mutate(LI=FALSE)
EEagg<-EE %>% group_by(Program="Home Energy Services") %>% summarise(kW=sum(Claimed.kW),kWh=sum(Claimed.kWh))

# Low Income - Home Energy Services - NM
LI<-read.csv("/volumes/Projects/457001 - New Mexico/Data/SPS/2018/Home Energy Services/Final year end data/HES L-I 2018 EMV_TT.csv",stringsAsFactors = FALSE) %>% filter(Program.Name!="") %>% mutate(LI=TRUE)
LIagg<-LI %>% group_by(Program="Low Income - Home Energy Services") %>% summarise(kW=sum(Claimed.kW),kWh=sum(Claimed.kWh))

# Business Comprehensive
BC<-read.csv("/volumes/Projects/457001 - New Mexico/Data/SPS/2018/Business Comprehensive/Final year end data/2018 NM Business Comprehensive.csv",stringsAsFactors = FALSE)
BCagg1<-BC %>% group_by(Program="Business Comprehensive") %>% summarise(kW=sum(PC.kW...Customer),kWh=sum(Customer.kWh), Net.kW=sum(PC.kW...Customer*Electric.Net.To.Gross),Net.kWh=sum(Customer.kWh*Electric.Net.To.Gross))
BCagg2<-BC %>% group_by(Opportunity..Program) %>% summarise(kW=sum(PC.kW...Customer),kWh=sum(Customer.kWh), Net.kW=sum(PC.kW...Customer*Electric.Net.To.Gross),Net.kWh=sum(Customer.kWh*Electric.Net.To.Gross))

# Home Lighting - NM
HL<-read.csv("/volumes/Projects/457001 - New Mexico/Data/SPS/2018/Home Lighting/Xcel NM Bulb Sales Dec 17 - Nov 18.csv",stringsAsFactors = FALSE)
HLagg<-HL %>% group_by(Program="Home Lighting") %>% summarise(kW=sum(kW.Savings),kWh=sum(kWh.Savings))

# Residential Cooling - NM
ResCool<-read.csv("/volumes/Projects/457001 - New Mexico/Data/SPS/2018/Residential Cooling/Res Cooling 2018 .csv",stringsAsFactors = FALSE) %>% filter(Opportunity..Opportunity..!="")
RCagg<-ResCool %>% group_by(Program="Residential Cooling") %>% summarise(kW=sum(PC.kW...Customer),kWh=sum(Customer.kWh), Net.kW=sum(PC.kW...Customer*Electric.Net.To.Gross),Net.kWh=sum(Customer.kWh*Electric.Net.To.Gross))

# School Education Kits - NM
Kits<-read.csv("/volumes//Projects/457001 - New Mexico/Data/SPS/2018/School Kits/2018 NMx School Kits Data for Evergreen.csv",stringsAsFactors = FALSE) %>% filter(!is.na(Premise.ID))
Kitsagg<-Kits %>% group_by(Program="School Education Kits") %>% summarise(kW=sum(Customer.kW),kWh=sum(as.numeric(gsub(",","",x=Customer.kWh))),Net.kW=sum(Net.Customer.kW),Net.kWh=sum(as.numeric(gsub(",","",x=Net.Customer.kWh))))

# Smart Thermostat - NM

# Residential Demand Response - NM

# Energy Feedback Residential - NM

