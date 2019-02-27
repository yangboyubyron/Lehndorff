# SPS Contractors
library(dplyr)
library(xlsx)

# HES
EE<-read.csv("/volumes/Projects/457001 - New Mexico/Data/SPS/2018/Home Energy Services/Final year end data/HES 2018 EMV_TT.csv",stringsAsFactors = FALSE) %>% mutate(LI=FALSE)
LI<-read.csv("/volumes/Projects/457001 - New Mexico/Data/SPS/2018/Home Energy Services/Final year end data/HES L-I 2018 EMV_TT.csv",stringsAsFactors = FALSE) %>% filter(Program.Name!="") %>% mutate(LI=TRUE)

All_Cont<-bind_rows(EE,LI)

TAs<-All_Cont %>% group_by(Trade.Ally.Business.Name,Trade.Ally.Contact.First.Name,Trade.Ally.Contact.Last.Name,Trade.Ally.Contact.Phone,Trade.Ally.Email) %>% 
  summarise(`Number of Projects`=n(),`LI Projects`=sum(LI),`LI Savings`=sum(Claimed.kWh[LI]),`Non-LI Projects`=sum(!LI),`Non-LI Savings`=sum(Claimed.kWh[!LI]))

# write.csv(TAs,"/volumes/Projects/457001 - New Mexico/Data/SPS/2018/Home Energy Services/HES Trade Ally Summary.csv",row.names = FALSE)

# Business Comprehensive
BC<-read.csv("/volumes/Projects/457001 - New Mexico/Data/SPS/2018/Business Comprehensive/Final year end data/2018 NM Business Comprehensive.csv",stringsAsFactors = FALSE)
BC_cont<-read.csv("/volumes/Projects/457001 - New Mexico/Data/SPS/2018/Business Comprehensive/Contractor contact info/report1551188622071.csv",stringsAsFactors = FALSE)

BCagg<-BC %>% select(Opportunity..Trade.Partner,Opportunity..Program,Opportunity..Opportunity..) %>% distinct() %>% 
  group_by(Opportunity..Trade.Partner) %>% 
  summarise(`Total Projects`=n(),
    `Lighting - NM` = sum(Opportunity..Program=="Lighting - NM"),
    `Cooling - NM` = sum(Opportunity..Program=="Cooling - NM"),
    `Motors Efficiency - NM` = sum(Opportunity..Program=="Motors Efficiency - NM"),
    `Computer Efficiency - NMx` = sum(Opportunity..Program=="Computer Efficiency - NMx"),
    `Custom Efficiency - NM` = sum(Opportunity..Program=="Custom Efficiency - NM"))

contagg<-BC_cont %>% 
  filter(Main.Phone.Number!="") %>% 
  group_by(Trade.Partner.Name,Main.Phone.Number) %>% 
  summarise(Contacts=n(),
    Name.1=Full.Name[1],
    Name.2=ifelse(Contacts>=2,Full.Name[2],"None"),
    Name.3=ifelse(Contacts>=3,Full.Name[3],"None"),
    Name.4=ifelse(Contacts>=4,Full.Name[4],"None"))

BC_TAs<-left_join(BCagg,contagg,by=c("Opportunity..Trade.Partner"="Trade.Partner.Name"))

# write.xlsx(BC_TAs %>% data.frame(),file = "/volumes/Projects/457001 - New Mexico/Data/SPS/2018/Business Comprehensive/BC Contractor Summary.xlsx",sheetName = "Contractor Summary",row.names = FALSE)
# write.xlsx(contagg %>% data.frame(),file = "/volumes/Projects/457001 - New Mexico/Data/SPS/2018/Business Comprehensive/BC Contractor Summary.xlsx",sheetName = "All Contact Information",append = TRUE,row.names = FALSE)

