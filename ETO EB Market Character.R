# ETO EB Sample Design
library(xlsx)
library(dplyr)
library(lubridate)
library(ggplot2)

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

population<-read.csv("/volumes/Projects/430011 - ETO Existing Buildings/Data/All Eligible Commercial Sites.csv",stringsAsFactors = FALSE)
projects<-read.csv("/volumes/Projects/430011 - ETO Existing Buildings/Data/All Commercial Site Projects.csv",stringsAsFactors = FALSE)
contacts<-read.csv("/volumes/Projects/430011 - ETO Existing Buildings/Data/All Commercial Site Contacts.csv",stringsAsFactors = FALSE)

# assign NAICS group
population$naicsgroup<-"Unknown"
population$naicsgroup[substr(population$naics_code,1,2)==11]<-"Industrial"
population$naicsgroup[substr(population$naics_code,1,2)==21]<-"Industrial"
population$naicsgroup[substr(population$naics_code,1,2)==22]<-"Industrial"
population$naicsgroup[substr(population$naics_code,1,2)==23]<-"Industrial"
population$naicsgroup[substr(population$naics_code,1,2)==42]<-"Retail"
population$naicsgroup[substr(population$naics_code,1,2)==44]<-"Retail"
population$naicsgroup[substr(population$naics_code,1,2)==45]<-"Retail"
population$naicsgroup[substr(population$naics_code,1,2)==48]<-"Warehouse"
population$naicsgroup[substr(population$naics_code,1,2)==49]<-"Warehouse"
population$naicsgroup[substr(population$naics_code,1,1)==5]<-"Office"
population$naicsgroup[substr(population$naics_code,1,2)==61]<-"School"
population$naicsgroup[substr(population$naics_code,1,2)==71]<-"Recreation"
population$naicsgroup[substr(population$naics_code,1,1)==3]<-"Industrial"
population$naicsgroup[substr(population$naics_code,1,2)==62]<-"Medical"
population$naicsgroup[substr(population$naics_code,1,3)==721]<-"Hotel"
population$naicsgroup[substr(population$naics_code,1,3)==722]<-"Restaurant"
population$naicsgroup[substr(population$naics_code,1,3)==811]<-"Repair"
population$naicsgroup[substr(population$naics_code,1,3)==812]<-"Repair"
population$naicsgroup[substr(population$naics_code,1,3)==813]<-"Religious"
population$naicsgroup[substr(population$naics_code,1,2)==92]<-"Public"
table(population$naicsgroup)

# then market
population$naicsgroup[population$naicsgroup=="Unknown"&population$markettype=="Education"]<-"School"
population$naicsgroup[population$naicsgroup=="Unknown"&population$markettype=="Health"]<-"Medical"
population$naicsgroup[population$naicsgroup=="Unknown"&population$markettype=="Office"]<-"Office"
population$naicsgroup[population$naicsgroup=="Unknown"&population$markettype=="Retail"]<-"Retail"
population$naicsgroup[population$naicsgroup=="Unknown"&population$markettype=="Warehousing and Storage"]<-"Warehouse"
population$naicsgroup[population$naicsgroup=="Unknown"&population$markettype=="Unspecified Government/Public Sector"]<-"Public"
population$naicsgroup[population$naicsgroup=="Unknown"&population$markettype=="Amusement/Recreational"]<-"Recreation"
population$naicsgroup[population$naicsgroup=="Unknown"&population$marketsubtype=="Auto Repair"]<-"Repair"
population$naicsgroup[population$naicsgroup=="Unknown"&population$markettype=="Auto Services"]<-"Retail"
population$naicsgroup[population$naicsgroup=="Unknown"&population$marketsubtype=="Lodging/Hotel/Motel"]<-"Hotel"
population$naicsgroup[population$naicsgroup=="Unknown"&population$markettype=="Hospitality"]<-"Restaurant"
population$naicsgroup[population$naicsgroup=="Unknown"&population$marketsubtype=="Religious/Spiritual"]<-"Religious"
population$naicsgroup[population$naicsgroup=="Unknown"&population$markettype=="Assembly"]<-"Public"
population$naicsgroup[population$naicsgroup=="Unknown"&population$markettype=="Courthouse/Probation Office"]<-"Public"
population$naicsgroup[population$naicsgroup=="Unknown"&population$markettype=="Fire Protection"]<-"Public"
population$naicsgroup[population$naicsgroup=="Unknown"&population$markettype=="Library"]<-"Public"
population$naicsgroup[population$naicsgroup=="Unknown"&population$markettype=="Jail/Reformatory/Penitentiary"]<-"Public"
population$naicsgroup[population$naicsgroup=="Unknown"&population$markettype=="Military (Armory, etc.)"]<-"Public"
population$naicsgroup[population$naicsgroup=="Unknown"&population$markettype=="Police"]<-"Public"
population$naicsgroup[population$naicsgroup=="Unknown"&population$markettype=="Transportation Infrastructure (Tunnel, Roadway, Dock, etc.)"]<-"Public"
population$naicsgroup[population$naicsgroup=="Unknown"&population$markettype=="Data Center"]<-"Warehouse"
population$naicsgroup[population$naicsgroup=="Unknown"&population$markettype=="Bank/Financial Institution"]<-"Retail"
population$naicsgroup[population$naicsgroup=="Unknown"&population$markettype=="Museum"]<-"Public"
population$naicsgroup[population$naicsgroup=="Unknown"&population$markettype=="Gym/Athletic Club"]<-"Retail"
population$naicsgroup[population$naicsgroup=="Unknown"&population$markettype=="Laundry/Dry Cleaner"]<-"Retail"
population$naicsgroup[population$naicsgroup=="Unknown"&population$markettype=="Funeral/Cremation"]<-"Office"
population$naicsgroup[population$naicsgroup=="Unknown"&population$markettype=="Parking Structure/Garage"]<-"Retail"

# then general
population$naicsgroup[population$naicsgroup=="Unknown"&(population$multifamily==1|population$residential==1)]<-"Multifamily/Residential"
population$naicsgroup[population$naicsgroup=="Unknown"&(population$commercial==1|population$nonresidential==1)]<-"Unknown Commercial"
table(population$naicsgroup)

# size categories
population$fuel_group<-"Unknow Fuel"
population$fuel_group[!is.na(population$kwh2017)&population$kwh2017>0]<-"Electric"
population$fuel_group[!is.na(population$therms2017)&population$therms2017>0]<-"Gas"
population$fuel_group[!is.na(population$kwh2017)&!is.na(population$therms2017)&population$kwh2017>0&population$therms2017>0]<-"Electric and Gas"
table(population$fuel_group)

summary(population$kwh2017[population$fuel_group=="Electric and Gas"]*0.0034121412+population$therms2017[population$fuel_group=="Electric and Gas"]*.1)

population$fuel_size<-"Unknown Size"
population$fuel_size[population$fuel_group=="Electric"]<-"Large"
population$fuel_size[population$fuel_group=="Electric"&population$kwh2017<40000]<-"Medium"
population$fuel_size[population$fuel_group=="Electric"&population$kwh2017<4000]<-"Small"
population$fuel_size[population$fuel_group=="Gas"]<-"Large"
population$fuel_size[population$fuel_group=="Gas"&population$therms2017<3000]<-"Medium"
population$fuel_size[population$fuel_group=="Gas"&population$therms2017<400]<-"Small"
population$fuel_size[population$fuel_group=="Electric and Gas"]<-"Large"
population$fuel_size[population$fuel_group=="Electric and Gas"&(population$kwh2017*0.0034121412+population$therms2017*.1)<700]<-"Medium"
population$fuel_size[population$fuel_group=="Electric and Gas"&(population$kwh2017*0.0034121412+population$therms2017*.1)<100]<-"Small"

table(population$fuel_size,population$fuel_group)

# Participation
nonpartproj<-subset(projects,programdescription=="")$et_siteid

NonPartCon<-contacts %>% filter(et_siteid%in%nonpartproj&CRMContactName==""&CRMContactEmail==""&CRMContactBusinessPhone==""&CRMContactMobilePhone=="") %>% 
  # filter((CostarOwnerName!=""&CostarOwnerContact!=""&CostarOwnerPhone!="")|(InfousaCompanyName!=""&InfousaContactName!=""&InfousaPhone!="")) %>% 
  group_by(et_siteid) %>% mutate(row=1:n()) %>% filter(row==1)

population$participation<-!(population$et_siteid%in%NonPartCon$et_siteid&population$participanttype=="Non-Participant")

table(population$participation,population$participanttype)

population$participation<-ifelse(population$participation,"Participant","Non-Participant")

# output
characterization<-population %>% group_by(fuel_size,naicsgroup,participation) %>% summarise(n=n()) %>% group_by(fuel_size,naicsgroup) %>% mutate(pct=round(n/sum(n),2)) %>% 
  arrange(participation,naicsgroup)

ggplot(population %>% filter(naicsgroup!="Unknown Commercial"))+
  geom_bar(position = "fill",aes(x=naicsgroup,fill=fuel_size))+
  facet_grid(participation~.)

ggplot(population %>% filter(naicsgroup!="Unknown Commercial"))+
  geom_bar(position = "dodge",aes(x=naicsgroup,fill=fuel_size))+
  facet_grid(participation~.)
