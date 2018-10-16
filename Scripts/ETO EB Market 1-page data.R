library(xlsx)
library(dplyr)
library(lubridate)
library(ggplot2)

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Data
population2<-read.csv("/volumes/Projects/430011 - ETO Existing Buildings/Data/All Eligible Commercial Sites.csv",stringsAsFactors = FALSE)
projects<-read.csv("/volumes/Projects/430011 - ETO Existing Buildings/Data/All Commercial Site Projects.csv",stringsAsFactors = FALSE)
contacts<-read.csv("/volumes/Projects/430011 - ETO Existing Buildings/Data/All Commercial Site Contacts.csv",stringsAsFactors = FALSE)
counties<-read.csv("/volumes/Projects/430011 - ETO Existing Buildings/Data/Oregon and SW Washington zip codes.csv",stringsAsFactors = FALSE) %>% group_by(Zip.Code) %>% summarise(County=unique(County))
regions<-read.csv("/volumes/Projects/430011 - ETO Existing Buildings/Data/ETO Regions.csv",stringsAsFactors = FALSE)
industries<-read.csv("/volumes/Projects/430011 - ETO Existing Buildings/Data/Employment by Industry.csv",stringsAsFactors = FALSE)
sector_groups<-read.csv("/volumes/Projects/430011 - ETO Existing Buildings/Data/EB market sector categories.csv",stringsAsFactors = FALSE) %>% select(et_marketname,Evergreen.categories)
SEM<-read.csv("/volumes/Projects/430011 - ETO Existing Buildings/Data/SEM impact data/SEM and Capital Participation Data to Evergreen.csv",stringsAsFactors = FALSE)

population<-population2 %>% left_join(sector_groups,by=c("market"="et_marketname"))

# assign NAICS group to population
population$naics_code_group<-"Unknown"
population$naics_code_group[substr(population$naics_code,1,2)==11]<-"Industrial"
population$naics_code_group[substr(population$naics_code,1,2)==21]<-"Industrial"
population$naics_code_group[substr(population$naics_code,1,2)==22]<-"Industrial"
population$naics_code_group[substr(population$naics_code,1,2)==23]<-"Industrial"
population$naics_code_group[substr(population$naics_code,1,2)==42]<-"Retail"
population$naics_code_group[substr(population$naics_code,1,2)==44]<-"Retail"
population$naics_code_group[substr(population$naics_code,1,3)==445]<-"Grocery"
population$naics_code_group[substr(population$naics_code,1,2)==45]<-"Retail"
population$naics_code_group[substr(population$naics_code,1,2)==48]<-"Warehouse"
population$naics_code_group[substr(population$naics_code,1,2)==49]<-"Warehouse"
population$naics_code_group[substr(population$naics_code,1,1)==5]<-"Office"
population$naics_code_group[substr(population$naics_code,1,4)==6111]<-"School K-12"
population$naics_code_group[substr(population$naics_code,1,2)==61&substr(population$naics_code,1,4)!=6111]<-"Higher Education"
population$naics_code_group[substr(population$naics_code,1,2)==71]<-"Recreation"
population$naics_code_group[substr(population$naics_code,1,1)==3]<-"Industrial"
population$naics_code_group[substr(population$naics_code,1,2)==62]<-"Healthcare"
population$naics_code_group[substr(population$naics_code,1,3)==721]<-"Hospitality"
population$naics_code_group[substr(population$naics_code,1,3)==722]<-"Restaurant"
population$naics_code_group[substr(population$naics_code,1,3)==811]<-"Repair"
population$naics_code_group[substr(population$naics_code,1,3)==812]<-"Repair"
population$naics_code_group[substr(population$naics_code,1,3)==813]<-"Religious"
population$naics_code_group[substr(population$naics_code,1,2)==92]<-"Government"
population$naics_code_group[substr(population$naics_code,1,4)==8123]<-"Laundry/Dry Cleaner"
table(population$naics_code_group,exclude = NULL)

population$naicsgroup<-population$Evergreen.categories
population$naicsgroup[population$naicsgroup=="Residential -- we'll probably want to exclude this"|is.na(population$naicsgroup)]<-"Unknown"
table(population$naicsgroup)

population$naicsgroup[population$naicsgroup=="Unknown"]<-population$naics_code_group[population$naicsgroup=="Unknown"]
table(population$naicsgroup)

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

population$elec_fuel_size<-"Unknown Size"
population$elec_fuel_size[!is.na(population$kwh2017)&population$kwh2017>0]<-"Large"
population$elec_fuel_size[population$kwh2017<500000&!is.na(population$kwh2017)&population$kwh2017>0]<-"Medium"
population$elec_fuel_size[population$kwh2017<50000&!is.na(population$kwh2017)&population$kwh2017>0]<-"Small"
population$gas_fuel_size<-"Unknown Size"
population$gas_fuel_size[!is.na(population$therms2017)&population$therms2017>0]<-"Large"
population$gas_fuel_size[population$therms2017<50000&!is.na(population$therms2017)&population$therms2017>0]<-"Medium"
population$gas_fuel_size[population$therms2017<10000&!is.na(population$therms2017)&population$therms2017>0]<-"Small"
# population$fuel_size[population$fuel_group=="Electric and Gas"]<-"Large"
# population$fuel_size[population$fuel_group=="Electric and Gas"&(population$kwh2017*0.0034121412+population$therms2017*.1)<700]<-"Medium"
# population$fuel_size[population$fuel_group=="Electric and Gas"&(population$kwh2017*0.0034121412+population$therms2017*.1)<100]<-"Small"

population$fuel_comb<-paste(population$elec_fuel_size,population$gas_fuel_size)
population$fuel_size<-"Unknown"
population$fuel_size[grepl("Large",population$fuel_comb)]<-"Large"
population$fuel_size[grepl("Medium",population$fuel_comb)]<-"Medium"
population$fuel_size[grepl("Small",population$fuel_comb)]<-"Small"

table(population$elec_fuel_size,population$fuel_group)
table(population$gas_fuel_size,population$fuel_group)
table(population$fuel_size,population$fuel_group)
table(population$fuel_size,population$naicsgroup)

# Region
zip_region<-left_join(counties,regions,by="County")
population<-left_join(population,zip_region,by=c("et_zip"="Zip.Code"))
table(population$Regions.for.EB.Process,exclude=NULL)

population<-population %>% group_by(et_city) %>% mutate(Region=ifelse(et_state=="WA","Southwest Washington",first(sort(unique(Regions.for.EB.Process)))))
table(population$Region,exclude = NULL)

test<-subset(population, is.na(Region))

region_levels<-c("Portland Metro","Northwest Oregon","Central Oregon","Southern Oregon","Eastern Oregon","Southwest Washington")

# Participation
nonpartproj<-subset(projects,programdescription=="")$et_siteid

NonPartCon<-contacts %>% filter(et_siteid%in%nonpartproj&CRMContactName==""&CRMContactEmail==""&CRMContactBusinessPhone==""&CRMContactMobilePhone=="") %>% 
  group_by(et_siteid) %>% mutate(row=1:n()) %>% filter(row==1)

population$participation<-!(population$et_siteid%in%NonPartCon$et_siteid&population$participanttype=="Non-Participant")

table(population$participation,population$participanttype)

population$participation<-ifelse(population$participation,"Participant","Non-Participant")

# Project Type
projects$SEM<-projects$projecttrackdescription=="SEM Cohort"
projects$SEM[!projects$projectid%in%SEM$ProjectId]<-FALSE
projects$Custom<-projects$projecttrackdescription=="Existing Buildings - Custom"
projects$DI<-projects$projecttrackdescription=="Existing Bldgs - Direct Install"
projects$Standard<-projects$projecttrackdescription=="Existing Buildings - Standard"
projects$Lighting<-projects$projecttrackdescription=="Existing Buildings - Lighting"

summary(projects[,c("SEM","Custom","DI","Standard","Lighting")])

# calculate year
projects$date<-as.Date(projects$installeddate)
projects$date[is.na(projects$date)]<-as.Date(projects$maxrecognizeddate[is.na(projects$date)])
projects$year<-year(projects$date)

ProjectAgg<-projects %>% 
  group_by(et_siteid) %>% 
  summarise(n_proj=n(),kWh_savings=sum(workingkwh,na.rm = TRUE),Therms_savings=sum(workingtherms,na.rm = TRUE),
    SEM=ifelse(sum(SEM)>0,"SEM ","X"),Custom=ifelse(sum(Custom)>0,"Custom ","X"),DI=ifelse(sum(DI)>0,"DI ","X"),Standard=ifelse(sum(Standard)>0,"Standard ","X"),Lighting=ifelse(sum(Lighting)>0,"Lighting ","X"),
    combo=gsub("[[:space:]]$","",gsub("X","",paste(SEM,Custom,DI,Standard,Lighting,sep=""))))

table(ProjectAgg$combo)
summary(ProjectAgg)

Proj_Pop<-left_join(ProjectAgg,population %>% select(et_siteid,participation,Region,naicsgroup,fuel_size, kwh2017,therms2017),by="et_siteid") %>% filter(!is.na(Region))

table(Proj_Pop$kWh_savings>Proj_Pop$kwh2017)  

# compare with ODE
## assign NAICS group
industries$naicsgroup<-"Unknown Commercial"
industries$naicsgroup[substr(industries$Naics,1,2)==11]<-"Industrial"
industries$naicsgroup[substr(industries$Naics,1,2)==21]<-"Industrial"
industries$naicsgroup[substr(industries$Naics,1,2)==22]<-"Industrial"
industries$naicsgroup[substr(industries$Naics,1,2)==23]<-"Industrial"
industries$naicsgroup[substr(industries$Naics,1,2)==42]<-"Retail"
industries$naicsgroup[substr(industries$Naics,1,2)==44]<-"Retail"
industries$naicsgroup[substr(industries$Naics,1,3)==445]<-"Grocery"
industries$naicsgroup[substr(industries$Naics,1,2)==45]<-"Retail"
industries$naicsgroup[substr(industries$Naics,1,2)==48]<-"Warehouse"
industries$naicsgroup[substr(industries$Naics,1,2)==49]<-"Warehouse"
industries$naicsgroup[substr(industries$Naics,1,1)==5]<-"Office"
industries$naicsgroup[substr(industries$Naics,1,4)==6111]<-"School K-12"
industries$naicsgroup[substr(industries$Naics,1,2)==61&substr(industries$Naics,1,4)!=6111]<-"Higher Education"
industries$naicsgroup[substr(industries$Naics,1,2)==71]<-"Recreation"
industries$naicsgroup[substr(industries$Naics,1,1)==3]<-"Industrial"
industries$naicsgroup[substr(industries$Naics,1,2)==62]<-"Healthcare"
industries$naicsgroup[substr(industries$Naics,1,3)==721]<-"Hospitality"
industries$naicsgroup[substr(industries$Naics,1,3)==722]<-"Restaurant"
industries$naicsgroup[substr(industries$Naics,1,3)==811]<-"Repair"
industries$naicsgroup[substr(industries$Naics,1,3)==812]<-"Repair"
industries$naicsgroup[substr(industries$Naics,1,3)==813]<-"Religious"
industries$naicsgroup[substr(industries$Naics,1,2)==92]<-"Government"
industries$naicsgroup[substr(industries$Naics,1,4)==8123]<-"Laundry/Dry Cleaner"
table(industries$naicsgroup,exclude = NULL)

pop_by_naics<-population %>% group_by(naicsgroup) %>% summarise(in_eto=n(),parts=sum(participation=="Participant"))
IndAgg<-industries %>% group_by(naicsgroup) %>% summarise(Units=sum(as.numeric(gsub(",","",Units))))
State_adj<-full_join(pop_by_naics,IndAgg,by="naicsgroup") %>% mutate(adj=(Units-parts)/(in_eto-parts))
State_adj$adj[is.na(State_adj$adj)]<-1
State_adj$adj[State_adj$naicsgroup!="Unknown Commercial"&State_adj$adj<1]<-1

State_adj$eto_NonPart<-State_adj$in_eto-State_adj$parts
State_adj$EST_NonPart<-State_adj$Units-State_adj$parts
State_adj$EST_NonPart[is.na(State_adj$EST_NonPart)|State_adj$EST_NonPart<0]<-0

# save out files for markdown
# write.csv(Proj_Pop,"/volumes/Projects/430011 - ETO Existing Buildings/Data/Project_info_for_Summaries.csv",row.names = FALSE)
# write.csv(State_adj,"/volumes/Projects/430011 - ETO Existing Buildings/Data/State_info_for_Summaries.csv",row.names = FALSE)
