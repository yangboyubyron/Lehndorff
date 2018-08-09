# ETO EB Sample Design
library(xlsx)
library(dplyr)
library(lubridate)
library(ggplot2)

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

population2<-read.csv("/volumes/Projects/430011 - ETO Existing Buildings/Data/All Eligible Commercial Sites.csv",stringsAsFactors = FALSE)
projects<-read.csv("/volumes/Projects/430011 - ETO Existing Buildings/Data/All Commercial Site Projects.csv",stringsAsFactors = FALSE)
contacts<-read.csv("/volumes/Projects/430011 - ETO Existing Buildings/Data/All Commercial Site Contacts.csv",stringsAsFactors = FALSE)
counties<-read.csv("/volumes/Projects/430011 - ETO Existing Buildings/Data/Oregon and SW Washington zip codes.csv",stringsAsFactors = FALSE) %>% group_by(Zip.Code) %>% summarise(County=unique(County))
regions<-read.csv("/volumes/Projects/430011 - ETO Existing Buildings/Data/ETO Regions.csv",stringsAsFactors = FALSE)
industries<-read.csv("/volumes/Projects/430011 - ETO Existing Buildings/Data/Employment by Industry.csv",stringsAsFactors = FALSE)
sector_groups<-read.csv("/volumes/Projects/430011 - ETO Existing Buildings/Data/EB market sector categories.csv",stringsAsFactors = FALSE) %>% select(et_marketname,Evergreen.categories)

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
population$elec_fuel_size[!is.na(population$kwh2017)]<-"Large"
population$elec_fuel_size[population$kwh2017<500000&!is.na(population$kwh2017)]<-"Medium"
population$elec_fuel_size[population$kwh2017<50000&!is.na(population$kwh2017)]<-"Small"
population$gas_fuel_size<-"Unknown Size"
population$gas_fuel_size[!is.na(population$therms2017)]<-"Large"
population$gas_fuel_size[population$therms2017<50000&!is.na(population$therms2017)]<-"Medium"
population$gas_fuel_size[population$therms2017<10000&!is.na(population$therms2017)]<-"Small"
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

# Participation
nonpartproj<-subset(projects,programdescription=="")$et_siteid

NonPartCon<-contacts %>% filter(et_siteid%in%nonpartproj&CRMContactName==""&CRMContactEmail==""&CRMContactBusinessPhone==""&CRMContactMobilePhone=="") %>% 
  # filter((CostarOwnerName!=""&CostarOwnerContact!=""&CostarOwnerPhone!="")|(InfousaCompanyName!=""&InfousaContactName!=""&InfousaPhone!="")) %>% 
  group_by(et_siteid) %>% mutate(row=1:n()) %>% filter(row==1)

population$participation<-!(population$et_siteid%in%NonPartCon$et_siteid&population$participanttype=="Non-Participant")

table(population$participation,population$participanttype)

population$participation<-ifelse(population$participation,"Participant","Non-Participant")

# output
# characterization<-population %>% filter(naicsgroup!="Multifamily"&naicsgroup!="Multifamily/Residential") %>% group_by(fuel_size,naicsgroup,participation) %>% summarise(n=n(),kWh=sum(as.numeric(kwh2017),na.rm = TRUE),Therms=sum(as.numeric(therms2017),na.rm = TRUE)) %>%
#   group_by(fuel_size,naicsgroup) %>% mutate(pct=round(n/sum(n),2),pctkWh=round(kWh/sum(kWh),2),pctTherms=round(Therms/sum(Therms),2)) %>% 
#   arrange(participation,naicsgroup)

population$fuel_part<-paste(population$fuel_size,population$participation)

ggplot(population %>% filter(naicsgroup!="Multifamily"&naicsgroup!="Multifamily/Residential"))+
  geom_bar(position="identity",aes(x=naicsgroup,fill=fuel_size))+
  facet_grid(participation~.)+
  coord_flip()

# ggplot(population %>% filter(naicsgroup!="Unknown Commercial"))+
#   geom_bar(position = "fill",aes(x=naicsgroup,fill=fuel_size))+
#   theme(axis.text.x = element_text(angle = 90))+
#   facet_grid(participation~.)
# 
# ggplot(population %>% filter(naicsgroup!="Unknown Commercial"))+
#   geom_bar(position = "dodge",aes(x=naicsgroup,fill=fuel_size))+
#   theme(axis.text.x = element_text(angle = 90))+
#   facet_grid(participation~.)
# 
# ggplot(characterization %>% filter(participation=="Participant"))+
#   theme(axis.text.x = element_text(angle = 90))+
#   geom_bar(stat = "identity",aes(x=paste(naicsgroup,fuel_size,sep=" "),y=pct,fill=naicsgroup))+
#   scale_fill_manual(values=rep(c("#ffb13d","#3e9933"),times=9))+
#   theme(legend.position = "none")
# 
# ggplot(characterization %>% filter(participation=="Participant"))+
#   geom_bin2d(aes(x=naicsgroup,y=fuel_size,fill=pct))+
#   theme(axis.text.x = element_text(angle = 90))+
#   scale_fill_gradient2(high = "red",low="green",mid="yellow",na.value = "white",midpoint = .5,limits=c(0,1))
# 
# ggplot(characterization)+
#   geom_bar(stat = "identity",position = "stack",aes(x=paste(naicsgroup,fuel_size,sep=" "),y=kWh,fill=participation))+
#   theme(axis.text.x = element_text(angle = 90))
# 
# ggplot(characterization)+
#   geom_bar(stat = "identity",position = "stack",aes(x=paste(naicsgroup,fuel_size,sep=" "),y=Therms,fill=participation))+
#   theme(axis.text.x = element_text(angle = 90))
# 
# ggplot(characterization)+
#   geom_bar(stat = "identity",position = "stack",aes(x=paste(naicsgroup,fuel_size,sep=" "),y=kWh,fill=participation))+
#   theme(axis.text.x = element_text(angle = 90))
# 
# ggplot(characterization)+
#   geom_bar(stat = "identity",position = "stack",aes(x=paste(naicsgroup,fuel_size,sep=" "),y=Therms,fill=participation))+
#   theme(axis.text.x = element_text(angle = 90))

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

pop_by_naics<-population %>% group_by(naicsgroup) %>% summarise(in_eto=n())
IndAgg<-industries %>% group_by(naicsgroup) %>% summarise(Units=sum(as.numeric(gsub(",","",Units))))
State_adj<-full_join(pop_by_naics,IndAgg,by="naicsgroup") %>% mutate(adj=Units/in_eto)
State_adj$adj[is.na(State_adj$adj)]<-1

# adjusted plots
counts_adj<-population %>% filter(naicsgroup!="Multifamily"&naicsgroup!="Multifamily/Residential") %>% group_by(fuel_size,naicsgroup,participation) %>% summarise(n=n()) %>% 
  left_join(State_adj,by="naicsgroup") %>% ungroup() %>% mutate(count_adj=ifelse(participation=="Non-Participant",round(n*adj),n))

ggplot(counts_adj %>% ungroup())+
  geom_bar(stat="identity",aes(x=naicsgroup,y=count_adj,fill=fuel_size))+
  facet_grid(participation~.)+
  coord_flip()

characterization_adj_kwh<-population %>% filter(naicsgroup!="Multifamily"&naicsgroup!="Multifamily/Residential") %>% group_by(elec_fuel_size,naicsgroup,participation) %>% summarise(n=n(),kwh=sum(kwh2017,na.rm = TRUE)) %>% 
  left_join(State_adj,by="naicsgroup") %>% ungroup() %>% mutate(count_adj=ifelse(participation=="Non-Participant",round(n*adj),n),kwh_adj=ifelse(participation=="Non-Participant",kwh*adj,kwh),fuel_part=ifelse(participation=="Non-Participant","A Non-Participant",paste(elec_fuel_size,participation))) %>% 
  group_by(naicsgroup,fuel_part) %>% arrange(naicsgroup,desc(fuel_part)) %>% summarise(kwh_adj=sum(kwh_adj),count_adj=sum(count_adj)) %>% group_by(naicsgroup) %>% arrange(desc(fuel_part)) %>% mutate(text=cumsum(kwh_adj)+7.5e7)

characterization_adj_kwh$count_adj[characterization_adj_kwh$fuel_part!="A Non-Participant"]<-""

ggplot(characterization_adj_kwh %>% ungroup())+
  geom_bar(stat="identity",aes(x=naicsgroup,y=kwh_adj,fill=fuel_part))+
  geom_text(aes(x=naicsgroup,y=text,label=count_adj))+
  coord_flip()

# characterization_adj<-population %>% filter(naicsgroup!="Multifamily"&naicsgroup!="Multifamily/Residential") %>% group_by(fuel_size,naicsgroup,participation) %>% summarise(n=n(),kWh=sum(kwh2017,na.rm = TRUE),Therms=sum(therms2017,na.rm = TRUE)) %>% 
#   left_join(State_adj,by="naicsgroup") %>% group_by(fuel_size,naicsgroup) %>% mutate(pct=round(n/sum(n*adj),2)) %>% 
#   arrange(participation,naicsgroup)

# characterization_adj$kWh_adj<-characterization_adj$kWh*ifelse(characterization_adj$participation=="Participant",1,characterization_adj$adj)
# characterization_adj$Therms_adj<-characterization_adj$Therms*ifelse(characterization_adj$participation=="Participant",1,characterization_adj$adj)
# 
# avg_pct<-weighted.mean(subset(characterization_adj,participation=="Participant"&naicsgroup!="Multifamily/Residential"&naicsgroup!="Unknown Commercial")$pct,w=subset(characterization_adj,participation=="Participant"&naicsgroup!="Multifamily/Residential"&naicsgroup!="Unknown Commercial")$n)



# ggplot(characterization_adj %>% filter(participation=="Participant"&naicsgroup!="Multifamily/Residential"&naicsgroup!="Unknown Commercial"))+
#   theme(axis.text.x = element_text(angle = 90))+
#   geom_point(aes(x=paste(naicsgroup,fuel_size,sep=" "),y=pct,color=naicsgroup))
# 
# ggplot(characterization_adj %>% filter(participation=="Participant"&naicsgroup!="Multifamily/Residential"&naicsgroup!="Unknown Commercial"))+
#   theme(axis.text.x = element_text(angle = 90))+
#   geom_bar(stat = "identity",aes(x=paste(naicsgroup,fuel_size,sep=" "),y=pct,fill=naicsgroup))+
#   scale_fill_manual(values=rep(c("#ffb13d","#3e9933"),times=8))+
#   theme(legend.position = "none")+
#   geom_hline(aes(yintercept=weighted.mean(avg_pct)))
# 
# ggplot(characterization_adj %>% filter(participation=="Participant"&naicsgroup!="Multifamily/Residential"&naicsgroup!="Unknown Commercial"))+
#   geom_bin2d(aes(x=naicsgroup,y=fuel_size,fill=pct))+
#   theme(axis.text.x = element_text(angle = 90))+
#   scale_fill_gradient2(high="green",low="red",midpoint=.5,mid="yellow")
# 
# ggplot(characterization_adj%>% filter(naicsgroup!="Multifamily/Residential"&naicsgroup!="Unknown Commercial"))+
#   geom_bar(stat = "identity",position = "stack",aes(x=paste(naicsgroup,fuel_size,sep=" "),y=kWh_adj,fill=participation))+
#   theme(axis.text.x = element_text(angle = 90))
# 
# ggplot(characterization_adj%>% filter(naicsgroup!="Multifamily/Residential"&naicsgroup!="Unknown Commercial"))+
#   geom_bar(stat = "identity",position = "stack",aes(x=paste(naicsgroup,fuel_size,sep=" "),y=Therms_adj,fill=participation))+
#   theme(axis.text.x = element_text(angle = 90))
