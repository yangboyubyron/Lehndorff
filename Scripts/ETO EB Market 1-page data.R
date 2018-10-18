library(xlsx)
library(dplyr)
library(lubridate)
library(ggplot2)
library(reshape2)
library(cowplot)
library(knitr)
library(maps)

# Functions
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
    combo=gsub("[[:space:]]$","",gsub("X","",paste(SEM,Custom,DI,Standard,Lighting,sep=""))),n_track=5-(SEM=="X")-(Custom=="X")-(DI=="X")-(Standard=="X")-(Lighting=="X"))

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

rm(list=ls()[ls()!="Proj_Pop"&ls()!="State_adj"&ls()!="regions"])

# Functions

bound<-function(x,l,h){
  data<-x
  data[x<l]<-l
  data[x>h]<-h
  return(data)
}

subset_plot<-function(data,size_group,track_group){
  ggplot(subset(data,data[,track_group]&fuel_size==size_group) %>%
    mutate(SEM=SEM*n/types,Custom=Custom*n/types,DI=DI*n/types,Standard=Standard*n/types,Lighting=Lighting*n/types) %>% 
    select(-n,-types) %>% 
    melt(id.vars = c("fuel_size","combo")))+
  geom_bar(aes(x=combo,y=value,fill=variable),stat = "identity",position="stack")+
  labs(x=NULL,y=NULL,fill=NULL,title=paste(size_group,track_group,sep = " "))+
  guides(fill=FALSE)+
  theme(axis.text.x=element_blank(),
    plot.title = element_text(size = 7),
    axis.text.y = element_text(size=6))
}

heat_data<-function(data=Proj_Pop,sector){
  data %>%
  filter(naicsgroup==sector) %>% 
  filter(participation=="Participant") %>% 
  group_by(fuel_size) %>% 
  summarise(
    SEM=mean(ifelse(SEM=="X",0,1)),
    Custom=mean(ifelse(Custom=="X",0,1)),
    DI=mean(ifelse(DI=="X",0,1)),
    Standard=mean(ifelse(Standard=="X",0,1)),
    Lighting=mean(ifelse(Lighting=="X",0,1))
    ) %>% 
  ungroup() %>% 
  melt(id.vars="fuel_size")
}

combo_data<-function(data=Proj_Pop,sector){
  data %>% 
  filter(naicsgroup==sector) %>% 
  filter(participation=="Participant") %>% 
  filter(combo!="") %>% 
  group_by(fuel_size,combo) %>% 
  summarise(n=n()) %>% 
  mutate(types=ifelse(combo=="",0,nchar(gsub("[^\r\n\t\f\v ]","",combo))+1),
    SEM=grepl("SEM",combo),
    Custom=grepl("Custom",combo),
    DI=grepl("DI",combo),
    Standard=grepl("Standard",combo),
    Lighting=grepl("Lighting",combo))
}

table_data<-function(data=Proj_Pop,sector,for_pene=FALSE){
  pop_dat<-data %>%
    filter(naicsgroup==sector) %>% 
    left_join(State_adj %>% filter(naicsgroup==sector),by="naicsgroup") %>% 
    summarise(Sites=unique(Units),
      GWh=(sum(kwh2017[participation=="Participant"],na.rm = TRUE)+sum(kwh2017[participation=="Non-Participant"]*adj[participation=="Non-Participant"],na.rm = TRUE))/1e6,
      `Therms (Millions)`=(sum(therms2017[participation=="Participant"],na.rm = TRUE)+sum(therms2017[participation=="Non-Participant"]*adj[participation=="Non-Participant"],na.rm = TRUE))/1e6)
    
  part_dat<-data %>% 
    filter(naicsgroup==sector) %>% 
    filter(participation=="Participant") %>% 
    summarise(Sites=n_distinct(et_siteid),GWh=sum(kwh2017,na.rm = TRUE)/1e6,`Therms (Millions)`=sum(therms2017,na.rm = TRUE)/1e6)
  
  nonpart_dat<-pop_dat-part_dat
  
  save_dat<-data %>%
    filter(naicsgroup==sector) %>% 
    summarise(Sites=sum(kWh_savings>0|Therms_savings>0,na.rm = TRUE),GWh=sum(kWh_savings,na.rm = TRUE)/1e6,`Therms (Millions)`=sum(Therms_savings)/1e6)
  
  full_dat<-bind_rows(pop_dat,nonpart_dat,part_dat,save_dat)
  row.names(full_dat)<-c("Total (est)","Non-Participant", "Participant","Savings")
  
  if(for_pene==TRUE){
    full_dat$pop<-row.names(full_dat)
    full_dat<-full_dat %>% 
      filter(pop=="Participant"|pop=="Non-Participant") %>% 
      mutate(`Site Penetration`=Sites/sum(Sites),`Energy Penetration`=GWh/sum(GWh),`Gas Penetration`=`Therms (Millions)`/sum(`Therms (Millions)`)) %>% 
      select(-Sites,-GWh,-`Therms (Millions)`)
  }
  
  return(full_dat)
}

table_data_pop<-function(data=Proj_Pop){
  pop_dat<-data %>%
    left_join(State_adj,by="naicsgroup") %>% 
    group_by(naicsgroup) %>% 
    summarise(Sites=unique(Units),
      GWh=(sum(kwh2017[participation=="Participant"],na.rm = TRUE)+sum(kwh2017[participation=="Non-Participant"]*adj[participation=="Non-Participant"],na.rm = TRUE))/1e6,
      `Therms (Millions)`=(sum(therms2017[participation=="Participant"],na.rm = TRUE)+sum(therms2017[participation=="Non-Participant"]*adj[participation=="Non-Participant"],na.rm = TRUE))/1e6) %>% 
    summarise(Sites=sum(Sites,na.rm = TRUE),GWh=sum(GWh),`Therms (Millions)`=sum(`Therms (Millions)`))
    
  part_dat<-data %>% 
    filter(participation=="Participant") %>% 
    summarise(Sites=n_distinct(et_siteid),GWh=sum(as.numeric(kwh2017),na.rm = TRUE)/1e6,`Therms (Millions)`=sum(therms2017,na.rm = TRUE)/1e6)
  
  nonpart_dat<-pop_dat-part_dat
  
  save_dat<-data %>%
    summarise(Sites=sum(kWh_savings>0|Therms_savings>0,na.rm = TRUE),GWh=sum(kWh_savings,na.rm = TRUE)/1e6,`Therms (Millions)`=sum(Therms_savings)/1e6)
  
  full_dat<-bind_rows(pop_dat,nonpart_dat,part_dat,save_dat)
  row.names(full_dat)<-c("Total (est)","Non-Participant", "Participant","Savings")
  
  return(full_dat)
}

data_for_map<-function(data=Proj_Pop,sector){
  map_dat<-data %>% 
    filter(!grepl("multifamily",naicsgroup,ignore.case = TRUE)&naicsgroup!="Industrial") %>% 
    filter(participation=="Participant") %>% 
    group_by(Region) %>% 
    summarise(
      total_kwh=sum(as.numeric(kwh2017),na.rm = TRUE),
      p_save_kwh=round(sum(kWh_savings[naicsgroup==sector])/sum(kWh_savings),3)*100,
      p_use_kwh=round(sum(as.numeric(kwh2017[naicsgroup==sector]),na.rm = TRUE)/sum(as.numeric(kwh2017),na.rm = TRUE),3)*100,
      save_ratio_kwh=ifelse(p_save_kwh<=.5&p_use_kwh<=.5,0,bound(log10(p_save_kwh/p_use_kwh),-2,2)),save_diff_kwh=bound(p_save_kwh-p_use_kwh,-10,10),
      total_therms=sum(as.numeric(therms2017),na.rm = TRUE),
      p_save_therms=round(sum(Therms_savings[naicsgroup==sector])/sum(Therms_savings),3)*100,
      p_use_therms=round(sum(as.numeric(therms2017[naicsgroup==sector]),na.rm = TRUE)/sum(as.numeric(therms2017),na.rm = TRUE),3)*100,
      save_ratio_therms=ifelse(p_save_therms<=.5&p_use_therms<=.5,0,bound(log10(p_save_therms/p_use_therms),-2,2)),save_diff_therms=bound(p_save_therms-p_use_therms,-10,10)
      )
  
  map<-map_data('county') %>% subset(region == 'oregon') %>% left_join(regions %>% mutate(merge=tolower(County)),by=c("subregion"="merge"))
  
  map_out<-left_join(map,map_dat,by=c("Regions.for.EB.Process"="Region"))
  
  return(map_out)
}

comb_funct<-function(data=combos,track,metric="percent"){
  if(metric=="percent"){
    comb_out<-data[as.vector(data[,track]==TRUE),] %>% 
  group_by(Track=as.character(track),fuel_size) %>% 
  summarise(
    total=sum(n),
    SEM=ifelse(track=="SEM",sum(0,n[types==1])/total,sum(n*SEM)/total),
    Custom=ifelse(track=="Custom",sum(0,n[types==1])/total,sum(n*Custom)/total),
    DI=ifelse(track=="DI",sum(0,n[types==1])/total,sum(n*DI)/total),
    Standard=ifelse(track=="Standard",sum(0,n[types==1])/total,sum(n*Standard)/total),
    Lighting=ifelse(track=="Lighting",sum(0,n[types==1])/total,sum(n*Lighting)/total),
    Total=sum(n)/0
    ) %>% 
  select(-total) %>% 
  melt(id.vars=c("Track","fuel_size"))
    
    return(comb_out)
    }
  if(metric=="count"){
    comb_out<-data[as.vector(data[,track]==TRUE),] %>% 
  group_by(Track=as.character(track),fuel_size) %>% 
  summarise(
    total=sum(n),
    SEM=ifelse(track=="SEM",sum(0,n[types==1])/total,sum(n*SEM)/total),
    Custom=ifelse(track=="Custom",sum(0,n[types==1])/total,sum(n*Custom)/total),
    DI=ifelse(track=="DI",sum(0,n[types==1])/total,sum(n*DI)/total),
    Standard=ifelse(track=="Standard",sum(0,n[types==1])/total,sum(n*Standard)/total),
    Lighting=ifelse(track=="Lighting",sum(0,n[types==1])/total,sum(n*Lighting)/total),
    Total=sum(n)
    ) %>% 
  select(-total) %>% 
  melt(id.vars=c("Track","fuel_size")) %>% 
      mutate(value=ifelse(variable=="Total",round(value,0),paste(round(value*100,0),"%",sep="")))
    
  return(comb_out)
  }
}

pie_plot<-function(data=pie_dat){
  ggplot(data %>% filter(fuel_size!="Unknown"))+
  geom_bar(aes(x="",y=value,fill=variable),stat = "identity",position = "fill",width = 1,color="black",size=0)+
  # geom_text(aes(x="",y=value,label=round(value*100,0)))+
  coord_polar("y")+
  theme(
    text=element_text(size=14),
    legend.text = element_text(size=7),
    legend.title = element_text(size=8),
    legend.key.size = unit(.25,"inches"),
    legend.position = "left",
    axis.text = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_text(face = "bold",size=11),
    axis.ticks = element_blank(),
    strip.text.x = element_blank(),
    legend.margin=margin(0,0,0,0),
    legend.box.margin=margin(0,-4,0,0),
    plot.margin = margin(-10,0,-10,0),
    axis.line = element_blank(),
    plot.title = )+
  labs(fill="Tracks per \nSite",y="Number of Tracks per Site")+
  scale_fill_manual(
    values = c("#99CC33","darkorange","#999933","#666699","black"),
    breaks = c("N1","N2","N3","N4","N5"),
    labels = c("One","Two","Three","Four","Five"))+
  facet_grid(.~fuel_size)
}

comb_plot<-function(CombFull,TextFull){
  ggplot(CombFull %>% filter(fuel_size!="Unknown") %>% 
    mutate(t_f=factor(Track,levels = c("SEM","Custom","DI","Standard","Lighting","Total"))))+
  geom_tile(aes(x=variable,fill=value*100,y=1))+
  geom_text(data=TextFull %>% filter(fuel_size!="Unknown") %>%
      mutate(t_f=factor(Track,levels = c("SEM","Custom","DI","Standard","Lighting","Total"))),
    aes(x=variable,y=1,label=value),size=3)+
  theme(text=element_text(size=10),
    axis.text.x = element_text(angle = 0, hjust = .5,size=6),
    axis.text.y = element_blank(),axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    strip.text.y = element_text(angle = 180),
    strip.text.x = element_text(size = 12))+
  scale_fill_gradient2(high = "red",low="green",mid = "yellow",midpoint = 50,na.value = "gray90",guide = FALSE)+
  scale_x_discrete(position = "top")+
  coord_fixed(ratio = 1)+
  labs(y=NULL,x=NULL,fill="% (by row)",title="Combinations of Tracks at Sites")+
  facet_grid(t_f~fuel_size,switch = "both")
}

freq_plot<-function(for_heat,size_count){
  ggplot(left_join(for_heat,size_count,by=c("fuel_size"="Var1")))+
  # geom_tile(aes(x=variable,y=fuel_size,fill=value*100),color="black")+
  geom_tile(aes(x=fuel_size,
    y=factor(variable,levels = rev(c("SEM","Custom","DI","Standard","Lighting","Total"))),
    fill=value*100),color="black")+
  scale_fill_gradient2(high = "#000000",low="#00ff00",mid="#008700",midpoint = 50,limit=c(0,100))+
  # geom_text(aes(x=variable,y=fuel_size,label=round(value*Freq,0)),size=2)+
  geom_text(aes(x=fuel_size,
    y=factor(variable,levels = rev(c("SEM","Custom","DI","Standard","Lighting","Total"))),
    label=round(value*Freq,0)),size=2)+
   labs(title="Project Type as a \nPercentage of Total Projects",y="Project Track",
    x="Site Size",fill="Percentage \n(of size group)")+
  theme(axis.text.x = element_text(angle = 0, hjust = .5,size = 5),
    text = element_text(size=6),
    axis.text.y=element_text(size=5),
    plot.title = element_text(size=6),
    plot.margin=grid::unit(c(0,0,0,0), "mm"))+
  coord_fixed(ratio=1)
}

pene_plot<-function(data){
  ggplot(data %>% melt(id.vars="pop"))+
    geom_bar(aes(x=variable,y=value,fill=pop),stat = "identity",position = "fill")+
      labs(y="Percentage",x="Metric",title="Distribution Between \nParticipants and Non-Participants",fill="Participation")+
      scale_fill_manual(
        values = c("#99CC33","darkorange"),
        breaks = c("Participant","Non-Participant"),
        labels = c("Participants","Non-Participants")
      )+
      scale_y_continuous(labels = scales::percent)+
      theme(
        text = element_text(size=8),
        axis.text = element_text(size=6),
        plot.title = element_text(size=10))+
      coord_flip()
}

map_plot<-function(data){
  
  plot_dat<-select(data,lat,long,group,kWh=save_diff_kwh,Therms=save_diff_therms) %>% melt(id.vars=c("lat","long","group"))
  
  ggplot(plot_dat)+
    coord_quickmap(xlim = c(-124.6, -116.4), ylim = c(42, 46.3)) +
    geom_polygon(data = plot_dat, aes(x = long, y = lat,group=group,fill = value), color = "black", size = .3)+
    # scale_fill_gradient2(mid="white",high="red",low="blue",midpoint = 0,limits=c(-3,3))+
    scale_fill_gradientn(colors = rev(c("#ff0000","#ff6666","white","#9ebbff","#004CFF")),limits=c(-10,10))+
    labs(fill="Savings % - \nUsage %",title="Regional Variation:\n Difference of Subsector Participant Savings and \nSubsector Participant Usage")+
    theme(axis.title = element_blank(),axis.text = element_blank(),axis.ticks = element_blank(),axis.line = element_blank(),
    text = element_text(size=8),
    plot.title = element_text(size=8))+
    facet_grid(.~variable)
}

# Sector data

sectors<-State_adj$naicsgroup[!grepl("multif",State_adj$naicsgroup,ignore.case = TRUE)&State_adj$naicsgroup!="Unknown Commercial"&State_adj$naicsgroup!="Industrial"]

overview_full<-table_data_pop()

for (i in sectors){
  print(i)
  
  # sect_dat<-subset(Proj_Pop,naicsgroup==i)
  
  overview<-table_data(sector = i)
  overview_metric<-overview %>% mutate(pop=row.names(.),Sector=i) %>% filter(pop=="Total (est)") %>% select(Sector,Sites,GWh,`Therms (Millions)`)
  
  penetration<-table_data(sector = i,for_pene = TRUE)
  pene_metrics<-penetration %>% filter(pop=="Participant") %>% mutate(Sector=i) %>% select(Sector,`Site Penetration`,`Energy Penetration`,`Gas Penetration`)
  
  map<-data_for_map(sector = i)
  map_metrics<-map %>% 
    select(Regions.for.EB.Process,total_kwh,save_diff_kwh,total_therms,save_diff_therms) %>% 
    filter(!is.na(Regions.for.EB.Process)) %>% unique() %>% 
    summarise(Sector=i,Heat_kWh=weighted.mean(save_diff_kwh,total_kwh),Regionality_kWh=sd(save_diff_kwh),Heat_Therms=weighted.mean(save_diff_therms,total_therms),Regionality_Therms=sd(save_diff_therms))
  
  for_heat<-heat_data(sector = i)
  
  size_count<-as.data.frame(table(Proj_Pop$fuel_size[Proj_Pop$naicsgroup==i&Proj_Pop$participation=="Participant"]))

  combos<-combo_data(sector = i)

  CombSEM<-comb_funct(track = "SEM")
  CombCust<-comb_funct(track = "Custom")
  CombDI<-comb_funct(track = "DI")
  CombStan<-comb_funct(track = "Standard")
  CombLite<-comb_funct(track = "Lighting")
  
  CombFull<-bind_rows(CombSEM,CombCust,CombDI,CombStan,CombLite)
  
  TextSEM<-comb_funct(track = "SEM",metric = "count")
  TextCust<-comb_funct(track = "Custom",metric = "count")
  TextDI<-comb_funct(track = "DI",metric = "count")
  TextStan<-comb_funct(track = "Standard",metric = "count")
  TextLite<-comb_funct(track = "Lighting",metric = "count")
  
  TextFull<-bind_rows(TextSEM,TextCust,TextDI,TextStan,TextLite)
  
  pie_dat<-combos %>% 
    group_by(fuel_size) %>% 
    summarise(N1=sum(n[types==1])/sum(n),N2=sum(n[types==2])/sum(n),N3=sum(n[types==3])/sum(n),N4=sum(n[types==4])/sum(n),N5=sum(n[types==5])/sum(n)) %>% 
    melt(id.vars="fuel_size")
  
  pie_metric<-combos %>% ungroup() %>% summarise(Sector=i,`Average Tracks`=weighted.mean(types,n))
  
  output<-setNames(
    list(overview,
      overview_metric,
      penetration,
      pene_metrics,
      map,
      map_metrics,
      for_heat,
      size_count,
      combos,
      CombFull,
      TextFull,
      pie_dat,
      pie_metric),
    paste(
      c("overview_",
        "overview_metric_",
        "penetration_",
        "pene_metrics_",
        "map_",
        "map_metrics_",
        "for_heat_",
        "size_count_",
        "combos_",
        "CombFull_",
        "TextFull_",
        "pie_dat_",
        "pie_metric_"),
      i,sep=""))
  
  list2env(output,envir = globalenv())
  
  rm(overview,
    overview_metric,
    penetration,
    pene_metrics,
    map,
    map_metrics,
    for_heat,
    size_count,
    combos,
    CombFull,
    TextFull,
    pie_dat,
    pie_metric,
    CombSEM,
    CombCust,
    CombDI,
    CombStan,
    CombLite,
    TextSEM,
    TextCust,
    TextDI,
    TextStan,
    TextLite,
    output
  )
  
}

# Summary Table

overview_metric<-bind_rows(mget(apropos("overview_metric")))
pene_metric<-bind_rows(mget(apropos("pene_metric")))
pie_metric<-bind_rows(mget(apropos("pie_metric")))
map_metric<-bind_rows(mget(apropos("map_metrics")))

Summary_Table<-left_join(overview_metric,pene_metric) %>% left_join(map_metric) %>% left_join(pie_metric)

pie_plot(data = pie_dat_Grocery)

comb_plot(CombFull = `CombFull_Higher Education`,TextFull = `TextFull_Higher Education`)

freq_plot(for_heat = for_heat_Hospitality,size_count = size_count_Hospitality)

pene_plot(data=penetration_Religious)

map_plot(data = map_Hospitality)

rm(i)

save.image("/volumes/projects/430011 - ETO Existing Buildings/Data/Data_for_summaries.RData")

# save out files for markdown
# write.csv(Proj_Pop,"/volumes/Projects/430011 - ETO Existing Buildings/Data/Project_info_for_Summaries.csv",row.names = FALSE)
# write.csv(State_adj,"/volumes/Projects/430011 - ETO Existing Buildings/Data/State_info_for_Summaries.csv",row.names = FALSE)
