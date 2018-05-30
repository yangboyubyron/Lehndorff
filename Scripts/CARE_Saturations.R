library(dplyr)
library(xlsx)

# Part One: Penetration rates of programmable vs. manual thermostats for low income customers

weights<-read.csv("/volumes/Projects/401013 - PG&E RBSA/Data/Data - MASTER/0 - Weights/FinalWeights_BySiteID_072816.csv",stringsAsFactors = FALSE) %>% select(c(siteid,weight_strata_prop))
completed<-read.csv("/volumes/projects/401013 - PG&E RBSA/Data/Data - MASTER/99 - QC Flags Appended/completed.csv",stringsAsFactors = FALSE) %>% select(c(siteid,BuildingType_clean, CARE.Non))
LIweight<-left_join(weights,completed,by="siteid")

HVACCool<-read.csv("/volumes/projects/401013 - PG&E RBSA/Data/Data - MASTER/99 - QC Flags Appended/HVACcooling_clean.csv",stringsAsFactors = FALSE) %>% select(c(siteid,HVACControls))
HVACHeat<-read.csv("/volumes/projects/401013 - PG&E RBSA/Data/Data - MASTER/99 - QC Flags Appended/HVACheating_clean.csv",stringsAsFactors = FALSE) %>% select(c(siteid,HVACControls))

HVAC<-bind_rows(HVACCool,HVACHeat)
table(HVAC$HVACControls)
HVAC$protherm<-0
HVAC$protherm[HVAC$HVACControls=="PROGRAMMABLE THERMOSTAT"|HVAC$HVACControls=="SMART THERMOSTAT"]<-1
table(HVAC$protherm)

HVACagg<-HVAC %>% group_by(siteid) %>% summarise(protherms=sum(protherm),haspt=as.numeric(protherms>0))

PTCARE<-left_join(HVACagg,LIweight,by="siteid") %>% filter(CARE.Non!="NC")
PTCAREagg<-PTCARE %>% group_by(BuildingType_clean) %>% summarise(PT_pen=weighted.mean(haspt,w=weight_strata_prop))

# Part two: Lighting types by SF/MF and by room

rooms<-read.csv("/volumes/projects/401013 - PG&E RBSA/Data/Data - MASTER/99 - QC Flags Appended/SFroom_clean.csv",stringsAsFactors = FALSE) %>% select(c(site=siteid,room_it,RoomType)) %>% mutate(merge=paste(site,room_it,sep="-"))
rooms$RoomType[rooms$RoomType=="Master Bedroom"]<-"Bedroom"
rooms$RoomType[rooms$RoomType=="Family Room"|rooms$RoomType=="Living Room"]<-"Living/Family Room"
rooms$RoomType[rooms$RoomType=="Office"|rooms$RoomType=="Laundry Room"|rooms$RoomType=="Basement"]<-"Other"
table(rooms$RoomType)

exlite<-read.csv("/volumes/projects/401013 - PG&E RBSA/Data/Data - MASTER/5 - Final Cleaning with QC and CEC/SFextlightingLamp_clean_qc.csv",stringsAsFactors = FALSE) %>% select(c(siteid,room_it,LightingLampCategory))
inlite<-read.csv("/volumes/projects/401013 - PG&E RBSA/Data/Data - MASTER/5 - Final Cleaning with QC and CEC/SFlightingLamp_clean_qc.csv",stringsAsFactors = FALSE) %>% select(c(siteid,room_it,LightingLampCategory))

## denominator
RoomType<-left_join(rooms, completed,by=c("site"="siteid")) %>% select(c(site,merge,RoomType,BuildingType_clean))
table(RoomType$BuildingType_clean,RoomType$RoomType)

lighting<-bind_rows(exlite,inlite) %>% filter(LightingLampCategory!="Missing Lamp"&LightingLampCategory!="Non-functional Lamp"&LightingLampCategory!="Unknown") %>% mutate(merge=paste(siteid,room_it,sep="-"))
lighting$LightingLampCategory[lighting$LightingLampCategory=="Rope Lamps"]<-"Other"
table(lighting$LightingLampCategory)

# Lightagg<-lighting %>% group_by(merge=paste(siteid,room_it,sep = "-")) %>% summarise(n=n(),CF=sum(LightingLampCategory=="Compact Fluorescent")>0,Halo=sum(LightingLampCategory=="Halogen")>0,Inc=sum(LightingLampCategory=="Incandescent")>0,LED=sum(LightingLampCategory=="LED")>0,LF=sum(LightingLampCategory=="Linear Fluorescent")>0,Other=sum(LightingLampCategory=="Other")>0)
Lightagg<-lighting %>% group_by(merge=paste(siteid,room_it,sep = "-")) %>% summarise(n=n(),CF=sum(LightingLampCategory=="Compact Fluorescent"),Halo=sum(LightingLampCategory=="Halogen"),Inc=sum(LightingLampCategory=="Incandescent"),LED=sum(LightingLampCategory=="LED"),LF=sum(LightingLampCategory=="Linear Fluorescent"),Other=sum(LightingLampCategory=="Other"))

RoomLight<-left_join(RoomType,Lightagg,by="merge")
RoomLight[is.na(RoomLight)]<-0
table(RoomLight$BuildingType_clean[!RoomLight$site%in%LIweight$siteid[LIweight$CARE.Non=="NC"]],RoomLight$RoomType[!RoomLight$site%in%LIweight$siteid[LIweight$CARE.Non=="NC"]])

# RLaggCARE<-RoomLight %>% filter(!site%in%LIweight$siteid[LIweight$CARE.Non=="NC"]) %>% group_by(Building_Type=BuildingType_clean,Room_Type=RoomType) %>% summarise(n=n(),CFL=mean(CF),Halogen=mean(Halo),Incandescent=mean(Inc),LED=mean(LED),Linear_Fluorescent=mean(LF),Other=mean(Other))
RLaggCARE<-RoomLight %>% filter(!site%in%LIweight$siteid[LIweight$CARE.Non=="NC"]) %>% group_by(Building_Type=BuildingType_clean,Room_Type=RoomType) %>% summarise(n=sum(n),CFL=sum(CF)/sum(n),Halogen=sum(Halo)/sum(n),Incandescent=sum(Inc)/sum(n),LED=sum(LED)/sum(n),Linear_Fluorescent=sum(LF)/sum(n),Other=sum(Other)/sum(n))
# write.xlsx(as.data.frame(RLaggCARE),"/Users/Lehndorff/Desktop/LowIncomeDataRequest.xlsx",row.names = FALSE,append = TRUE,sheetName = "Lighting Penetration")


