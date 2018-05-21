HomeAge<-read.csv("/volumes/Projects/401013 - PG&E RBSA/Data/Data - MASTER/2 - Restructured Tables/SF_ri_custdat.csv",stringsAsFactors = FALSE)
HVACCool<-read.csv("/volumes/Projects/401013 - PG&E RBSA/Data/Data - MASTER/3 - Initial Cleaning and QC/HVACcooling_clean.csv",stringsAsFactors = FALSE)
HVACHeat<-read.csv("/volumes/Projects/401013 - PG&E RBSA/Data/Data - MASTER/3 - Initial Cleaning and QC/HVACheating_clean.csv",stringsAsFactors = FALSE)

UseCool<-subset(HVACCool,!is.na(AC_Year))
UseHeat<-subset(HVACHeat,!is.na(HVACYear))

CoolAge<-left_join(UseCool,select(HomeAge,c(siteid,ResInt_YearBuilt)),by="siteid")
HeatAge<-left_join(UseHeat,select(HomeAge,c(siteid,ResInt_YearBuilt)),by="siteid")

CoolAge$ResInt_YearBuilt[CoolAge$ResInt_YearBuilt<1800]<-NA
HeatAge$ResInt_YearBuilt[HeatAge$ResInt_YearBuilt<1800]<-NA

CoolAge$newHVAC<-CoolAge$AC_Year>CoolAge$ResInt_YearBuilt
HeatAge$newHVAC<-HeatAge$HVACYear>HeatAge$ResInt_YearBuilt

NewCool2009<-subset(CoolAge,newHVAC&ResInt_YearBuilt<2009&AC_Year>=2009&HVACType=="CENTRAL AIR")
NewHeat2009<-subset(HeatAge,newHVAC&ResInt_YearBuilt<2009&HVACYear>=2009)

Hagecheck<-HVACHeat%>%group_by(HVACType)%>%summarise(n=n(),notNA=sum(!is.na(HVACYear)),minY=min(HVACYear,na.rm = TRUE),meanY=mean(HVACYear,na.rm = TRUE),medY=median(HVACYear,na.rm = TRUE),maxY=max(HVACYear,na.rm = TRUE))%>%arrange(-notNA)
Cagecheck<-HVACCool%>%group_by(HVACType)%>%summarise(n=n(),notNA=sum(!is.na(AC_Year)),minY=min(AC_Year,na.rm = TRUE),meanY=mean(AC_Year,na.rm = TRUE),medY=median(AC_Year,na.rm = TRUE),maxY=max(AC_Year,na.rm = TRUE))%>%arrange(-notNA)

# write.csv(Hagecheck,"~/desktop/HeatAgebyType.csv",row.names = FALSE)
# write.csv(Cagecheck,"~/desktop/CoolAgebyType.csv",row.names = FALSE)

HVACCool$AgeCat<-"No date or model number"
HVACCool$AgeCat[!is.na(HVACCool$HVACModel_clean)&HVACCool$HVACModel_clean!="UNKNOWN"]<-"Only has model number"
HVACCool$AgeCat[HVACCool$AC_Year>1900]<-"Has year"
table(HVACCool$AgeCat)

HVACHeat$AgeCat<-"No date or model number"
HVACHeat$AgeCat[!is.na(HVACHeat$HVACModel_clean)]<-"Only has model number"
HVACHeat$AgeCat[HVACHeat$HVACYear>1900]<-"Has year"
table(HVACHeat$AgeCat)

WH<-read.csv("/volumes/Projects/401013 - PG&E RBSA/Data/Data - MASTER/3 - Initial Cleaning and QC/SFwheater_clean.csv",stringsAsFactors = FALSE)

UseWH<-subset(WH,!is.na(WHManYear))

WHAge<-left_join(UseWH,select(HomeAge,c(siteid,ResInt_YearBuilt)),by="siteid")

WHAge$ResInt_YearBuilt[WHAge$ResInt_YearBuilt<1800]<-NA

WHAge$newWH<-WHAge$WHManYear>WHAge$ResInt_YearBuilt

NewWH2009<-subset(WHAge,newWH&ResInt_YearBuilt<2009&WHManYear>=2009)

WHcheck<-WH%>%group_by(WaterheaterType)%>%summarise(n=n(),HasYear=sum(!is.na(WHManYear)),minYear=min(WHManYear,na.rm = TRUE),meanYear=mean(WHManYear,na.rm = TRUE),medYear=median(WHManYear,na.rm = TRUE),maxYear=max(WHManYear,na.rm = TRUE))%>%arrange(-HasYear)

# write.csv(WHcheck,"~/desktop/WHAgebyType.csv",row.names = FALSE)

BuildType<-read.csv("/volumes/Projects/401013 - PG&E RBSA/Data/Data - MASTER/3 - Initial Cleaning and QC/SFMaster_housegeometry_clean.csv",stringsAsFactors = FALSE)
TypeAge<-left_join(BuildType,HomeAge,by="siteid")

AgeAgg<-TypeAge %>% group_by(BuildType_rolled.y) %>% summarise(n=n(),Built2009orLater=sum(ResInt_YearBuilt>=2009))
# write.csv(AgeAgg,"~/desktop/AgebyType.csv",row.names = FALSE)

table(HomeAge$ResInt_YearBuilt>2009&HomeAge$ResInt_YearBuilt<2018)

OldHomes<-subset(HomeAge,ResInt_YearBuilt<2009&ResInt_YearBuilt>1800)$siteid

Walls<-read.csv("/volumes/Projects/401013 - PG&E RBSA/Data/Data - MASTER/3 - Initial Cleaning and QC/SFframedwall_clean.csv",stringsAsFactors = FALSE)
Wallsagg<-Walls %>% group_by(siteid) %>% summarise(useR=sum(FramedInsLvl!="Unknown"&FramedInsLvl!="Measured Thickness"&!is.na(FramedInsLvl)))
Ducts<-read.csv("/volumes/Projects/401013 - PG&E RBSA/Data/Data - MASTER/2 - Restructured Tables/SFducts.csv",stringsAsFactors = FALSE)
Ductsagg<-Ducts %>% group_by(siteid) %>% summarise(ducts=sum(Ducts_Present),useRmetal=sum(all_metal_insulation_level!="N/A"&all_metal_insulation_level!="Unknown"&!is.na(all_metal_insulation_level)),useRflex=sum(flex_insulation_type!="N/A"&flex_insulation_type!="Unknown"&!is.na(flex_insulation_type)),all=sum(useRflex,useRmetal))
  
centralHeat<-HVACHeat %>% filter(Type_clean=="Forced Air Furnace"|Type_clean=="Heat Pump")
CHeatType<-left_join(centralHeat,BuildType,by="siteid")
Heat2009Ratio<-sum(CHeatType$HVACYear>=2009,na.rm = TRUE)/sum(CHeatType$HVACYear>=1900,na.rm = TRUE)
CHeataggOld1<-CHeatType %>% filter(siteid%in%OldHomes)%>% group_by(siteid) %>% summarise(BType=unique(BuildType_rolled.y),Has1Model=max(!is.na(HVACModel_clean))) 
CHeataggOld<-CHeataggOld1%>% group_by(BType) %>% summarise(n=n(),HasModel=sum(Has1Model),Aft2009Est=trunc(HasModel*Heat2009Ratio))
# CHeataggOld<-CHeatType %>% filter(siteid%in%OldHomes) %>% group_by(BuildType_rolled.y) %>% summarise(n=n(),HasModelOnly=sum(!is.na(HVACModel_clean)&is.na(HVACYear)), HasYear=sum(!is.na(HVACYear)),YearAft2009=sum(HVACYear>=2009,na.rm = TRUE),Aft2009Est=trunc(YearAft2009+(YearAft2009/HasYear*HasModelOnly)))
write.xlsx(as.data.frame(CHeataggOld),"/Users/Lehndorff/desktop/MFSF.xlsx",sheetName = "Central Heat",row.names = FALSE)

WHtype<-left_join(WH,BuildType,by="siteid") 
WH2009Ratio<-sum(WHtype$WHManYear>=2009,na.rm=TRUE)/sum(WHtype$WHManYear>=1900,na.rm=TRUE)
WHtypeaggOld1<-WHtype %>% filter(siteid%in%OldHomes) %>% group_by(siteid) %>% summarise(BType=unique(BuildType_rolled.y),Has1Model=max(!is.na(WHModel_clean)))
WHtypeaggOld<-WHtypeaggOld1%>% group_by(BType) %>% summarise(n=n(),HasModel=sum(Has1Model),Aft2009Est=trunc(HasModel*WH2009Ratio))
# WHtypeaggOld<-WHtype %>% filter(siteid%in%OldHomes) %>% group_by(BuildType_rolled.y) %>% summarise(n=n(),HasModelOnly=sum(!is.na(WHModel_clean)&is.na(WHManYear)), HasYear=sum(!is.na(WHManYear)),YearAft2009=sum(WHManYear>=2009,na.rm = TRUE),Aft2009Est=trunc(YearAft2009+(YearAft2009/HasYear*HasModelOnly)))
write.xlsx(as.data.frame(WHtypeaggOld),"/Users/Lehndorff/desktop/MFSF.xlsx",sheetName = "Water Heaters",row.names = FALSE, append = TRUE)

CoolType<-left_join(HVACCool,BuildType,by="siteid") %>% filter(HVACType!="MULTIFAMILY CENTRAL SYSTEM")
Cool2009Ratio<-sum(CoolType$AC_Year>=2009,na.rm = TRUE)/sum(CoolType$AC_Year>=1900,na.rm = TRUE)
CoolTypeaggOld1<-CoolType %>% filter(siteid%in%OldHomes) %>% group_by(siteid) %>% summarise(BType=unique(BuildType_rolled.y),Has1Model=max(!is.na(HVACModel_clean)&is.na(AC_Year)&HVACModel_clean!="UNKNOWN"&HVACModel_clean!="UNK"&HVACModel_clean!="X"&HVACModel_clean!="INACCESSIBLE")) 
CoolTypeaggOld<-CoolTypeaggOld1%>% group_by(BType) %>% summarise(n=n(),HasModel=sum(Has1Model),Aft2009Est=trunc(HasModel*Cool2009Ratio))
# CoolTypeaggOld<-CoolType %>% filter(siteid%in%OldHomes) %>% group_by(BuildType_rolled.y) %>% summarise(n=n(),HasModelOnly=sum(!is.na(HVACModel_clean)&is.na(AC_Year)&HVACModel_clean!="UNKNOWN"&HVACModel_clean!="UNK"&HVACModel_clean!="X"&HVACModel_clean!="INACCESSIBLE"), HasYear=sum(!is.na(AC_Year)),YearAft2009=sum(AC_Year>=2009,na.rm = TRUE),Aft2009Est=trunc(YearAft2009+(YearAft2009/HasYear*HasModelOnly)))
write.xlsx(as.data.frame(CoolTypeaggOld),"/Users/Lehndorff/desktop/MFSF.xlsx",sheetName = "HVAC Cooling",row.names = FALSE, append = TRUE)

WallsType<-left_join(Wallsagg,BuildType, by="siteid")
WallTypeaggOld1<-WallsType %>% filter(siteid%in%OldHomes)
WallTypeaggOld<-WallTypeaggOld1%>% group_by(BuildType_rolled.y) %>% summarise(n=n(),HomeWithR=sum(useR>0))
write.xlsx(as.data.frame(WallTypeaggOld),"/Users/Lehndorff/desktop/MFSF.xlsx",sheetName = "Wall Insulation",row.names = FALSE, append = TRUE)

DuctsType<-left_join(Ductsagg,BuildType, by="siteid")
DuctTypeaggOld1<-DuctsType %>% filter(siteid%in%OldHomes) 
DuctTypeaggOld<-DuctTypeaggOld1 %>% group_by(BuildType_rolled.y) %>% summarise(n=sum(ducts>0),HomeWithR=sum(all>0))
write.xlsx(as.data.frame(DuctTypeaggOld),"/Users/Lehndorff/desktop/MFSF.xlsx",sheetName = "Duct Insulation",row.names = FALSE, append = TRUE)

HomeIDs<-left_join(data.frame(siteid=unique(c(CHeataggOld1$siteid,WHtypeaggOld1$siteid,CoolTypeaggOld1$siteid,WallTypeaggOld1$siteid,DuctTypeaggOld1$siteid))),BuildType,by="siteid")
HomeIDsagg<-HomeIDs %>% group_by(BuildType_rolled.y) %>% summarise(n=n())
write.xlsx(as.data.frame(HomeIDsagg),"/Users/Lehndorff/desktop/MFSF.xlsx",sheetName = "Total Homes",row.names = FALSE, append = TRUE)

ModelNumIDs<-left_join(data.frame(siteid=unique(c(subset(CHeataggOld1,Has1Model==1)$siteid,subset(CoolTypeaggOld1,Has1Model==1)$siteid,subset(WHtypeaggOld1,Has1Model==1)$siteid,subset(WallTypeaggOld1,useR>0)$siteid,subset(DuctTypeaggOld1,all>0)$siteid))),BuildType,by="siteid")
ModNumagg<-ModelNumIDs %>% group_by(BuildType_rolled.y) %>% summarise(HasModel=n())
write.xlsx(as.data.frame(ModNumagg),"/Users/Lehndorff/desktop/MFSF.xlsx",sheetName = "Home with min. 1 Model #",row.names = FALSE, append = TRUE)

