# ETO EB Sample Design
library(xlsx)
library(dplyr)
library(lubridate)

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

population<-read.csv("/volumes/Projects/430011 - ETO Existing Buildings/Data/All Eligible Commercial Sites.csv",stringsAsFactors = FALSE)
projects2<-read.csv("/volumes/Projects/430011 - ETO Existing Buildings/Data/All Commercial Site Projects.csv",stringsAsFactors = FALSE)
contacts<-read.csv("/volumes/Projects/430011 - ETO Existing Buildings/Data/All Commercial Site Contacts.csv",stringsAsFactors = FALSE)
impact_surv<-read.csv("/volumes/Projects/430011 - ETO Existing Buildings/Data/EB 17 Sample Data Request v2_impact evaluation.csv",stringsAsFactors = FALSE)
SEM<-read.csv("/volumes/Projects/430011 - ETO Existing Buildings/Data/SEM impact data/SEM and Capital Participation Data to Evergreen.csv",stringsAsFactors = FALSE)
sector_groups<-read.csv("/volumes/Projects/430011 - ETO Existing Buildings/Data/EB market sector categories.csv",stringsAsFactors = FALSE) %>% select(et_marketname,Evergreen.categories)

# assign NAICS group to population
population$naicsgroup<-"Unknown"
population$naicsgroup[substr(population$naics_code,1,2)==11]<-"Industrial"
population$naicsgroup[substr(population$naics_code,1,2)==21]<-"Industrial"
population$naicsgroup[substr(population$naics_code,1,2)==22]<-"Industrial"
population$naicsgroup[substr(population$naics_code,1,2)==23]<-"Industrial"
population$naicsgroup[substr(population$naics_code,1,2)==42]<-"Retail"
population$naicsgroup[substr(population$naics_code,1,2)==44]<-"Retail"
population$naicsgroup[substr(population$naics_code,1,3)==445]<-"Grocery"
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
table(population$naicsgroup,exclude = NULL)

projects<-projects2 %>% left_join(sector_groups,by="et_marketname") %>% left_join(select(population,et_siteid,naicsgroup),by="et_siteid")
table(projects$Evergreen.categories,exclude=NULL)

projects$Evergreen.categories[projects$Evergreen.categories=="Unknown"&!is.na(projects$Evergreen.categories)]<-projects$naicsgroup[projects$Evergreen.categories=="Unknown"&!is.na(projects$Evergreen.categories)]

table(projects$projecttrackdescription)
table(projects$projecttrackdescription[projects$programdescription=="Existing Buildings"])
projects$trackval<-1000
projects$trackval[projects$projecttrackdescription=="SEM Cohort"]<-1
projects$trackval[projects$projecttrackdescription=="Existing Buildings - Custom"]<-2
projects$trackval[projects$projecttrackdescription=="Existing Bldgs - Direct Install"]<-3
projects$trackval[projects$projecttrackdescription=="Existing Buildings - Standard"]<-4
projects$trackval[projects$projecttrackdescription=="Existing Buildings - Lighting"]<-5
table(projects$trackval)

projects$date<-as.Date(projects$installeddate)
projects$date[is.na(projects$date)]<-as.Date(projects$maxrecognizeddate[is.na(projects$date)])
projects$year<-year(projects$date)

projects$confrim_SEM<-projects$projectid%in%subset(SEM,Is.SEM.customer.=="Yes")$ProjectId&projects$trackval==1
table(projects$trackval,projects$confrim_SEM)

projects$trackval[projects$trackval==1&!projects$confrim_SEM]<-1000

projects$impact_survey<-projects$projectid%in%impact_surv$projectid

Parts<-projects %>% filter(programdescription!="") %>% group_by(et_siteid) %>% summarise(n_measures=n(),mult_date=n_distinct(year),mult_track=n_distinct(trackval),past_year=max(year[year<2016&trackval<1000&!is.na(year)],na.rm = TRUE), recent_group=min(trackval[as.Date(date)>="2016-01-01"]),past_group=min(trackval[year==past_year],na.rm = TRUE),impact=max(impact_survey),sector=unique(Evergreen.categories),proj_incent=sum(measureincentive,na.rm = TRUE))
Parts$segment<-paste(Parts$recent_group,"R",sep = "-")
Parts$segment[is.infinite(Parts$recent_group)|is.na(Parts$recent_group)|Parts$recent_group==1000]<-paste(Parts$past_group[is.infinite(Parts$recent_group)|is.na(Parts$recent_group)|Parts$recent_group==1000],"P",sep="-")

Parts<-Parts %>% filter(segment!="Inf-P") %>% data.frame()
table(Parts$segment)
table(Parts$segment,Parts$impact)
table(Parts$sector)

# contacts
contacts$cont_val<-1
contacts$cont_val[contacts$CRMContactRole=="ATTENDEE"|contacts$CRMContactRole=="HOSTOWNR"|contacts$CRMContactRole=="MGRPROP"|contacts$CRMContactRole=="ACCTSREC"]<-2
contacts$cont_val[contacts$CRMContactRole==""]<-3
table(contacts$cont_val)

contacts$date<-as.Date(contacts$MostRecentProjectDate)
table(is.na(contacts$date),contacts$cont_val)

contacts$part_phone<-contacts$CRMContactMobilePhone
contacts$part_phone[contacts$part_phone==""]<-contacts$CRMContactBusinessPhone[contacts$part_phone==""]
contacts$part_phone[contacts$part_phone==""]<-contacts$CRMContactHomePhone[contacts$part_phone==""]
contacts$part_phone[contacts$part_phone==""]<-contacts$CRMCompanyPhone[contacts$part_phone==""]
contacts$part_phone[contacts$part_phone==""]<-contacts$CRMCompanyOtherPhone[contacts$part_phone==""]
table(contacts$part_phone=="")

contacts$part_email<-contacts$CRMContactEmail
contacts$part_email[contacts$part_email==""]<-contacts$CRMCompanyEmail[contacts$part_email==""]
table(contacts$part_email=="")

PartCon<-contacts %>% filter(et_siteid%in%Parts$et_siteid) %>% select(1:30,38:44) %>% unique() %>% filter(RecentSurvey==0&DoNotContact==0&CompanyDoNotContact==0&CRMContactName!=""&part_email!=""&part_phone!="") %>% group_by(et_siteid) %>% arrange(cont_val,desc(date)) %>% mutate(rank=1:n()) %>% filter(rank<=3)

PartConagg<-PartCon %>% group_by(et_siteid) %>% summarise(n=n(),companies=n_distinct(CRMCompanyName),Primary_Contact=CRMContactName[rank==1],C1_Company=CRMCompanyName[rank==1],C1_phone=part_phone[rank==1],C1_email=part_email[rank==1],C1_Current_as_of=date[rank==1],
  C2_Name=ifelse(n>1,CRMContactName[rank==2],"NA"),C2_Company=ifelse(n>1,CRMCompanyName[rank==2],"NA"),C2_phone=ifelse(n>1,part_phone[rank==2],"NA"),C2_email=ifelse(n>1,part_email[rank==2],"NA"),
  C3_Name=ifelse(n>2,CRMContactName[rank==3],"NA"),C3_Company=ifelse(n>2,CRMCompanyName[rank==3],"NA"),C3_phone=ifelse(n>2,part_phone[rank==3],"NA"),C3_email=ifelse(n>2,part_email[rank==3],"NA"))

PartFrame<-left_join(Parts,PartConagg,by="et_siteid") %>% filter(!is.na(Primary_Contact))

# dedupe
PartFrame_dedupe<-PartFrame %>% filter(sector!="Residential -- we'll probably want to exclude this") %>% group_by(Primary_Contact) %>% arrange(desc(substr(segment,3,3)),as.numeric(substr(segment,1,1))) %>% mutate(contact_n_siteids=n(),row=1:n(),any_impact=max(impact),most_common_sector=Mode(sector),n_sectors=n_distinct(sector),total_incent=sum(proj_incent)) %>% filter(row==1) %>% data.frame()
table(PartFrame_dedupe$segment)
table(PartFrame_dedupe$segment,PartFrame_dedupe$any_impact)
table(PartFrame_dedupe$sector)

PartFrame_dedupe$C2_Name[PartFrame_dedupe$C2_Name%in%PartFrame_dedupe$Primary_Contact]<-"NA"
PartFrame_dedupe$C2_Company[PartFrame_dedupe$C2_Name%in%PartFrame_dedupe$Primary_Contact]<-"NA"
PartFrame_dedupe$C2_phone[PartFrame_dedupe$C2_Name%in%PartFrame_dedupe$Primary_Contact]<-"NA"
PartFrame_dedupe$C2_email[PartFrame_dedupe$C2_Name%in%PartFrame_dedupe$Primary_Contact]<-"NA"
table(PartFrame_dedupe$C2_Name=="NA")

PartFrame_dedupe$C3_Name[PartFrame_dedupe$C3_Name%in%PartFrame_dedupe$Primary_Contact|PartFrame_dedupe$C3_Name%in%PartFrame_dedupe$C2_Name]<-"NA"
PartFrame_dedupe$C3_Company[PartFrame_dedupe$C3_Name%in%PartFrame_dedupe$Primary_Contact|PartFrame_dedupe$C3_Name%in%PartFrame_dedupe$C2_Name]<-"NA"
PartFrame_dedupe$C3_phone[PartFrame_dedupe$C3_Name%in%PartFrame_dedupe$Primary_Contact|PartFrame_dedupe$C3_Name%in%PartFrame_dedupe$C2_Name]<-"NA"
PartFrame_dedupe$C3_email[PartFrame_dedupe$C3_Name%in%PartFrame_dedupe$Primary_Contact|PartFrame_dedupe$C3_Name%in%PartFrame_dedupe$C2_Name]<-"NA"
table(PartFrame_dedupe$C3_Name=="NA")

# rename segments
PartFrame_dedupe$segment[PartFrame_dedupe$segment=="1-P"]<-"Past SEM"
PartFrame_dedupe$segment[PartFrame_dedupe$segment=="2-P"]<-"Past Custom"
PartFrame_dedupe$segment[PartFrame_dedupe$segment=="3-P"]<-"Past DI"
PartFrame_dedupe$segment[PartFrame_dedupe$segment=="4-P"]<-"Past Standard"
PartFrame_dedupe$segment[PartFrame_dedupe$segment=="5-P"]<-"Past Lighting"
PartFrame_dedupe$segment[PartFrame_dedupe$segment=="1-R"]<-"Recent SEM"
PartFrame_dedupe$segment[PartFrame_dedupe$segment=="2-R"]<-"Recent Custom"
PartFrame_dedupe$segment[PartFrame_dedupe$segment=="3-R"]<-"Recent DI"
PartFrame_dedupe$segment[PartFrame_dedupe$segment=="4-R"]<-"Recent Standard"
PartFrame_dedupe$segment[PartFrame_dedupe$segment=="5-R"]<-"Recent Lighting"
table(PartFrame_dedupe$segment)

# summary table
PartFrame_summary<-PartFrame_dedupe %>% group_by(track=sub("[[:alpha:]]+[[:space:]]","",segment),timeframe=sub("[[:space:]][[:alpha:]]+","",segment),segment) %>% summarise(count_of_contacts=n(),impact=sum(any_impact)) %>% arrange(track,desc(timeframe)) %>% data.frame()
# write.xlsx(PartFrame_summary,"/Users/Lehndorff/desktop/ETO_EB_Interview_Summary.xlsx",append = FALSE,sheetName = "Participants",row.names = FALSE)

# write out frame by segment

j<-0
for (i in unique(PartFrame_dedupe$segment)){
  if(WRITE!=TRUE){break}
  j<-j+1
  frameout<-PartFrame_dedupe %>% filter(segment==i) %>% select("et_siteid","segment","any_impact","n_sectors","most_common_sector","n_measures","total_incent","Primary_Contact","C1_Company","C1_phone","C1_email","C1_Current_as_of","C2_Name","C2_Company","C2_phone","C2_email","C3_Name","C3_Company","C3_phone","C3_email") %>% data.frame()
  if(j==1){
    write.xlsx(frameout,"/Users/Lehndorff/desktop/Part_Frame.xlsx",append = FALSE,sheetName = i,row.names = FALSE)
  }else{
    write.xlsx(frameout,"/Users/Lehndorff/desktop/Part_Frame.xlsx",append = TRUE,sheetName = i,row.names = FALSE)
  }
  if(j==n_distinct(PartFrame_dedupe$segment)){
    j<-0
    rm(i,WRITE)
  }
}

# non-parts
nonpartproj<-subset(projects,programdescription=="")$et_siteid

NonPartCon<-contacts %>% filter(et_siteid%in%nonpartproj&CRMContactName==""&CRMContactEmail==""&CRMContactBusinessPhone==""&CRMContactMobilePhone=="") %>% 
  filter((CostarOwnerName!=""&CostarOwnerContact!=""&CostarOwnerPhone!="")|(InfousaCompanyName!=""&InfousaContactName!=""&InfousaPhone!="")) %>% 
  group_by(et_siteid) %>% mutate(row=1:n()) %>% filter(row==1)

NonPartPop<-population %>% filter(et_siteid%in%NonPartCon$et_siteid&participanttype=="Non-Participant")

NonPartPop$naicsgroup<-"Unknown"
NonPartPop$naicsgroup[substr(NonPartPop$naics_code,1,2)==11]<-"Industrial"
NonPartPop$naicsgroup[substr(NonPartPop$naics_code,1,2)==21]<-"Industrial"
NonPartPop$naicsgroup[substr(NonPartPop$naics_code,1,2)==22]<-"Industrial"
NonPartPop$naicsgroup[substr(NonPartPop$naics_code,1,2)==23]<-"Industrial"
NonPartPop$naicsgroup[substr(NonPartPop$naics_code,1,2)==42]<-"Retail"
NonPartPop$naicsgroup[substr(NonPartPop$naics_code,1,2)==44]<-"Retail"
NonPartPop$naicsgroup[substr(NonPartPop$naics_code,1,2)==45]<-"Retail"
NonPartPop$naicsgroup[substr(NonPartPop$naics_code,1,2)==48]<-"Warehouse"
NonPartPop$naicsgroup[substr(NonPartPop$naics_code,1,2)==49]<-"Warehouse"
NonPartPop$naicsgroup[substr(NonPartPop$naics_code,1,1)==5]<-"Office"
NonPartPop$naicsgroup[substr(NonPartPop$naics_code,1,2)==61]<-"School"
NonPartPop$naicsgroup[substr(NonPartPop$naics_code,1,2)==71]<-"Recreation"
NonPartPop$naicsgroup[substr(NonPartPop$naics_code,1,1)==3]<-"Industrial"
NonPartPop$naicsgroup[substr(NonPartPop$naics_code,1,2)==62]<-"Medical"
NonPartPop$naicsgroup[substr(NonPartPop$naics_code,1,3)==721]<-"Hotel"
NonPartPop$naicsgroup[substr(NonPartPop$naics_code,1,3)==722]<-"Restaurant"
NonPartPop$naicsgroup[substr(NonPartPop$naics_code,1,3)==811]<-"Repair"
NonPartPop$naicsgroup[substr(NonPartPop$naics_code,1,3)==812]<-"Repair"
NonPartPop$naicsgroup[substr(NonPartPop$naics_code,1,3)==813]<-"Religious"
NonPartPop$naicsgroup[substr(NonPartPop$naics_code,1,2)==92]<-"Public"
table(NonPartPop$naicsgroup)

table(substr(NonPartPop$naics_code[NonPartPop$naicsgroup==999],1,2),exclude = FALSE)

# MMBtu
NonPartPop$MMBtu<-rowSums(NonPartPop %>% select(kwh2017,therms2017) %>% mutate(kwh2017=kwh2017*0.0034121412,therms2017=therms2017*.1),na.rm = TRUE) 
summary(NonPartPop$MMBtu)

NonPartPop$sizegroup<-"Large"
NonPartPop$sizegroup[NonPartPop$MMBtu<1000]<-"Medium"
NonPartPop$sizegroup[NonPartPop$MMBtu<100]<-"Small"
table(NonPartPop$sizegroup)

NonPartPop$segment<-paste(NonPartPop$sizegroup,NonPartPop$naicsgroup,sep = " ")
table(NonPartPop$segment)

# join to contacts
NonPartConSeg<-left_join(NonPartCon,select(NonPartPop,c(et_siteid,segment)),by="et_siteid") %>% filter(!is.na(segment))

NonPartFrame<-select(NonPartConSeg,c(et_siteid,SiteName,SiteStreetAddress,SiteCity,segment,CostarOwnerName,CostarOwnerContact,CostarOwnerPhone,InfousaCompanyName,InfousaContactName,InfousaPhone))

NonPartFrame_dedupe<-NonPartFrame %>% 
  group_by(CostarOwnerName) %>% mutate(n1=1:n(),drop1=(n1>1&CostarOwnerName!="")) %>% ungroup() %>%
  group_by(InfousaContactName) %>% mutate(n2=1:n(),drop2=(n2>1&InfousaContactName!="")) %>%
  filter(drop1==FALSE&drop2==FALSE) %>% select(-n1,-n2,-drop1,-drop2)

table(NonPartFrame_dedupe$segment)

# summary table

NonPartFrame_summary<-NonPartFrame_dedupe %>% group_by(NAICS_Group=sub("[[:alpha:]]+[[:space:]]","",segment),Size=sub("[[:space:]][[:alpha:]]+","",segment),segment) %>% summarise(count_of_contacts=n()) %>% data.frame()
# write.xlsx(NonPartFrame_summary,"/Users/Lehndorff/desktop/ETO_EB_Interview_Summary.xlsx",append = TRUE,sheetName = "Non-Participants",row.names = FALSE)

# write out frame by naics group


# Contractors
trade<-read.csv("/volumes/Projects/430011 - ETO Existing Buildings/Data/Trade Allies for Process Eval.csv",stringsAsFactors = FALSE)
trade$actval<-5
trade$actval[trade$activity=="Inactive"]<-4
trade$actval[trade$activity=="Low Activity"]<-3
trade$actval[trade$activity=="Medium Activity"]<-2
trade$actval[trade$activity=="High Activity"]<-1
table(trade$actval)

ally<-trade %>% group_by(TradeAllyName) %>% mutate(n=1:n(),use=sum(EBProcessFlag)) %>% filter(EBProcessFlag==1|(EBProcessFlag==0&n==1))
ally_act<-trade %>% group_by(TradeAllyName) %>% mutate(n=1:n(),use=sum(EBProcessFlag)) %>% filter(EBProcessFlag==1|(EBProcessFlag==0&n==1)) %>% summarise(activity=min(actval),EBproc=max(EBProcessFlag))

n_distinct(ally$TradeAllyName)==n_distinct(trade$TradeAllyName)

non_ally<-projects %>% filter(!installercompanyname%in%ally$TradeAllyName)

contractor_proj<-projects %>% mutate(Ally=installercompanyname%in%ally$TradeAllyName)
table(contractor_proj$ally)

contproj_agg<-contractor_proj %>% filter(installercompanyname!="") %>% group_by(installercompanyname) %>% summarise(n_proj=n_distinct(projectid[date>="2017-01-01"]),most_common=Mode(sort(trackval[trackval<1000&date>="2017-01-01"])),min=min(trackval,na.rm = TRUE)) %>% mutate(ally=installercompanyname%in%ally$TradeAllyName)
contproj_agg_act<-left_join(contproj_agg,ally_act,by=c("installercompanyname"="TradeAllyName"))
contproj_agg_act$EBproc[is.na(contproj_agg_act$EBproc)]<-1

contproj_agg_act$activity[is.na(contproj_agg_act$activity)&contproj_agg_act$n_proj>=10]<-1
contproj_agg_act$activity[is.na(contproj_agg_act$activity)&contproj_agg_act$n_proj>=4]<-2
contproj_agg_act$activity[is.na(contproj_agg_act$activity)&contproj_agg_act$n_proj>0]<-3
contproj_agg_act$activity[is.na(contproj_agg_act$activity)&contproj_agg_act$n_proj==0]<-4
table(contproj_agg_act$activity[!contproj_agg_act$ally])

contproj_agg_act$act_level<-"NOT DEFINED"
contproj_agg_act$act_level[contproj_agg_act$activity==1]<-"High Activity"
contproj_agg_act$act_level[contproj_agg_act$activity==2]<-"Medium Activity"
contproj_agg_act$act_level[contproj_agg_act$activity==3]<-"Low Activity"
contproj_agg_act$act_level[contproj_agg_act$activity==4]<-"Inactive"
table(contproj_agg_act$act_level)

contproj_agg_act$track_level<-"Past/Other"
contproj_agg_act$track_level[contproj_agg_act$most_common==1]<-"SEM"
contproj_agg_act$track_level[contproj_agg_act$most_common==2]<-"Custom"
contproj_agg_act$track_level[contproj_agg_act$most_common==3]<-"DI"
contproj_agg_act$track_level[contproj_agg_act$most_common==4]<-"Standard"
contproj_agg_act$track_level[contproj_agg_act$most_common==5]<-"Lighting"
table(contproj_agg_act$track_level)

contproj_agg_act$ally_level<-"Ally"
contproj_agg_act$ally_level[!contproj_agg_act$ally]<-"Non-Ally"
table(contproj_agg_act$ally_level)

contproj_agg_act$segment<-paste(contproj_agg_act$act_level,contproj_agg_act$track_level,contproj_agg_act$ally_level,sep = " ")
table(contproj_agg_act$segment)

contractor_summary<-contproj_agg_act %>% filter(EBproc!=0) %>% group_by(ally,most_common,activity,act_level,track_level,ally_level,segment) %>% summarise(number_of_companies=n()) %>% arrange(-ally,most_common,activity) %>% ungroup() %>% select(act_level,track_level,ally_level,segment,number_of_companies) %>% as.data.frame()
# write.xlsx(contractor_summary,"/Users/Lehndorff/desktop/ETO_EB_Interview_Summary.xlsx",append = TRUE,sheetName = "Contractors",row.names = FALSE)

# ATACs
table(projects$evaluationdescription[projects$projecttrackdescription=="Existing Buildings - Custom"])
Has_ATAC<-projects %>% filter(projecttrackdescription=="Existing Buildings - Custom"&evaluationdescription=="Study"&year>=2016)

table(projects$evaluationdescription[projects$projectid%in%Has_ATAC$projectid])

ATAC_Summary<-Has_ATAC %>% group_by(year) %>% summarise(n_proj=n_distinct(projectid),total_cost=sum(installcost))

ATAC_And<-projects %>% filter(projectid%in%Has_ATAC$projectid) %>% group_by(projectid,installercompanyname) %>% summarise(years=n_distinct(year),n_total_measures=n(),ATAC_studies=sum(evaluationdescription=="Study"),ATAC_value=sum(installcost[evaluationdescription=="Study"]),non_ATAC_measures=sum(evaluationdescription!="Study"))
mean(ATAC_And$non_ATAC_measures>0)


ATAC_Con_out<-ATAC_And %>% filter(ATAC_studies>0) %>% group_by(installercompanyname) %>% summarise(total_projects=n_distinct(projectid), total_measures=sum(n_total_measures),total_ATAC_studies=sum(ATAC_studies),total_ATAC_values=sum(ATAC_value),total_non_ATAC=sum(non_ATAC_measures))
# write.csv(ATAC_Con_out,"~/desktop/ATAC_Summary.csv",row.names = FALSE)

