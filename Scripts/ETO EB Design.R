# ETO EB Sample Design
library(xlsx)
library(dplyr)
library(lubridate)

population<-read.csv("/volumes/Projects/430011 - ETO Existing Buildings/Data/All Eligible Commercial Sites.csv",stringsAsFactors = FALSE)
projects<-read.csv("/volumes/Projects/430011 - ETO Existing Buildings/Data/All Commercial Site Projects.csv",stringsAsFactors = FALSE)
contacts<-read.csv("/volumes/Projects/430011 - ETO Existing Buildings/Data/All Commercial Site Contacts.csv",stringsAsFactors = FALSE)

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

Parts<-projects %>% filter(programdescription!="") %>% group_by(et_siteid) %>% summarise(n_measures=n(),mult_date=n_distinct(year),mult_track=n_distinct(trackval),past_year=max(year[year<2016&trackval<1000&!is.na(year)],na.rm = TRUE), recent_group=min(trackval[as.Date(date)>="2016-01-01"]),past_group=min(trackval[year==past_year],na.rm = TRUE))
Parts$segment<-paste(Parts$recent_group,"R",sep = "-")
Parts$segment[is.infinite(Parts$recent_group)|is.na(Parts$recent_group)|Parts$recent_group==1000]<-paste(Parts$past_group[is.infinite(Parts$recent_group)|is.na(Parts$recent_group)|Parts$recent_group==1000],"P",sep="-")

Parts<-Parts %>% filter(segment!="Inf-P") %>% data.frame()
table(Parts$segment)


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

PartCon<-contacts %>% filter(et_siteid%in%Parts$et_siteid) %>% select(1:30,39:44) %>% unique() %>% filter(DoNotContact==0&CompanyDoNotContact==0&CRMContactName!=""&part_email!=""&part_phone!="") %>% group_by(et_siteid) %>% arrange(cont_val,desc(date)) %>% mutate(rank=1:n()) %>% filter(rank<=3)

PartConagg<-PartCon %>% group_by(et_siteid) %>% summarise(n=n(),companies=n_distinct(CRMCompanyName),Primary_Contact=CRMContactName[rank==1],C1_Company=CRMCompanyName[rank==1],C1_phone=part_phone[rank==1],C1_email=part_email[rank==1],C1_Current_as_of=date[rank==1],
  C2_Name=ifelse(n>1,CRMContactName[rank==2],"NA"),C2_Company=ifelse(n>1,CRMCompanyName[rank==2],"NA"),C2_phone=ifelse(n>1,part_phone[rank==2],"NA"),C2_email=ifelse(n>1,part_email[rank==2],"NA"),
  C3_Name=ifelse(n>2,CRMContactName[rank==3],"NA"),C3_Company=ifelse(n>2,CRMCompanyName[rank==3],"NA"),C3_phone=ifelse(n>2,part_phone[rank==3],"NA"),C3_email=ifelse(n>2,part_email[rank==3],"NA"))

PartFrame<-left_join(Parts,PartConagg,by="et_siteid") %>% filter(!is.na(Primary_Contact))

# dedupe
PartFrame_dedupe<-PartFrame %>% group_by(Primary_Contact) %>% arrange(desc(substr(segment,3,3)),as.numeric(substr(segment,1,1))) %>% mutate(contact_n_siteids=n(),row=1:n()) %>% filter(row==1) %>% data.frame()
table(PartFrame_dedupe$segment)

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

j<-0
for (i in unique(PartFrame_dedupe$segment)){
  if(WRITE!=TRUE){break}
  j<-j+1
  frameout<-PartFrame_dedupe %>% filter(segment==i) %>% data.frame()
  if(j==1){
    write.xlsx(frameout,"/Users/Lehndorff/desktop/Part_Frame.xlsx",append = FALSE,sheetName = i,row.names = FALSE)
  }else{
    write.xlsx(frameout,"/Users/Lehndorff/desktop/Part_Frame.xlsx",append = TRUE,sheetName = i,row.names = FALSE)
  }
  if(j=n_distinct(PartFrame_dedupe$segment)){
    j<-0
    rm(i,WRITE)
  }
}

# non-parts
nonpartproj<-subset(projects,programdescription=="")$et_siteid

NonPartCon<-contacts %>% filter(et_siteid%in%nonpartproj&CRMContactName==""&CRMContactEmail==""&CRMContactBusinessPhone==""&CRMContactMobilePhone=="") %>% 
  filter((CostarOwnerName!=""&CostarOwnerContact!=""&CostarOwnerPhone!="")|(InfousaCompanyName!=""&InfousaContactName!=""&InfousaPhone!=""))
NonPartPop<-population %>% filter(et_siteid%in%NonPartCon$et_siteid&participanttype=="Non-Participant")

NonPartPop$naicsgroup<-999
NonPartPop$naicsgroup[substr(NonPartPop$naics_code,1,2)==11]<-3
NonPartPop$naicsgroup[substr(NonPartPop$naics_code,1,2)==21]<-3
NonPartPop$naicsgroup[substr(NonPartPop$naics_code,1,2)==22]<-3
NonPartPop$naicsgroup[substr(NonPartPop$naics_code,1,2)==23]<-3
NonPartPop$naicsgroup[substr(NonPartPop$naics_code,1,2)==42]<-44
NonPartPop$naicsgroup[substr(NonPartPop$naics_code,1,2)==44]<-44
NonPartPop$naicsgroup[substr(NonPartPop$naics_code,1,2)==45]<-44
NonPartPop$naicsgroup[substr(NonPartPop$naics_code,1,2)==48]<-48
NonPartPop$naicsgroup[substr(NonPartPop$naics_code,1,2)==49]<-48
NonPartPop$naicsgroup[substr(NonPartPop$naics_code,1,1)==5]<-5
NonPartPop$naicsgroup[substr(NonPartPop$naics_code,1,2)==61]<-61
NonPartPop$naicsgroup[substr(NonPartPop$naics_code,1,2)==71]<-71
NonPartPop$naicsgroup[substr(NonPartPop$naics_code,1,1)==3]<-3
NonPartPop$naicsgroup[substr(NonPartPop$naics_code,1,2)==62]<-62
NonPartPop$naicsgroup[substr(NonPartPop$naics_code,1,3)==721]<-721
NonPartPop$naicsgroup[substr(NonPartPop$naics_code,1,3)==722]<-722
NonPartPop$naicsgroup[substr(NonPartPop$naics_code,1,3)==811]<-811
NonPartPop$naicsgroup[substr(NonPartPop$naics_code,1,3)==812]<-811
NonPartPop$naicsgroup[substr(NonPartPop$naics_code,1,3)==813]<-813
NonPartPop$naicsgroup[substr(NonPartPop$naics_code,1,2)==92]<-92
table(NonPartPop$naicsgroup)

table(substr(NonPartPop$naics_code[NonPartPop$naicsgroup==999],1,2),exclude = FALSE)

# MMBtu
NonPartPop$MMBtu<-rowSums(NonPartPop %>% select(kwh2017,therms2017) %>% mutate(kwh2017=kwh2017*0.0034121412,therms2017=therms2017*.1),na.rm = TRUE) 
summary(NonPartPop$MMBtu)

NonPartPop %>% group_by(naicsgroup) %>% mutate(naicstotal=sum(MMBtu),)
  
  