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
impact_include<-read.csv("/volumes/Projects/430011 - ETO Existing Buildings/Data/Include from impact.csv",stringsAsFactors = FALSE)
counties<-read.csv("/volumes/Projects/430011 - ETO Existing Buildings/Data/Oregon and SW Washington zip codes.csv",stringsAsFactors = FALSE) %>% group_by(Zip.Code) %>% summarise(County=unique(County))
regions<-read.csv("/volumes/Projects/430011 - ETO Existing Buildings/Data/ETO Regions.csv",stringsAsFactors = FALSE)

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
population$naicsgroup[substr(population$naics_code,1,4)==6111]<-"School K-12"
population$naicsgroup[substr(population$naics_code,1,2)==61&substr(population$naics_code,1,4)!=6111]<-"Higher Education"
population$naicsgroup[substr(population$naics_code,1,2)==71]<-"Recreation"
population$naicsgroup[substr(population$naics_code,1,1)==3]<-"Industrial"
population$naicsgroup[substr(population$naics_code,1,2)==62]<-"Healthcare"
population$naicsgroup[substr(population$naics_code,1,3)==721]<-"Hospitality"
population$naicsgroup[substr(population$naics_code,1,3)==722]<-"Restaurant"
population$naicsgroup[substr(population$naics_code,1,3)==811]<-"Repair"
population$naicsgroup[substr(population$naics_code,1,3)==812]<-"Repair"
population$naicsgroup[substr(population$naics_code,1,3)==813]<-"Religious"
population$naicsgroup[substr(population$naics_code,1,2)==92]<-"Government"
population$naicsgroup[substr(population$naics_code,1,4)==8123]<-"Laundry/Dry Cleaner"
table(population$naicsgroup,exclude = NULL)

projects<-projects2 %>% left_join(sector_groups,by="et_marketname") %>% left_join(select(population,et_siteid,naicsgroup),by="et_siteid")
table(projects$Evergreen.categories,exclude=NULL)

projects$Evergreen.categories[projects$Evergreen.categories=="Unknown"&!is.na(projects$Evergreen.categories)]<-projects$naicsgroup[projects$Evergreen.categories=="Unknown"&!is.na(projects$Evergreen.categories)]
table(projects$Evergreen.categories,exclude = NULL)

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

# Parts<-projects %>% filter(programdescription!="") %>% group_by(et_siteid) %>% summarise(n_measures=n(),mult_date=n_distinct(year),mult_track=n_distinct(trackval),past_year=max(year[year<2016&trackval<1000&!is.na(year)],na.rm = TRUE), recent_group=min(trackval[as.Date(date)>="2016-01-01"]),past_group=min(trackval[year==past_year],na.rm = TRUE),impact=max(impact_survey),sector=unique(Evergreen.categories),proj_incent=sum(measureincentive,na.rm = TRUE))

Parts<-projects %>% filter(programdescription!="") %>% group_by(et_siteid) %>% arrange(desc(date)) %>% 
  mutate(Project_Name=projecttitle,Project_Address=et_streetaddress,numb_projects=n_distinct(projectid),most_recent_date=first(date),most_recent_track=first(projecttrackdescription),most_recent_project=first(bcreportdescription),n_measures=n(),mult_date=n_distinct(year),mult_track=n_distinct(trackval),
    recent_year=max(year[year>=2016&trackval<1000&!is.na(year)],na.rm = TRUE),past_year=max(year[year<2016&trackval<1000&!is.na(year)],na.rm = TRUE), recent_group=min(trackval[as.Date(date)>="2016-01-01"]),past_group=min(trackval[year==past_year],na.rm = TRUE),impact=max(impact_survey),sector=unique(Evergreen.categories),proj_incent=sum(measureincentive,na.rm = TRUE),
    county=first(et_county),sqft=first(sqft),units=first(numberofunits),floors=first(numberoffloors),yearbuilt=first(yearbuilt),rental=first(rentalindicator),
    segment=ifelse((is.infinite(recent_group)|is.na(recent_group)|recent_group==1000),paste(past_group,"P",sep="-"),paste(recent_group,"R",sep="-")),rel_year=ifelse(grepl("R",segment),recent_year,past_year),rel_trackval=ifelse(grepl("R",segment),recent_group,past_group)) %>% 
  ungroup() %>% 
  filter(year==rel_year&trackval==rel_trackval) %>% 
  group_by(et_siteid) %>% mutate(row=1:n()) %>% filter(row==1) %>% 
  left_join(select(regions,-Trade.Ally.Region),by=c("county"="County")) %>% 
  left_join(select(population,c(et_siteid,kwh2017,therms2017)),by="et_siteid") %>% 
  filter(segment!="Inf-P") %>% data.frame()


# Parts<-projects %>% filter(programdescription!="") %>% group_by(et_siteid) %>% arrange(desc(date)) %>% 
#   summarise(Project_Name=first(projecttitle),Project_Address=first(et_streetaddress),numb_projects=n_distinct(projectid),proj_date=first(date),n_measures=n(),mult_date=n_distinct(year),mult_track=n_distinct(trackval),
#     past_year=max(year[year<2016&trackval<1000&!is.na(year)],na.rm = TRUE), recent_group=min(trackval[as.Date(date)>="2016-01-01"]),past_group=min(trackval[year==past_year],na.rm = TRUE),impact=max(impact_survey),sector=unique(Evergreen.categories),proj_incent=sum(measureincentive,na.rm = TRUE),
#     county=first(et_county),sqft=first(sqft),units=first(numberofunits),floors=first(numberoffloors),yearbuilt=first(yearbuilt),rental=first(rentalindicator)) %>% 
#   left_join(select(regions,-Trade.Ally.Region),by=c("county"="County")) %>% 
#   left_join(select(population,c(et_siteid,kwh2017,therms2017)),by="et_siteid")
# 
# Parts$segment<-paste(Parts$recent_group,"R",sep = "-")
# Parts$segment[is.infinite(Parts$recent_group)|is.na(Parts$recent_group)|Parts$recent_group==1000]<-paste(Parts$past_group[is.infinite(Parts$recent_group)|is.na(Parts$recent_group)|Parts$recent_group==1000],"P",sep="-")

# Parts<-Parts %>% filter(segment!="Inf-P") %>% data.frame()
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

# remove impact study parts
impact_include$match<-paste(impact_include$et_siteid,impact_include$segment,sep = " - ")
PartFrame_dedupe$match<-paste(PartFrame_dedupe$et_siteid,PartFrame_dedupe$segment,sep = " - ")
table(impact_include$match%in%PartFrame_dedupe$match)

PartFrame_dedupe<-PartFrame_dedupe %>% filter((any_impact==0|match%in%impact_include$match))
PartFrame_dedupe$year_bin<-"UNASSIGNED"
PartFrame_dedupe$year_bin[PartFrame_dedupe$year<2008&PartFrame_dedupe$year_bin=="UNASSIGNED"]<-"Pre 2008"
PartFrame_dedupe$year_bin[PartFrame_dedupe$year<2013&PartFrame_dedupe$year_bin=="UNASSIGNED"]<-"2008 through 2012"
PartFrame_dedupe$year_bin[PartFrame_dedupe$year<2019&PartFrame_dedupe$year_bin=="UNASSIGNED"]<-"Since 2013"
table(PartFrame_dedupe$year_bin)

# summary table
PartFrame_summary<-PartFrame_dedupe %>% group_by(track=sub("[[:alpha:]]+[[:space:]]","",segment),timeframe=sub("[[:space:]][[:alpha:]]+","",segment),segment) %>% summarise(count_of_contacts=n(),impact=sum(any_impact)) %>% arrange(track,desc(timeframe)) %>% data.frame()
# write.xlsx(PartFrame_summary,"/Users/Lehndorff/desktop/ETO_EB_Interview_Summary.xlsx",append = FALSE,sheetName = "Participants",row.names = FALSE)

# write out frame by segment and assign random
set.seed(729358)

j<-0
for (i in unique(PartFrame_dedupe$segment)){
  if(WRITE!=TRUE){break}
  j<-j+1
  # frameout<-PartFrame_dedupe %>% filter(segment==i) %>% select("et_siteid","segment","any_impact","n_sectors","most_common_sector","n_measures","total_incent","Primary_Contact","C1_Company","C1_phone","C1_email","C1_Current_as_of","C2_Name","C2_Company","C2_phone","C2_email","C3_Name","C3_Company","C3_phone","C3_email") %>% data.frame()
  frameout<-PartFrame_dedupe %>% filter(segment==i) %>% select("et_siteid","Project_Name","Project_Address","date","year_bin","segment","numb_projects","most_recent_date","most_recent_track","most_recent_project", "kwh2017","therms2017","Regions.for.EB.Process","sector","n_sectors","most_common_sector","n_measures","Primary_Contact","C1_Company","C1_phone","C1_email","C1_Current_as_of","C2_Name","C2_Company","C2_phone","C2_email","C3_Name","C3_Company","C3_phone","C3_email","sqft","yearbuilt","units","floors","rental") %>% data.frame()
  frameout$random<-runif(nrow(frameout),100,100+nrow(frameout))
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

# Size
NonPartPop$elec_fuel_size<-"Unknown Size"
NonPartPop$elec_fuel_size[!is.na(NonPartPop$kwh2017)&NonPartPop$kwh2017>0]<-"Large"
NonPartPop$elec_fuel_size[NonPartPop$kwh2017<500000&!is.na(NonPartPop$kwh2017)&NonPartPop$kwh2017>0]<-"Medium"
NonPartPop$elec_fuel_size[NonPartPop$kwh2017<50000&!is.na(NonPartPop$kwh2017)&NonPartPop$kwh2017>0]<-"Small"
NonPartPop$gas_fuel_size<-"Unknown Size"
NonPartPop$gas_fuel_size[!is.na(NonPartPop$therms2017)&NonPartPop$therms2017>0]<-"Large"
NonPartPop$gas_fuel_size[NonPartPop$therms2017<50000&!is.na(NonPartPop$therms2017)&NonPartPop$therms2017>0]<-"Medium"
NonPartPop$gas_fuel_size[NonPartPop$therms2017<10000&!is.na(NonPartPop$therms2017)&NonPartPop$therms2017>0]<-"Small"

NonPartPop$fuel_comb<-paste(NonPartPop$elec_fuel_size,NonPartPop$gas_fuel_size)
NonPartPop$sizegroup<-"Unknown"
NonPartPop$sizegroup[grepl("Large",NonPartPop$fuel_comb)]<-"Large"
NonPartPop$sizegroup[grepl("Medium",NonPartPop$fuel_comb)]<-"Medium"
NonPartPop$sizegroup[grepl("Small",NonPartPop$fuel_comb)]<-"Small"

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

# dedupe 2
dup_phones<-as.data.frame.table(table(c(gsub("-","",NonPartFrame_dedupe$InfousaPhone),NonPartFrame_dedupe$CostarOwnerPhone)))
dup_phones$phone_match<-as.character(dup_phones$Var1)
dup_phones$phone_freq<-as.character(dup_phones$Freq)
dup_phones<-select(dup_phones,phone_match,phone_freq) %>% filter(phone_match!="")

dup_phone_name<-as.data.frame.table(
  table(
    c(
      paste(NonPartFrame_dedupe$CostarOwnerContact,NonPartFrame_dedupe$CostarOwnerPhone),
      paste(NonPartFrame_dedupe$InfousaContactName,gsub("-","",NonPartFrame_dedupe$InfousaPhone)))))
dup_phone_name$phone_name_match<-as.character(dup_phone_name$Var1)
dup_phone_name$phone_name_freq<-as.character(dup_phone_name$Freq)
dup_phone_name<-select(dup_phone_name,phone_name_match,phone_name_freq) %>% filter(phone_name_match!=" ")

NonPartFrame_dedupe$costarphonename<-paste(NonPartFrame_dedupe$CostarOwnerContact,NonPartFrame_dedupe$CostarOwnerPhone)
NonPartFrame_dedupe$infophone<-gsub("-","",NonPartFrame_dedupe$InfousaPhone)
NonPartFrame_dedupe$infophonename<-paste(NonPartFrame_dedupe$InfousaContactName,gsub("-","",NonPartFrame_dedupe$InfousaPhone))

costar_dedupe<-NonPartFrame_dedupe %>% 
  left_join(dup_phones,by=c("CostarOwnerPhone"="phone_match")) %>% 
  left_join(dup_phone_name,by=c("costarphonename"="phone_name_match")) %>% 
  group_by(CostarOwnerPhone) %>% 
  mutate(it=1:n())

costar_dedupe$it[is.na(costar_dedupe$phone_freq)]<-NA
costar_dedupe$CostarOwnerPhone[costar_dedupe$it>1&!is.na(costar_dedupe$it)]<-paste("DUPLICATE! -",costar_dedupe$CostarOwnerPhone[costar_dedupe$it>1&!is.na(costar_dedupe$it)])

infousa_dedupe<-costar_dedupe %>% 
  left_join(dup_phones,by=c("infophone"="phone_match"),suffix=c(".costar",".infoUSA")) %>% 
  left_join(dup_phone_name,by=c("infophonename"="phone_name_match"),suffix=c(".costar",".infoUSA")) %>% 
  group_by(InfousaPhone) %>% 
  mutate(it2=1:n())

infousa_dedupe$it2[is.na(infousa_dedupe$phone_freq.infoUSA)]<-NA
infousa_dedupe$InfousaPhone[infousa_dedupe$it2>1&!is.na(infousa_dedupe$it2)]<-paste("DUPLICATE! -",infousa_dedupe$InfousaPhone[infousa_dedupe$it2>1&!is.na(infousa_dedupe$it2)])

infousa_dedupe$CostarOwnerPhone[infousa_dedupe$CostarOwnerPhone%in%infousa_dedupe$infophone&infousa_dedupe$CostarOwnerPhone!=""]<-paste("DUPLICATE! -",infousa_dedupe$CostarOwnerPhone[infousa_dedupe$CostarOwnerPhone%in%infousa_dedupe$infophone&infousa_dedupe$CostarOwnerPhone!=""])

test<-subset(infousa_dedupe,CostarOwnerPhone%in%dup_phones$phone_match[dup_phones$phone_freq>1]|infophone%in%dup_phones$phone_match[dup_phones$phone_freq>1])

# eliminate participant phone numbers
partphones<-unique(c(
  gsub("[^0-9]","",PartFrame_dedupe$C1_phone),
  gsub("[^0-9]","",PartFrame_dedupe$C2_phone),
  gsub("[^0-9]","",PartFrame_dedupe$C3_phone)
  ))

infousa_dedupe$CostarOwnerPhone[infousa_dedupe$CostarOwnerPhone%in%partphones[partphones!=""]]<-"PARTICIPANT PHONE NUMBER REMOVED"
infousa_dedupe$InfousaPhone[infousa_dedupe$infophone%in%partphones[partphones!=""]]<-"PARTICIPANT PHONE NUMBER REMOVED"

non_part_out<-infousa_dedupe %>% select(colnames(NonPartFrame_dedupe),"phone_freq.costar","phone_name_freq.costar","phone_freq.infoUSA","phone_name_freq.infoUSA",-infophone,-infophonename)

# summary table
NonPartFrame_summary<-NonPartFrame_dedupe %>% group_by(NAICS_Group=sub("[[:alpha:]]+[[:space:]]","",segment),Size=sub("[[:space:]][[:alpha:]]+","",segment),segment) %>% summarise(count_of_contacts=n()) %>% data.frame()
# write.xlsx(NonPartFrame_summary,"/Users/Lehndorff/desktop/ETO_EB_Interview_Summary.xlsx",append = TRUE,sheetName = "Non-Participants",row.names = FALSE)

# write.csv(non_part_out,"/volumes/projects/430011 - ETO Existing Buildings/Data/Sample Frames/Non_Part_Frame_1002.csv",row.names = FALSE)

# sample comparison
onlycommercial<-population %>% 
  filter(commercial==1) %>% 
  left_join(counties,by=c("et_zip"="Zip.Code")) %>% 
  left_join(regions,by="County")
fullkwh<-as.data.frame(as.list(summary(onlycommercial$kwh2017)))
row.names(fullkwh)<-"Population kWh"
fulltherm<-as.data.frame(as.list(summary(onlycommercial$therms2017)))
row.names(fulltherm)<-"Population Therms"

fullnaics<-as.data.frame.table(table(onlycommercial$naicsgroup,exclude = NULL)/nrow(onlycommercial)*100)
fullregion<-as.data.frame.table(table(onlycommercial$Regions.for.EB.Process,exclude = NULL)/nrow(onlycommercial)*100)

nonpartsamp<-population %>% filter(commercial==1) %>% 
  filter(et_siteid%in%NonPartFrame_dedupe$et_siteid) %>% 
  left_join(counties,by=c("et_zip"="Zip.Code")) %>% 
  left_join(regions,by="County")
sampkwh<-as.data.frame(as.list(summary(nonpartsamp$kwh2017)))
row.names(sampkwh)<-"Contact kWh"
samptherm<-as.data.frame(as.list(summary(nonpartsamp$therms2017)))
row.names(samptherm)<-"Contact Therms"

sampnaics<-as.data.frame.table(table(nonpartsamp$naicsgroup,exclude = NULL)/nrow(nonpartsamp)*100)
sampregion<-as.data.frame(table(nonpartsamp$Regions.for.EB.Process,exclude = NULL)/nrow(nonpartsamp)*100)

usagecomp<-rbind(fullkwh,sampkwh,fulltherm,samptherm)
naicscomp<-left_join(fullnaics,sampnaics,by="Var1",suffix=c(".Population",".Contacts"))
regioncomp<-left_join(fullregion,sampregion,by="Var1",suffix=c(".Population",".Contacts"))

write.xlsx(naicscomp,"/users/lehndorff/desktop/NonPart_Contact_Comparison.xlsx",row.names = FALSE,sheetName = "By Sector")
write.xlsx(regioncomp,"/users/lehndorff/desktop/NonPart_Contact_Comparison.xlsx",row.names = FALSE,sheetName = "By Region",append = TRUE)
write.xlsx(usagecomp,"/users/lehndorff/desktop/NonPart_Contact_Comparison.xlsx",row.names = TRUE,sheetName = "Usage Comparison",append = TRUE)

# write out frame by naics group
# write.csv(NonPartFrame_dedupe,"/volumes/Projects/430011 - ETO Existing Buildings/Data/Sample Frames/Initial_NonPart_Frame.csv",row.names = FALSE)

# Contractor summary
trade<-read.csv("/volumes/Projects/430011 - ETO Existing Buildings/Data/Trade Allies for Process Eval.csv",stringsAsFactors = FALSE)
nontrade<-read.csv("/volumes/Projects/430011 - ETO Existing Buildings/Data/EB Contractor Contact Info.csv",stringsAsFactors = FALSE)

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
table(contractor_proj$Ally)

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

# ATAC_And<-projects %>% filter(projectid%in%Has_ATAC$projectid) %>% group_by(projectid,installercompanyname) %>% summarise(years=n_distinct(year),n_total_measures=n(),ATAC_studies=sum(evaluationdescription=="Study"),ATAC_value=sum(installcost[evaluationdescription=="Study"]),non_ATAC_measures=sum(evaluationdescription!="Study"))
# mean(ATAC_And$non_ATAC_measures>0)

ATAC_out <- projects %>% filter(projectid%in%Has_ATAC$projectid) %>% group_by(installercompanyname) %>% summarise(ATAC_2016=n_distinct(projectid[evaluationdescription=="Study"&year==2016]),ATAC_2017=n_distinct(projectid[evaluationdescription=="Study"&year==2017]),
  ATAC_2018=n_distinct(projectid[evaluationdescription=="Study"&year==2018]),Total_ATAC=n_distinct(projectid[evaluationdescription=="Study"]),
  Value_2016=sum(installcost[evaluationdescription=="Study"&year==2016]),Value_2017=sum(installcost[evaluationdescription=="Study"&year==2017]),
  Value_2018=sum(installcost[evaluationdescription=="Study"&year==2018]),Total_Value=sum(installcost[evaluationdescription=="Study"]),
  non_ATAC_measures=sum(evaluationdescription!="Study"))

ATAC_out$merge<-tolower(gsub("[[:punct:]]|[[:space:]]","",substr(ATAC_out$installercompanyname,1,11)))
ATAC_out$merge[ATAC_out$installercompanyname=="Mazzetti, Inc."]<-"mazzetti"
ATAC_out$merge[ATAC_out$installercompanyname=="Northwest Engineering Services, Inc."]<-"nwesi"
ATAC_out$merge[ATAC_out$installercompanyname=="Nexant, Inc."]<-"nexant"
ATAC_out$merge[ATAC_out$installercompanyname=="PlanB Consultancy Inc"]<-"planb"
ATAC_out$merge[ATAC_out$installercompanyname=="SOLARC Architecture Inc"]<-"solarc"
ATAC_out$merge[ATAC_out$installercompanyname=="Siemens Industry INC"]<-"siemensbui"
ATAC_out$merge[ATAC_out$installercompanyname=="Trane U.S. Inc"]<-"traneusin"

ATAC_Con<-read.csv("/volumes/Projects/430011 - ETO Existing Buildings/Data/ATAC Contact List.csv",stringsAsFactors = FALSE)
ATAC_Con$merge<-tolower(gsub("[[:punct:]]|[[:space:]]","",substr(ATAC_Con$Company,1,11)))

non_match<-c(ATAC_Con$merge[!ATAC_Con$merge%in%ATAC_out$merge],ATAC_out$merge[!ATAC_out$merge%in%ATAC_Con$merge]) %>% sort()

table(ATAC_Con$merge%in%ATAC_out$merge)
table(ATAC_out$merge%in%ATAC_Con$merge)

ATAC_Con_out<-inner_join(ATAC_out,ATAC_Con,by="merge") %>% select(-merge)

# ATAC_Con_out<-ATAC_And %>% filter(ATAC_studies>0) %>% group_by(installercompanyname) %>% summarise(total_projects=n_distinct(projectid), total_measures=sum(n_total_measures),ATAC_2016=sum(ATAC_studies[years]),ATAC_2017=sum(),ATAC_2018=sum(),total_ATAC_studies=sum(ATAC_studies),total_ATAC_values=sum(ATAC_value),total_non_ATAC=sum(non_ATAC_measures))
# write.csv(ATAC_Con_out,"~/desktop/ATAC_Frame.csv",row.names = FALSE)

# Contractor Frame
trade<-read.csv("/volumes/Projects/430011 - ETO Existing Buildings/Data/Trade Allies for Process Eval.csv",stringsAsFactors = FALSE)
trade_con<-trade %>% 
  filter(Phone!="NULL"&EBProcessFlag==1) %>% 
  group_by(Account=AccountNumber,Company=TradeAllyName) %>% 
  mutate(order=1:n()) %>% 
  summarise(n=n(),Contact_Name=ContactName[order==1],Contact_Phone=Phone[order==1],Contact_Email=Email[order==1],
    Contact_2=ifelse(n>1,ContactName[order==2],"None"),Phone_2=ifelse(n>1,Phone[order==2],"None"),Email_2=ifelse(n>1,Email[order==2],"None"),
    priority=1,Ally="Trade Ally" ,Activity=unique(activity),Enrollment=unique(Program.Enrollments))

nontrade<-read.csv("/volumes/Projects/430011 - ETO Existing Buildings/Data/EB Contractor Contact Info.csv",stringsAsFactors = FALSE)
nontrade$Use_phone<-NA
nontrade$Use_phone[nontrade$ContactHomePhone!=""]<-nontrade$ContactHomePhone[nontrade$ContactHomePhone!=""]
nontrade$Use_phone[nontrade$ContactMobilePhone!=""&is.na(nontrade$Use_phone)]<-nontrade$ContactMobilePhone[nontrade$ContactMobilePhone!=""&is.na(nontrade$Use_phone)]
nontrade$Use_phone[nontrade$ContactHomePhone!=""&is.na(nontrade$Use_phone)]<-nontrade$ContactHomePhone[nontrade$ContactHomePhone!=""&is.na(nontrade$Use_phone)]
nontrade$Use_phone[nontrade$CompanyPhone!=""&is.na(nontrade$Use_phone)]<-nontrade$CompanyPhone[nontrade$CompanyPhone!=""&is.na(nontrade$Use_phone)]

nontrade_con<-nontrade %>%
  # filter(contactinfo==1&!is.na(Use_phone)) %>%
  filter(CompanyName!="") %>% 
  arrange(desc(ContactFirstName)) %>% 
  group_by(Account=accountnumber,Company=CompanyName) %>% 
  mutate(order=1:n()) %>%
  summarise(n=n(),Contact_Name=paste(ContactFirstName,ContactLastName,sep=" ")[order==1],Contact_Phone=Use_phone[order==1],Contact_Email=ContactEmail[order==1],
    Contact_2=ifelse(n>1,paste(ContactFirstName,ContactLastName,sep=" ")[order==2],"None"),Phone_2=ifelse(n>1,Use_phone[order==2],"None"),Email_2=ifelse(n>1,ContactEmail[order==2],"None"),
    priority=2, Ally="Non-Trade Ally",Activity="Unknown",Enrollment="None")

cont_con_full<-bind_rows(trade_con,nontrade_con) %>% group_by(Account,Company) %>% mutate(min=min(priority)) %>% filter(priority==min)

# dedupe by Name then phone
cont_con_dupe1<-cont_con_full %>% arrange(priority) %>% group_by(Contact_Name) %>% mutate(dupe=1:n())
cont_con_dupe1$keep<-FALSE
cont_con_dupe1$keep[cont_con_dupe1$dupe==1|cont_con_dupe1$Contact_Name==" "|cont_con_dupe1$Contact_Name=="Accounts Receivable"]<-TRUE

cont_con<-cont_con_dupe1 %>% filter(keep) %>% 
  arrange(priority) %>% group_by(Contact_Phone) %>% mutate(dupe=1:n()) %>% filter(dupe==1|is.na(Contact_Phone)) %>% select(-dupe,-min,-keep,-priority)

# contractor projects
projects$zip<-as.numeric(substr(projects$et_zipplus4,1,5))
cont_proj<-left_join(projects,counties,by=c("zip"="Zip.Code")) %>% left_join(regions,by="County")
cont_proj_agg<-cont_proj %>% filter(installercompany!=0&!is.na(installercompany)) %>% 
  group_by(installercompany) %>% 
  summarise(n_proj=n_distinct(projectid[year>=2017]),Proj_2016=sum(year==2016),Proj_2017=sum(year==2017),Proj_2018=sum(year==2018),recent_kWh=sum(workingkwh[year>=2017]),recent_therms=sum(workingtherms[year>=2017]),Common_Region=Mode(Trade.Ally.Region),Common_Track=Mode(trackval))

cont_proj_agg$Common_Track[cont_proj_agg$Common_Track==1]<-"SEM"
cont_proj_agg$Common_Track[cont_proj_agg$Common_Track==2]<-"Custom"
cont_proj_agg$Common_Track[cont_proj_agg$Common_Track==3]<-"DI"
cont_proj_agg$Common_Track[cont_proj_agg$Common_Track==4]<-"Standard"
cont_proj_agg$Common_Track[cont_proj_agg$Common_Track==5]<-"Lighting"
cont_proj_agg$Common_Track[cont_proj_agg$Common_Track==1000]<-"Other"
table(cont_proj_agg$Common_Track)

# contractor frame
cont_frame<-left_join(cont_con,cont_proj_agg,c("Account"="installercompany")) %>% filter(Common_Track!="SEM"&Common_Track!="Other"&Common_Track!="DI")

cont_frame$Activity[cont_frame$Activity=="Unknown"&cont_frame$n_proj>10]<-"High Activity"
cont_frame$Activity[cont_frame$Activity=="Unknown"&cont_frame$n_proj>4]<-"Medium Activity"
cont_frame$Activity[cont_frame$Activity=="Unknown"&cont_frame$n_proj>0]<-"Low Activity"
cont_frame$Activity[cont_frame$Activity=="Unknown"&cont_frame$n_proj==0]<-"Inactive"
table(cont_frame$Activity)

set.seed(349857)
cont_frame$Random<-runif(nrow(cont_frame),1,nrow(cont_frame))

# New as of 0920. Trade allies are listed in non-trade ally contracts. As a result, this code includes some trade allies in the sample frame, but listed as non-trade allies. 
cont_frame$drop_ally<-cont_frame$Company%in%trade$TradeAllyName&cont_frame$Ally=="Non-Trade Ally"
table(cont_frame$drop_ally,cont_frame$Ally)

cont_frame_out<-cont_frame %>% filter(Activity!="Inactive"&!drop_ally) %>%  select(-n_proj,-n,-drop_ally)
table(cont_frame_out$Company%in%trade$TradeAllyName,cont_frame_out$Ally)

# write.csv(cont_frame_out,row.names = FALSE,file="/volumes/Projects/430011 - ETO Existing Buildings/Data/Sample Frames/Contractor_Frame_0921.csv")

# for Phil 0920
proj_agg<-projects %>% 
  filter(year>=2017) %>% 
  group_by(projectid) %>% 
  mutate(total_kWh=sum(workingkwh,na.rm=TRUE),total_therms=sum(workingtherms,na.rm=TRUE)) %>% 
  group_by(projectid,installercompany,installercompanyname,total_kWh,total_therms) %>% 
  summarise(date=min(date),installer_kWh=sum(workingkwh,na.rm = TRUE),installer_therms=sum(workingtherms,narm=TRUE)) %>% 
  ungroup() %>% 
  mutate(kWh_rank=dense_rank(-total_kWh),therms_rank=dense_rank(-total_therms)) %>% 
  filter((kWh_rank<=20|therms_rank<=20)) %>% 
  arrange(kWh_rank)

# write.csv(proj_agg,"~/desktop/Contractors for Large Projects.csv",row.names = FALSE)
