# new data 7/23/19
rm(list = ls())
library(dplyr)
permit.data<-read.csv("/volumes/Projects Berkeley/401006 - PG&E MSA and Tech Assistance CWA/PG&E RTU Recruitment/Data - Confidential/072319 Data Delivery/Results - Codes & Standards RTU Permit Data Matching.csv",stringsAsFactors = FALSE)
permit.data$CZ<-as.numeric(substr(permit.data$climate_zone_cd,2,3))
table(permit.data$CZ)
# year? tonnage?

# old samples
RTUin<-read.csv("/volumes/Projects Berkeley/401006 - PG&E MSA and Tech Assistance CWA/PG&E RTU Recruitment/Data - Confidential/old sample/RTU_Frame_and_Sample.csv",stringsAsFactors = FALSE)
table(RTUin$Sample)

Sample1<-RTUin %>% filter(Sample!="Not yet sampled") %>% select(EEID,business_name,line2,exCITY,exZIP,contact_name,validEmail,emaildupe,Channel,CZ,acc1516,strata)
Sample5<-read.csv("/volumes/Projects Berkeley/401006 - PG&E MSA and Tech Assistance CWA/PG&E RTU Recruitment/Data - Confidential/4. Email/RTUdraft_SAMPLE5_0719.csv",stringsAsFactors = FALSE)
Sample6<-read.csv("/volumes/Projects Berkeley/401006 - PG&E MSA and Tech Assistance CWA/PG&E RTU Recruitment/Data - Confidential/old sample/RTUdraft_SAMPLE6_0801.csv",stringsAsFactors = FALSE)
Sample7<-read.csv("/volumes/Projects Berkeley/401006 - PG&E MSA and Tech Assistance CWA/PG&E RTU Recruitment/Data - Confidential/6. email 1K/RTUdraft_SAMPLE7_0801.csv",stringsAsFactors = FALSE)
Sample8<-read.csv("/volumes/Projects Berkeley/401006 - PG&E MSA and Tech Assistance CWA/PG&E RTU Recruitment/Data - Confidential/7. email 1K/RTUdraft_SAMPLE8_0801.csv",stringsAsFactors = FALSE)
Sample9<-read.csv("/volumes/Projects Berkeley/401006 - PG&E MSA and Tech Assistance CWA/PG&E RTU Recruitment/Data - Confidential/old sample/RTUdraft_SAMPLE9_1015.csv",stringsAsFactors = FALSE)
Sample10<-read.csv("/volumes/Projects Berkeley/401006 - PG&E MSA and Tech Assistance CWA/PG&E RTU Recruitment/Data - Confidential/old sample/RTUdraft_SAMPLE10_1015.csv",stringsAsFactors = FALSE)
Sample11<-read.csv("/volumes/Projects Berkeley/401006 - PG&E MSA and Tech Assistance CWA/PG&E RTU Recruitment/Data - Confidential/old sample/RTUdraft_SAMPLE11_1015.csv",stringsAsFactors = FALSE)
Sample12<-read.csv("/volumes/Projects Berkeley/401006 - PG&E MSA and Tech Assistance CWA/PG&E RTU Recruitment/Data - Confidential/old sample/RTUdraft_SAMPLE12_1015.csv",stringsAsFactors = FALSE)
Sample13<-read.csv("/volumes/Projects Berkeley/401006 - PG&E MSA and Tech Assistance CWA/PG&E RTU Recruitment/Data - Confidential/old sample/RTUdraft_SAMPLE13_1108.csv",stringsAsFactors = FALSE)
Sample14<-read.csv("/volumes/Projects Berkeley/401006 - PG&E MSA and Tech Assistance CWA/PG&E RTU Recruitment/Data - Confidential/old sample/RTUdraft_SAMPLE14_1108.csv",stringsAsFactors = FALSE)
Sample15<-read.csv("/volumes/Projects Berkeley/401006 - PG&E MSA and Tech Assistance CWA/PG&E RTU Recruitment/Data - Confidential/old sample/RTUdraft_SAMPLE15_1108.csv",stringsAsFactors = FALSE)
Sample16<-read.csv("/volumes/Projects Berkeley/401006 - PG&E MSA and Tech Assistance CWA/PG&E RTU Recruitment/Data - Confidential/old sample/RTUdraft_SAMPLE16_1108.csv",stringsAsFactors = FALSE)
Sample17<-read.csv("/volumes/Projects Berkeley/401006 - PG&E MSA and Tech Assistance CWA/PG&E RTU Recruitment/Data - Confidential/old sample/RTUdraft_SAMPLE17_1108.csv",stringsAsFactors = FALSE)
Sample18<-read.csv("/volumes/Projects Berkeley/401006 - PG&E MSA and Tech Assistance CWA/PG&E RTU Recruitment/Data - Confidential/old sample/RTUdraft_SAMPLE18_1108.csv",stringsAsFactors = FALSE)
Sample19<-read.csv("/volumes/Projects Berkeley/401006 - PG&E MSA and Tech Assistance CWA/PG&E RTU Recruitment/Data - Confidential/old sample/RTUdraft_FinalSample_0108.csv",stringsAsFactors = FALSE)

# PrevSamp<-bind_rows(Sample1,Sample5,Sample6,Sample7,Sample8,Sample9,Sample10,Sample11,Sample12,Sample13,Sample14,Sample15,Sample16,Sample17,Sample18,Sample19)
PrevSamp<-RTUin %>% filter(Sample!="Not yet sampled"|
    EEID%in%Sample5$EEID|
    EEID%in%Sample6$EEID|
    EEID%in%Sample7$EEID|
    EEID%in%Sample8$EEID|
    EEID%in%Sample9$EEID|
    EEID%in%Sample10$EEID|
    EEID%in%Sample11$EEID|
    EEID%in%Sample12$EEID|
    EEID%in%Sample13$EEID|
    EEID%in%Sample14$EEID|
    EEID%in%Sample15$EEID|
    EEID%in%Sample16$EEID|
    EEID%in%Sample17$EEID|
    EEID%in%Sample18$EEID|
    EEID%in%Sample19$EEID)

Sample2018<-read.csv("/Volumes/Projects Berkeley/401006 - PG&E MSA and Tech Assistance CWA/PG&E RTU Recruitment/Data - Confidential/082818 data/Deduped_2018_0910.csv",stringsAsFactors = FALSE)

# Energy Solutions data
es.data<-read.csv("/volumes/Projects Berkeley/401006 - PG&E MSA and Tech Assistance CWA/PG&E RTU Recruitment/Data - Confidential/041719 Sample/041719 Sample.csv",stringsAsFactors = FALSE)

# size flag - no tonnage
# permit.data$size.good<-?

# CZ flag (all of the CZ can be included)
permit.data$CZ.good<-permit.data$CZ%in%(c(3,12,13,2,4))
table(permit.data$CZ.good)

# local gov flag
permit.data$local_government<-FALSE
permit.data$full.name<-paste(permit.data$prsn_full_nm,permit.data$do_bus_as_nm,sep = " ... ")
permit.data$local_government[grepl("police|fire d|fire p|govern|school|district|county|city of|university|community center|PD|education|consulate",permit.data$full.name,ignore.case = TRUE)]<-TRUE
permit.data$local_government[grepl("park",permit.data$full.name,ignore.case = TRUE)&grepl("rec",permit.data$full.name,ignore.case = TRUE)]<-TRUE
permit.data$local_government[grepl("courthouse|district court|municipal court|court house",permit.data$full.name,ignore.case = TRUE)]<-TRUE
permit.data$local_government[grepl("inc|LLC|corp",permit.data$full.name,ignore.case = TRUE)]<-FALSE
permit.data$local_government[grepl("DEPARTMENT OF CORRECTIONS|NATIONAL GUARD|CITY OF|NAVAL AIR STATION",permit.data$full.name)]<-TRUE
table(permit.data$local_government)

# old addresses
PrevSamp$cleanest<-tolower(gsub("[[:punct:]]*","",PrevSamp$Site_Address_Combined2))
PrevSamp$cleanest<-gsub(" north "," n ",PrevSamp$cleanest)
PrevSamp$cleanest<-gsub(" south "," s ",PrevSamp$cleanest)
PrevSamp$cleanest<-gsub(" east "," e ",PrevSamp$cleanest)
PrevSamp$cleanest<-gsub(" west "," w ",PrevSamp$cleanest)
PrevSamp$cleanest<-gsub(" northwest "," nw ",PrevSamp$cleanest)
PrevSamp$cleanest<-gsub(" northeast "," ne ",PrevSamp$cleanest)
PrevSamp$cleanest<-gsub(" southwest "," sw ",PrevSamp$cleanest)
PrevSamp$cleanest<-gsub(" road "," rd ",PrevSamp$cleanest)
PrevSamp$cleanest<-gsub(" drive "," dr ",PrevSamp$cleanest)
PrevSamp$cleanest<-gsub(" avenue "," ave ",PrevSamp$cleanest)
PrevSamp$cleanest<-gsub(" parkway "," pkwy ",PrevSamp$cleanest)
PrevSamp$cleanest<-gsub(" boulevard "," blvd ",PrevSamp$cleanest)
PrevSamp$cleanest<-gsub(" street "," st ",PrevSamp$cleanest)
PrevSamp$cleanest<-gsub(" place "," pl ",PrevSamp$cleanest)
PrevSamp$cleanest<-gsub(" court "," ct ",PrevSamp$cleanest)
PrevSamp$cleanest<-gsub(" way "," wy ",PrevSamp$cleanest)
PrevSamp$cleanest<-gsub(" highway "," hwy ",PrevSamp$cleanest)
PrevSamp$cleanest<-gsub(" suite "," ",PrevSamp$cleanest)
PrevSamp$cleanest<-gsub(" ste "," ",PrevSamp$cleanest)
PrevSamp$cleanest<-gsub(" hse "," ",PrevSamp$cleanest)
PrevSamp$cleanest<-gsub(" #"," ",PrevSamp$cleanest,fixed = TRUE)
PrevSamp$cleanest<-gsub(" sp "," ",PrevSamp$cleanest)

# 2018 addresses
Sample2018$cleanest<-tolower(gsub("[[:punct:]]*","",Sample2018$fullad))
Sample2018$cleanest<-gsub(" north "," n ",Sample2018$cleanest)
Sample2018$cleanest<-gsub(" south "," s ",Sample2018$cleanest)
Sample2018$cleanest<-gsub(" east "," e ",Sample2018$cleanest)
Sample2018$cleanest<-gsub(" west "," w ",Sample2018$cleanest)
Sample2018$cleanest<-gsub(" northwest "," nw ",Sample2018$cleanest)
Sample2018$cleanest<-gsub(" northeast "," ne ",Sample2018$cleanest)
Sample2018$cleanest<-gsub(" southwest "," sw ",Sample2018$cleanest)
Sample2018$cleanest<-gsub(" road "," rd ",Sample2018$cleanest)
Sample2018$cleanest<-gsub(" drive "," dr ",Sample2018$cleanest)
Sample2018$cleanest<-gsub(" avenue "," ave ",Sample2018$cleanest)
Sample2018$cleanest<-gsub(" parkway "," pkwy ",Sample2018$cleanest)
Sample2018$cleanest<-gsub(" boulevard "," blvd ",Sample2018$cleanest)
Sample2018$cleanest<-gsub(" street "," st ",Sample2018$cleanest)
Sample2018$cleanest<-gsub(" place "," pl ",Sample2018$cleanest)
Sample2018$cleanest<-gsub(" court "," ct ",Sample2018$cleanest)
Sample2018$cleanest<-gsub(" way "," wy ",Sample2018$cleanest)
Sample2018$cleanest<-gsub(" highway "," hwy ",Sample2018$cleanest)
Sample2018$cleanest<-gsub(" suite "," ",Sample2018$cleanest)
Sample2018$cleanest<-gsub(" ste "," ",Sample2018$cleanest)
Sample2018$cleanest<-gsub(" hse "," ",Sample2018$cleanest)
Sample2018$cleanest<-gsub(" #"," ",Sample2018$cleanest,fixed = TRUE)
Sample2018$cleanest<-gsub(" sp "," ",Sample2018$cleanest)

# es addresses
es.data$fullad<-paste(es.data$Address,es.data$CUSTOMER.CITY,es.data$Zip.Code)
es.data$cleanest<-tolower(gsub("[[:punct:]]*","",es.data$fullad))
es.data$cleanest<-gsub(" north "," n ",es.data$cleanest)
es.data$cleanest<-gsub(" south "," s ",es.data$cleanest)
es.data$cleanest<-gsub(" east "," e ",es.data$cleanest)
es.data$cleanest<-gsub(" west "," w ",es.data$cleanest)
es.data$cleanest<-gsub(" northwest "," nw ",es.data$cleanest)
es.data$cleanest<-gsub(" northeast "," ne ",es.data$cleanest)
es.data$cleanest<-gsub(" southwest "," sw ",es.data$cleanest)
es.data$cleanest<-gsub(" road "," rd ",es.data$cleanest)
es.data$cleanest<-gsub(" drive "," dr ",es.data$cleanest)
es.data$cleanest<-gsub(" avenue "," ave ",es.data$cleanest)
es.data$cleanest<-gsub(" parkway "," pkwy ",es.data$cleanest)
es.data$cleanest<-gsub(" boulevard "," blvd ",es.data$cleanest)
es.data$cleanest<-gsub(" street "," st ",es.data$cleanest)
es.data$cleanest<-gsub(" place "," pl ",es.data$cleanest)
es.data$cleanest<-gsub(" court "," ct ",es.data$cleanest)
es.data$cleanest<-gsub(" way "," wy ",es.data$cleanest)
es.data$cleanest<-gsub(" highway "," hwy ",es.data$cleanest)
es.data$cleanest<-gsub(" suite "," ",es.data$cleanest)
es.data$cleanest<-gsub(" ste "," ",es.data$cleanest)
es.data$cleanest<-gsub(" hse "," ",es.data$cleanest)
es.data$cleanest<-gsub(" #"," ",es.data$cleanest,fixed = TRUE)
es.data$cleanest<-gsub(" sp "," ",es.data$cleanest)

# new full address
permit.data$fullad<-paste(permit.data$prem_addr_ln1_txt,paste(permit.data$prem_cty_nm, permit.data$prem_zip_5_dgt,sep=" "),sep = ", ")

permit.data$cleanest<-tolower(gsub("[[:punct:]]*","",permit.data$fullad))
permit.data$cleanest<-gsub(" north "," n ",permit.data$cleanest)
permit.data$cleanest<-gsub(" south "," s ",permit.data$cleanest)
permit.data$cleanest<-gsub(" east "," e ",permit.data$cleanest)
permit.data$cleanest<-gsub(" west "," w ",permit.data$cleanest)
permit.data$cleanest<-gsub(" northwest "," nw ",permit.data$cleanest)
permit.data$cleanest<-gsub(" northeast "," ne ",permit.data$cleanest)
permit.data$cleanest<-gsub(" southwest "," sw ",permit.data$cleanest)
permit.data$cleanest<-gsub(" road "," rd ",permit.data$cleanest)
permit.data$cleanest<-gsub(" drive "," dr ",permit.data$cleanest)
permit.data$cleanest<-gsub(" avenue "," ave ",permit.data$cleanest)
permit.data$cleanest<-gsub(" parkway "," pkwy ",permit.data$cleanest)
permit.data$cleanest<-gsub(" boulevard "," blvd ",permit.data$cleanest)
permit.data$cleanest<-gsub(" street "," st ",permit.data$cleanest)
permit.data$cleanest<-gsub(" place "," pl ",permit.data$cleanest)
permit.data$cleanest<-gsub(" court "," ct ",permit.data$cleanest)
permit.data$cleanest<-gsub(" way "," wy ",permit.data$cleanest)
permit.data$cleanest<-gsub(" highway "," hwy ",permit.data$cleanest)
permit.data$cleanest<-gsub(" suite "," ",permit.data$cleanest)
permit.data$cleanest<-gsub(" ste "," ",permit.data$cleanest)
permit.data$cleanest<-gsub(" hse "," ",permit.data$cleanest)
permit.data$cleanest<-gsub(" #"," ",permit.data$cleanest,fixed = TRUE)
permit.data$cleanest<-gsub(" sp "," ",permit.data$cleanest)

table(!permit.data$cleanest%in%PrevSamp$cleanest)
table(!permit.data$cleanest%in%Sample2018$cleanest)
table(!permit.data$cleanest%in%es.data$cleanest)

# new out (no tonnage filter)
not.new<-permit.data %>% filter(permit.data$cleanest%in%PrevSamp$cleanest|permit.data$cleanest%in%Sample2018$cleanest|permit.data$cleanest%in%es.data$cleanest)
not.new.full<-PrevSamp %>% filter(cleanest%in%not.new$cleanest)
not.new.2018<-Sample2018 %>% filter(cleanest%in%not.new$cleanest)
n_distinct(not.new$cleanest)

new.out<-permit.data %>% filter(!cleanest%in%PrevSamp$cleanest&!cleanest%in%Sample2018$cleanest&!permit.data$cleanest%in%es.data$cleanest&!local_government&CZ.good)
table(new.out$CZ)

new.out$phone1<-ifelse(new.out$prsn_phone_1=="?",NA,new.out$prsn_phone_1)
new.out$phone2<-ifelse(new.out$prsn_phone_2=="?",NA,new.out$prsn_phone_2)
new.out$phone2[new.out$phone2==new.out$phone1]<-NA
new.out$email<-ifelse(new.out$prsn_email=="?",NA,new.out$prsn_email)

new.out.sum<-new.out %>% group_by(full.name,cleanest) %>% summarise(phones=n_distinct(c(phone1,phone2),na.rm = TRUE),emails=n_distinct(email,na.rm = TRUE))
table(new.out.sum$phones)
table(new.out.sum$emails)

new.out.dedupe<-new.out %>% 
  group_by(prsn_full_nm,do_bus_as_nm,prem_cty_nm,prem_zip_5_dgt,CZ) %>% 
  summarise(Address=first(prem_addr_ln1_txt),Unique.Adds=n_distinct(prem_addr_ln1_txt),Qualifying.Units=n()) %>% 
  select(prsn_full_nm,do_bus_as_nm,Address,prem_cty_nm,prem_zip_5_dgt,CZ,Unique.Adds,Qualifying.Units) %>% arrange(prsn_full_nm,do_bus_as_nm)

most.recent<- read.csv("/volumes/Projects Berkeley/401006 - PG&E MSA and Tech Assistance CWA/PG&E RTU Recruitment/Data - Confidential/Permit_Contacts.csv",stringsAsFactors = FALSE)

