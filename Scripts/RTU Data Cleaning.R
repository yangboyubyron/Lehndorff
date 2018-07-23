library(dplyr)
library(xlsx)
.simpleCap <- function(x) {
    x<-tolower(x)
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1, 1)), substring(s, 2),
          sep = "", collapse = " ")
}

RTUdata<-read.csv("~/desktop/PG&E_RTUcsv.csv",stringsAsFactors = FALSE)
EEIDS<-read.csv("~/desktop/EE_RTU_IDs.csv",stringsAsFactors=FALSE)

Remove<-read.xlsx("/volumes/Projects Berkeley/401006 - PG&E MSA and Tech Assistance CWA/PG&E RTU Recruitment/RTU Project (for desktop)/Data - Confidential/From Data Transfer/Raw Data/Channel A (Upstream HVAC) Data.xlsx",sheetIndex = 2)
Remove$Date<-as.Date(paste(as.character(as.vector(Remove$Estimated.Install.Date)),1,as.numeric(as.vector(Remove$Years)),sep = "-"),format = "%b-%d-%Y")
Remove$addclean<-paste(Remove$Address,","," ",Remove$City, " ",Remove$Zip.Code,sep = "")
Remove<-subset(Remove,Date<"2014-07-01")

RAW_C<-read.csv("/volumes/Projects Berkeley/401006 - PG&E MSA and Tech Assistance CWA/PG&E RTU Recruitment/RTU Project (for desktop)/Data - Confidential/From Data Transfer/Raw Data/PGE Billing Data.csv",stringsAsFactors = FALSE)

RTUdatasave<-RTUdata

# clean channel
RTUdata$channel_clean<-"C"
RTUdata$channel_clean[RTUdata$ChanA>0]<-"A"
RTUdata$channel_clean[RTUdata$chanB>0]<-"B"
table(RTUdata$channel_clean)

# 6 fail zip code
RTUdata$exZIP<-"NO ZIP CODE"
RTUdata$exZIP[grepl("[[:digit:]]+$",RTUdata$Site_Address_Combined2)]<-substr(RTUdata$Site_Address_Combined2, regexpr("[[:digit:]]+$",RTUdata$Site_Address_Combined2),regexpr("[[:digit:]]+$",RTUdata$Site_Address_Combined2)+4)[grepl("[[:digit:]]+$",RTUdata$Site_Address_Combined2)]
table(RTUdata$exZIP)

# 61 fail city
RTUdata$exCITY<-"NO CITY"
RTUdata$exCITY<-substr(RTUdata$Site_Address_Combined2,regexpr("[,][^\\,]*$",RTUdata$Site_Address_Combined2)+2,regexpr("[[:digit:]]+$",RTUdata$Site_Address_Combined2)-2)
# table(RTUdata$exCITY)
RTUdata$exCITY2[RTUdata$exCITY==""]<-substr(RTUdata$Site_Address_Combined2,regexpr("[,][^z]{7,50}$",RTUdata$Site_Address_Combined2)+2,regexpr("[[:digit:]]+$",RTUdata$Site_Address_Combined2)-3)[RTUdata$exCITY==""]

RTUdata$line2<-substr(RTUdata$Site_Address_Combined2,0,regexpr("[,][^\\,]*$",RTUdata$Site_Address_Combined2)-1)
RTUdata$line2_2[RTUdata$exCITY==""]<-substr(RTUdata$Site_Address_Combined2,0,regexpr("[,][^z]{7,50}$",RTUdata$Site_Address_Combined2)-1)[RTUdata$exCITY==""]

RTUdata$exCITY[RTUdata$exCITY==""]<-RTUdata$exCITY2[RTUdata$exCITY==""]
RTUdata$line2[RTUdata$exCITY==""]<-RTUdata$line2_2[RTUdata$exCITY==""]

RTUdata$exCITY[RTUdata$exZIP=="NO ZIP CODE"]<-RTUdata$MailingZip[RTUdata$exZIP=="NO ZIP CODE"]
RTUdata$exZIP[RTUdata$exZIP=="NO ZIP CODE"]<-RTUdata$MailingState[RTUdata$exZIP=="NO ZIP CODE"]

RTUdata$business_name<-RTUdata$Name

RTUdata<-RTUdata%>%group_by(Site_Address_Combined2)%>%mutate(addressmatch=grepl(MailingAddress,Site_Address_Combined2,fixed=TRUE))
table(RTUdata$addressmatch)
RTUdata$business_name[RTUdata$addressmatch&RTUdata$BusinessName!=" "]<-RTUdata$BusinessName[RTUdata$addressmatch&RTUdata$BusinessName!=" "]

RTUdata$namecheck<-grepl("[,][^\\ ]",RTUdata$business_name)&!grepl("Corp|INC|LLC|L.P.",RTUdata$business_name,ignore.case = TRUE)
table(RTUdata$namecheck)

RTUdata$contact_name<-"No contact name"
RTUdata$contact_name[RTUdata$namecheck]<-paste(substr(RTUdata$business_name,regexpr("[,]",RTUdata$business_name)+1,10000),substr(RTUdata$business_name,1,regexpr("[,]",RTUdata$business_name)-1),sep=" ")[RTUdata$namecheck]
RTUdata$contact_name[RTUdata$namecheck]<-sapply(RTUdata$contact_name[RTUdata$namecheck],.simpleCap)

RTUdata$local_government<-FALSE
RTUdata$local_government[grepl("police|fire d|fire p|govern|school|district|county|city of|university|community center|PD|education|consulate",RTUdata$business_name,ignore.case = TRUE)]<-TRUE
RTUdata$local_government[grepl("park",RTUdata$business_name,ignore.case = TRUE)&grepl("rec",RTUdata$business_name,ignore.case = TRUE)]<-TRUE
RTUdata$local_government[grepl("courthouse|district court|municipal court|court house",RTUdata$business_name,ignore.case = TRUE)]<-TRUE
RTUdata$local_government[grepl("inc|LLC|corp",RTUdata$business_name,ignore.case = TRUE)]<-FALSE

MWedits1<-subset(read.csv("~/desktop/local_governmentMW1.csv",stringsAsFactors = FALSE),X==1)
MWedits2<-subset(read.csv("~/desktop/local_government2MW.csv",stringsAsFactors = FALSE),C==1&X1.if.keep.in.sample..0.if.flag.for.removal==1)
RTUdata$local_government[RTUdata$business_name%in%MWedits1$B]<-FALSE
RTUdata$local_government[RTUdata$business_name%in%MWedits2$B]<-FALSE

table(RTUdata$local_government)

RTUdata$date<-as.Date(RTUdata$ServiceConnectionDate,"%m/%e/%Y")
RTUdata$acc1516<-FALSE
RTUdata$acc1516[RTUdata$date>"2014-12-31"&RTUdata$date<"2017-01-01"]<-TRUE
RTUdata$drop_C_pre0714<-RTUdata$date<"2014-07-01"&RTUdata$channel_clean=="C"
RTUdata$drop_A_pre0714<-RTUdata$Site_Address_Combined2%in%Remove$addclean

RTUdata$validEmail<-"No valid email"
RTUdata$validEmail[RTUdata$hasemail==1]<-RTUdata$Email[RTUdata$hasemail==1]

RTUdata$strata<-paste(RTUdata$CZ,RTUdata$channel_clean,sep="")
table(RTUdata$strata)

# write.csv(RTUdata,"/volumes/Projects Berkeley/401006 - PG&E MSA and Tech Assistance CWA/PG&E RTU Recruitment/RTU Project (for desktop)/Data - Confidential/old sample/RTUFrame.csv",row.names = FALSE)

set.seed(128487)

RTUpre<-RTUdata%>%filter(!local_government&!drop_A_pre0714&!drop_C_pre0714)%>%group_by(strata)%>%mutate(rand=runif(length(strata),0,1),rank=rank(rand),select=0)
RTUpre$select[RTUpre$strata=="3A"&RTUpre$rank<=67]<-1
RTUpre$select[RTUpre$strata=="3B"&RTUpre$rank<=7]<-1
RTUpre$select[RTUpre$strata=="3C"&RTUpre$rank<=351&RTUpre$validEmail!="No valid email"]<-1
RTUpre$select[RTUpre$strata=="12A"&RTUpre$rank<=65]<-1
RTUpre$select[RTUpre$strata=="12B"&RTUpre$rank<=200]<-1
RTUpre$select[RTUpre$strata=="12C"&RTUpre$rank<=343&RTUpre$validEmail!="No valid email"]<-1

table(RTUpre$select,RTUpre$strata)
table(RTUpre$strata)

RTUpreEEID<-left_join(RTUpre,select(EEIDS,c(Site_Address_Combined2,EEID)),by="Site_Address_Combined2")

RTUout<-select(RTUpreEEID%>%filter(select==1)%>%ungroup(),c(EEID,business_name,line2,exCITY,exZIP,contact_name,validEmail,emaildupe,Channel,CZ,acc1516))
RTUout$contact_name[RTUout$business_name=="ERNEST MONKEN"]<-.simpleCap("ERNEST MONKEN")
RTUout$contact_name[RTUout$business_name=="JOE WITHERSPOON"]<-.simpleCap("JOE WITHERSPOON")
RTUout$contact_name[RTUout$business_name=="LUDOVICO CASSARA"]<-.simpleCap("LUDOVICO CASSARA")
RTUout$contact_name[RTUout$business_name=="NINA THOMPSON"]<-.simpleCap("NINA THOMPSON")
RTUout$contact_name[RTUout$business_name=="THOMAS A KLINKHAMMER"]<-.simpleCap("THOMAS A KLINKHAMMER")
RTUout$contact_name[RTUout$business_name=="THOMAS A KLINKHAMMER"]<-.simpleCap("THOMAS A KLINKHAMMER")

table(paste(RTUout$CZ,RTUout$Channel,sep=""))

# write.csv(RTUout,"/volumes/Projects Berkeley/401006 - PG&E MSA and Tech Assistance CWA/PG&E RTU Recruitment/RTU Project (for desktop)/Data - Confidential/old sample/RTUdraft0431.csv",row.names = FALSE)

### sample 2. Clear enviroment
EEIDS<-read.csv("~/desktop/EE_RTU_IDs.csv",stringsAsFactors=FALSE)
RTUin<-read.csv("/volumes/Projects Berkeley/401006 - PG&E MSA and Tech Assistance CWA/PG&E RTU Recruitment/RTU Project (for desktop)/Data - Confidential/old sample/RTUFrame.csv",stringsAsFactors = FALSE)
RTUsample1<-read.csv("/volumes/Projects Berkeley/401006 - PG&E MSA and Tech Assistance CWA/PG&E RTU Recruitment/RTU Project (for desktop)/Data - Confidential/old sample/RTUdraft0420.csv",stringsAsFactors=FALSE)
RTUdata<-left_join(RTUin,select(EEIDS,c(Site_Address_Combined2,EEID)),by="Site_Address_Combined2")

RTUdata$select<-0
RTUdata$select[RTUdata$EEID%in%RTUsample1$EEID]<-1
table(RTUdata$select)

set.seed(133987)
RTUpre<-RTUdata%>%filter(!local_government&!drop_A_pre0714&!drop_C_pre0714&select==0)%>%group_by(strata)%>%mutate(rand=runif(length(strata),0,1),rank=rank(rand))
RTUpre$select[grepl("A|B",RTUpre$strata)]<-1
RTUpre$select[RTUpre$strata=="3C"&RTUpre$rank<=345&RTUpre$validEmail!="No valid email"]<-1
RTUpre$select[RTUpre$strata=="12C"&RTUpre$rank<=379&RTUpre$validEmail!="No valid email"]<-1

table(RTUpre$select,RTUpre$strata)
table(RTUpre$strata)

RTUout<-select(RTUpre%>%filter(select==1)%>%ungroup(),c(EEID,business_name,line2,exCITY,exZIP,contact_name,validEmail,emaildupe,Channel,CZ,acc1516,strata))
RTUout$contact_name[RTUout$business_name=="LEPE,CARLA"]<-.simpleCap("CARLA LEPE")
RTUout$contact_name[RTUout$business_name=="JOHN F KLAAS"]<-.simpleCap("JOHN F KLAAS")
RTUout$contact_name[RTUout$business_name=="DALE J MENDOZA"]<-.simpleCap("DALE J MENDOZA")
RTUout$contact_name[RTUout$business_name=="MATT JANES"]<-.simpleCap("MATT JANES")
RTUout$contact_name[RTUout$business_name=="ROBERT M LUTZ"]<-.simpleCap("ROBERT M LUTZ")
RTUout$contact_name[RTUout$business_name=="SYLVIA BROWN"]<-.simpleCap("SYLVIA BROWN")
RTUout$contact_name[RTUout$business_name=="THOMAS W NEDELSKY"]<-.simpleCap("THOMAS W NEDELSKY")

table(paste(RTUout$CZ,RTUout$Channel,sep=""))

# write.csv(RTUout %>% filter(CZ==13) %>% data.frame(),"/volumes/Projects Berkeley/401006 - PG&E MSA and Tech Assistance CWA/PG&E RTU Recruitment/RTU Project (for desktop)/Data - Confidential/old sample/RTUdraft_SAMPLE_13A13B_0629.csv",row.names = FALSE)
# write.csv(RTUout,"/volumes/Projects Berkeley/401006 - PG&E MSA and Tech Assistance CWA/PG&E RTU Recruitment/RTU Project (for desktop)/Data - Confidential/old sample/RTUdraft_SAMPLE2_0504.csv",row.names = FALSE)

### Remaining sites
RTUsample2<-read.csv("/volumes/Projects Berkeley/401006 - PG&E MSA and Tech Assistance CWA/PG&E RTU Recruitment/RTU Project (for desktop)/Data - Confidential/second mailing/RTUdraft_SAMPLE2_0504.csv",stringsAsFactors = FALSE)
RTUdata$selected<-FALSE
RTUdata$selected[RTUdata$EEID%in%RTUsample1$EEID|RTUdata$EEID%in%RTUsample2$EEID]<-TRUE
table(RTUdata$selected,RTUdata$strata)

Remaining<-RTUdata %>% filter(!selected&!local_government&!drop_A_pre0714) %>% group_by(strata) %>% summarise(n=n(),email=sum(validEmail!="No valid email"),ue=n_distinct(validEmail))

# sample 3 (and 4). clear enviroment
EEIDS<-read.csv("~/desktop/EE_RTU_IDs.csv",stringsAsFactors=FALSE)
RTUin<-read.csv("/volumes/Projects Berkeley/401006 - PG&E MSA and Tech Assistance CWA/PG&E RTU Recruitment/RTU Project (for desktop)/Data - Confidential/old sample/RTUFrame.csv",stringsAsFactors = FALSE)
RTUsample1<-read.csv("/volumes/Projects Berkeley/401006 - PG&E MSA and Tech Assistance CWA/PG&E RTU Recruitment/RTU Project (for desktop)/Data - Confidential/old sample/RTUdraft0420.csv",stringsAsFactors=FALSE)
RTUsample2<-read.csv("/volumes/Projects Berkeley/401006 - PG&E MSA and Tech Assistance CWA/PG&E RTU Recruitment/RTU Project (for desktop)/Data - Confidential/second mailing/RTUdraft_SAMPLE2_0504.csv",stringsAsFactors = FALSE)
RTUdata<-left_join(RTUin,select(EEIDS,c(Site_Address_Combined2,EEID)),by="Site_Address_Combined2")

RTUdata$select<-0
RTUdata$select[RTUdata$EEID%in%RTUsample1$EEID|RTUdata$EEID%in%RTUsample2$EEID]<-1
table(RTUdata$select)

set.seed(287349)
RTUpre<-RTUdata%>%filter(!local_government&!drop_A_pre0714&!drop_C_pre0714&select==0&validEmail!="No valid email")%>%group_by(strata)%>%mutate(rand=runif(length(strata),0,1),rank=rank(rand))
RTUpre$select[RTUpre$strata=="3C"&RTUpre$rank<=1000&RTUpre$validEmail!="No valid email"]<-1
RTUpre$select[RTUpre$strata=="12C"&RTUpre$rank<=1000&RTUpre$validEmail!="No valid email"]<-1

RTUout<-select(RTUpre%>%filter(select==1) %>% mutate(rand=runif(length(strata),0,1))%>%ungroup(),c(EEID,business_name,line2,exCITY,exZIP,contact_name,validEmail,emaildupe,Channel,CZ,acc1516,strata,rand))
RTUout$batch<-1
RTUout$batch[RTUout$rand>=median(RTUout$rand)]<-2
table(RTUout$batch)

table(RTUpre$select,RTUpre$strata)
table(RTUpre$strata)

# write.csv(select(subset(RTUout,batch==1),c(-rand,-batch)),"/volumes/Projects Berkeley/401006 - PG&E MSA and Tech Assistance CWA/PG&E RTU Recruitment/RTU Project (for desktop)/Data - Confidential/old sample/RTUdraft_SAMPLE3_0530.csv",row.names = FALSE)
# write.csv(select(subset(RTUout,batch==2),c(-rand,-batch)),"/volumes/Projects Berkeley/401006 - PG&E MSA and Tech Assistance CWA/PG&E RTU Recruitment/RTU Project (for desktop)/Data - Confidential/old sample/RTUdraft_SAMPLE4_0530.csv",row.names = FALSE)

# create file with all data and sample groups. Clear enviroment.
EEIDS<-read.csv("~/desktop/EE_RTU_IDs.csv",stringsAsFactors=FALSE)
RTUin<-read.csv("/volumes/Projects Berkeley/401006 - PG&E MSA and Tech Assistance CWA/PG&E RTU Recruitment/Data - Confidential/old sample/RTUFrame.csv",stringsAsFactors = FALSE)
RTUsample1<-read.csv("/volumes/Projects Berkeley/401006 - PG&E MSA and Tech Assistance CWA/PG&E RTU Recruitment/Data - Confidential/old sample/RTUdraft0420.csv",stringsAsFactors=FALSE)
RTUsample2<-read.csv("/volumes/Projects Berkeley/401006 - PG&E MSA and Tech Assistance CWA/PG&E RTU Recruitment/Data - Confidential/2. Second Mailing/RTUdraft_SAMPLE2_0504.csv",stringsAsFactors = FALSE)
RTUsample3<-read.csv("/volumes/Projects Berkeley/401006 - PG&E MSA and Tech Assistance CWA/PG&E RTU Recruitment/Data - Confidential/old sample/RTUdraft_SAMPLE3_0530.csv",stringsAsFactors=FALSE)
RTUsample4<-read.csv("/volumes/Projects Berkeley/401006 - PG&E MSA and Tech Assistance CWA/PG&E RTU Recruitment/Data - Confidential/old sample/RTUdraft_SAMPLE4_0530.csv",stringsAsFactors = FALSE)
RTUdata<-left_join(RTUin,select(EEIDS,c(Site_Address_Combined2,EEID)),by="Site_Address_Combined2")

RTUdata$Sample<-"Not yet sampled"
RTUdata$Sample[RTUdata$EEID%in%RTUsample1$EEID]<-"Sample 1"
RTUdata$Sample[RTUdata$EEID%in%RTUsample2$EEID]<-"Sample 2"
RTUdata$Sample[RTUdata$EEID%in%RTUsample3$EEID]<-"Sample 3"
RTUdata$Sample[RTUdata$EEID%in%RTUsample4$EEID]<-"Sample 4"
table(RTUdata$Sample)

# write.csv(RTUdata,"/volumes/Projects Berkeley/401006 - PG&E MSA and Tech Assistance CWA/PG&E RTU Recruitment/RTU Project (for desktop)/Data - Confidential/old sample/RTU_Frame_and_Sample.csv",row.names = FALSE)

# Sample 5. Clear enviroment.
EEIDS<-read.csv("~/desktop/EE_RTU_IDs.csv",stringsAsFactors=FALSE)
RTUin<-read.csv("/volumes/Projects Berkeley/401006 - PG&E MSA and Tech Assistance CWA/PG&E RTU Recruitment/Data - Confidential/old sample/RTU_Frame_and_Sample.csv",stringsAsFactors = FALSE)
RTUdata<-RTUin

set.seed(948577)
RTUdata$samplable<-(RTUdata$strata=="3C"|RTUdata$strata=="12C")
RTUpre<-RTUdata%>%filter(!local_government&!drop_A_pre0714&!drop_C_pre0714&Sample=="Not yet sampled"&validEmail!="No valid email")%>%group_by(samplable)%>%mutate(rand=runif(length(strata),0,1),rank=rank(rand))
RTUpre$select<-0
RTUpre$select[RTUpre$samplable&RTUpre$rank<=1000&RTUpre$validEmail!="No valid email"]<-1

table(RTUpre$select,RTUpre$strata)
table(RTUpre$strata)

RTUout<-select(RTUpre%>%filter(select==1)%>%ungroup(),c(EEID,business_name,line2,exCITY,exZIP,contact_name,validEmail,emaildupe,Channel,CZ,acc1516,strata))

table(paste(RTUout$CZ,RTUout$Channel,sep=""))

# write.csv(RTUout,"/volumes/Projects Berkeley/401006 - PG&E MSA and Tech Assistance CWA/PG&E RTU Recruitment/Data - Confidential/old sample/RTUdraft_SAMPLE5_0719.csv",row.names = FALSE)
