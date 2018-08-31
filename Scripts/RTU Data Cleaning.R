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

# Samples 6,7,8. Clear enviroment.
RTUin<-read.csv("/volumes/Projects Berkeley/401006 - PG&E MSA and Tech Assistance CWA/PG&E RTU Recruitment/Data - Confidential/old sample/RTU_Frame_and_Sample.csv",stringsAsFactors = FALSE)
Sample5<-read.csv("/volumes/Projects Berkeley/401006 - PG&E MSA and Tech Assistance CWA/PG&E RTU Recruitment/Data - Confidential/4. Email/RTUdraft_SAMPLE5_0719.csv",stringsAsFactors = FALSE)
RTUdata<-RTUin

set.seed(279348)
RTUdata$samplable<-(RTUdata$strata=="3C"|RTUdata$strata=="12C")
RTUpre<-RTUdata%>%filter(!local_government&!drop_A_pre0714&!drop_C_pre0714&Sample=="Not yet sampled"&validEmail!="No valid email"&!EEID%in%Sample5$EEID)%>%group_by(samplable)%>%mutate(rand=runif(length(strata),0,1),rank=rank(rand))

RTUpre$select<-0
RTUpre$select[RTUpre$samplable&RTUpre$rank<=2000&RTUpre$validEmail!="No valid email"]<-1
RTUpre$select[RTUpre$samplable&RTUpre$rank<=3000&RTUpre$rank>2000&RTUpre$validEmail!="No valid email"]<-2
RTUpre$select[RTUpre$samplable&RTUpre$rank<=4000&RTUpre$rank>3000&RTUpre$validEmail!="No valid email"]<-3

table(RTUpre$select,RTUpre$strata)
table(RTUpre$strata)

RTUout1<-select(RTUpre%>%filter(select==1)%>%ungroup(),c(EEID,business_name,line2,exCITY,exZIP,contact_name,validEmail,emaildupe,Channel,CZ,acc1516,strata))
RTUout2<-select(RTUpre%>%filter(select==2)%>%ungroup(),c(EEID,business_name,line2,exCITY,exZIP,contact_name,validEmail,emaildupe,Channel,CZ,acc1516,strata))
RTUout3<-select(RTUpre%>%filter(select==3)%>%ungroup(),c(EEID,business_name,line2,exCITY,exZIP,contact_name,validEmail,emaildupe,Channel,CZ,acc1516,strata))

table(paste(RTUout1$CZ,RTUout1$Channel,sep=""))
table(paste(RTUout2$CZ,RTUout2$Channel,sep=""))
table(paste(RTUout3$CZ,RTUout3$Channel,sep=""))

# write.csv(RTUout1,"/volumes/Projects Berkeley/401006 - PG&E MSA and Tech Assistance CWA/PG&E RTU Recruitment/Data - Confidential/old sample/RTUdraft_SAMPLE6_0801.csv",row.names = FALSE)
# write.csv(RTUout2,"/volumes/Projects Berkeley/401006 - PG&E MSA and Tech Assistance CWA/PG&E RTU Recruitment/Data - Confidential/old sample/RTUdraft_SAMPLE7_0801.csv",row.names = FALSE)
# write.csv(RTUout3,"/volumes/Projects Berkeley/401006 - PG&E MSA and Tech Assistance CWA/PG&E RTU Recruitment/Data - Confidential/old sample/RTUdraft_SAMPLE8_0801.csv",row.names = FALSE)

# 2018 Data

New2018<-read.csv("/volumes/Projects Berkeley/401006 - PG&E MSA and Tech Assistance CWA/PG&E RTU Recruitment/Data - Confidential/082818 data/SMB Customer List 02062018.csv",stringsAsFactors=FALSE)
New2018$fullad<-paste(New2018$Site.Address,paste(New2018$City, New2018$Zip.Code,sep=" "),sep = ", ")

test<-subset(New2018,tolower(fullad)%in%tolower(RTUin$Site_Address_Combined2))
test2<-subset(RTUin,tolower(Site_Address_Combined2)%in%tolower(New2018$fullad))
test3<-subset(New2018,!tolower(fullad)%in%tolower(RTUin$Site_Address_Combined2)) %>% filter(Site.Address!="") %>% group_by(fullad) %>% mutate(double=substr(fullad,1,regexpr("[a-zA-z][[:space:]]",fullad)[1]))

RTUin$double<-substr(RTUin$Site_Address_Combined2,1,regexpr("[a-zA-z][[:space:]]",RTUin$Site_Address_Combined2)[1])

RTUin<-RTUin %>% group_by(Site_Address_Combined2) %>% mutate(double=substr(Site_Address_Combined2,1,regexpr("[a-zA-z][[:space:]]",Site_Address_Combined2)[1]))

test4<-subset(test3,tolower(double)%in%tolower(RTUin$double))

RTUin$cleanest<-tolower(gsub("[[:punct:]]*","",RTUin$Site_Address_Combined2))
RTUin$cleanest<-gsub(" north "," n ",RTUin$cleanest)
RTUin$cleanest<-gsub(" south "," s ",RTUin$cleanest)
RTUin$cleanest<-gsub(" east "," e ",RTUin$cleanest)
RTUin$cleanest<-gsub(" west "," w ",RTUin$cleanest)
RTUin$cleanest<-gsub(" northwest "," nw ",RTUin$cleanest)
RTUin$cleanest<-gsub(" northeast "," ne ",RTUin$cleanest)
RTUin$cleanest<-gsub(" southwest "," sw ",RTUin$cleanest)
RTUin$cleanest<-gsub(" road "," rd ",RTUin$cleanest)
RTUin$cleanest<-gsub(" drive "," dr ",RTUin$cleanest)
RTUin$cleanest<-gsub(" avenue "," ave ",RTUin$cleanest)
RTUin$cleanest<-gsub(" parkway "," pkwy ",RTUin$cleanest)
RTUin$cleanest<-gsub(" boulevard "," blvd ",RTUin$cleanest)
RTUin$cleanest<-gsub(" street "," st ",RTUin$cleanest)
RTUin$cleanest<-gsub(" place "," pl ",RTUin$cleanest)
RTUin$cleanest<-gsub(" court "," ct ",RTUin$cleanest)
RTUin$cleanest<-gsub(" way "," wy ",RTUin$cleanest)
RTUin$cleanest<-gsub(" highway "," hwy ",RTUin$cleanest)
RTUin$cleanest<-gsub(" suite "," ",RTUin$cleanest)
RTUin$cleanest<-gsub(" ste "," ",RTUin$cleanest)
RTUin$cleanest<-gsub(" hse "," ",RTUin$cleanest)
RTUin$cleanest<-gsub(" #"," ",RTUin$cleanest,fixed = TRUE)
RTUin$cleanest<-gsub(" sp "," ",RTUin$cleanest)

New2018$cleanest<-tolower(gsub("[[:punct:]]*","",New2018$fullad))
New2018$cleanest<-gsub(" north "," n ",New2018$cleanest)
New2018$cleanest<-gsub(" south "," s ",New2018$cleanest)
New2018$cleanest<-gsub(" east "," e ",New2018$cleanest)
New2018$cleanest<-gsub(" west "," w ",New2018$cleanest)
New2018$cleanest<-gsub(" northwest "," nw ",New2018$cleanest)
New2018$cleanest<-gsub(" northeast "," ne ",New2018$cleanest)
New2018$cleanest<-gsub(" southwest "," sw ",New2018$cleanest)
New2018$cleanest<-gsub(" road "," rd ",New2018$cleanest)
New2018$cleanest<-gsub(" drive "," dr ",New2018$cleanest)
New2018$cleanest<-gsub(" avenue "," ave ",New2018$cleanest)
New2018$cleanest<-gsub(" parkway "," pkwy ",New2018$cleanest)
New2018$cleanest<-gsub(" boulevard "," blvd ",New2018$cleanest)
New2018$cleanest<-gsub(" street "," st ",New2018$cleanest)
New2018$cleanest<-gsub(" place "," pl ",New2018$cleanest)
New2018$cleanest<-gsub(" court "," ct ",New2018$cleanest)
New2018$cleanest<-gsub(" way "," wy ",New2018$cleanest)
New2018$cleanest<-gsub(" highway "," hwy ",New2018$cleanest)
New2018$cleanest<-gsub(" suite "," ",New2018$cleanest)
New2018$cleanest<-gsub(" ste "," ",New2018$cleanest)
New2018$cleanest<-gsub(" hse "," ",New2018$cleanest)
New2018$cleanest<-gsub(" #"," ",New2018$cleanest,fixed = TRUE)
New2018$cleanest<-gsub(" sp "," ",New2018$cleanest)

table(subset(New2018,Site.Address!="")$cleanest%in%RTUin$cleanest)

drops<-subset(New2018,Site.Address!=""&cleanest%in%RTUin$cleanest)

otherdupe<-c("1 SOUTHLAND MALL DR, Hayward 94545",
"10734 TRINITY PKWY SP C, Stockton 95219",
"2400 CENTRAL PARKWAY, DUBLIN 94568",
"1041 Helen Power Drive, Vacaville 95687",
"141 General Stilwell Dr Suite B, Marina 93933",
"1508 Tollhouse Road, Clovis 93611",
"1700 Market Street, OAKLAND 94607",
"2090 Diamond Blvd Ste 20, Concord 94520",
"2121 2nd Street, Davis 95616",
"2701 Ming Ave., Bakersfield 93304",
"280 Hegenberger Road, OAKLAND 94621",
"3105 Highland Ave, Selma 93662",
"312 Railroad Ave, Danville 94526",
"3755 Atherton Rd, Rocklin 95765",
"4751 Pacific Avenue, Stockton 95207",
"520 Chadbourne Road, Fairfield 94534",
"555 Selby St., San Francisco 94124",
"5620 California Ave, Bakersfield 93309",
"6001 Pioneer Dr., Bakersfield 93306",
"7448 Fox Rd., Hughson 95326",
"9000 Ming Avenue, Bakersfield 93311")

table(otherdupe%in%drops$fullad)

New2018_dedupe<-New2018 %>% filter(!fullad%in%otherdupe&!fullad%in%test$fullad,!cleanest%in%drops$cleanest) %>% filter(Site.Address!="")

# write.csv(New2018_dedupe %>% select(-SAID,-cleanest) %>% ungroup(),"/Volumes/Projects Berkeley/401006 - PG&E MSA and Tech Assistance CWA/PG&E RTU Recruitment/Data - Confidential/082818 data/Deduped_2018.csv",row.names = FALSE)




