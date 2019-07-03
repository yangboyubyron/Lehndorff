library(readxl)
library(dplyr)
library(lubridate)

# load data (set 2)
SD_ULP_data <- read_excel("/volumes/Projects Berkeley/428010 - SDG&E ULP Verification/CONFIDENTIAL - Data from Client/Copy of 2019_PrimaryLighting_EECP_vs_RW.xlsx", 
  col_types = c("text", "numeric", "text", 
  "numeric", "text", "numeric", "date", 
  "date", "text", "date", "date", "date", 
  "text", "text", "text", "numeric", 
  "text", "text", "text", "text"))

# define big box stores
SD_ULP_data$big.box<-FALSE
SD_ULP_data$big.box[grepl("home depot",SD_ULP_data$CustomerName,ignore.case = TRUE)]<-TRUE
SD_ULP_data$big.box[grepl("costco",SD_ULP_data$CustomerName,ignore.case = TRUE)]<-TRUE
table(SD_ULP_data$CustomerName,SD_ULP_data$big.box)

# aggregate to address. identify multiple customers
ULP.agg<-SD_ULP_data %>% group_by(ServiceFullAddress) %>% 
  summarise(is.big.box=as.logical(max(big.box)),shipments=n_distinct(InstallDate),min.date=min(InstallDate),max.date=max(InstallDate),
    total.QTY=sum(MeasureQty),QTY.per.10=total.QTY/6,
    CustNames=n_distinct(CustomerName),CustName_1=unique(CustomerName)[1],CustName_2=ifelse(CustNames>1,unique(CustomerName)[2],"NA"),
    mans=n_distinct(ContractorName),first.man=first(ContractorName))

# calculate bulbs per month
table(ULP.agg$QTY.per.10>=3000,ULP.agg$is.big.box)
table(ULP.agg$QTY.per.10<3000&ULP.agg$QTY.per.10>=1000,ULP.agg$is.big.box)
table(ULP.agg$QTY.per.10<1000,ULP.agg$is.big.box)

# define tiers
ULP.agg$tier<-"Not Defined"
ULP.agg$tier[ULP.agg$QTY.per.10<1000]<-"Tier 3"
ULP.agg$tier[ULP.agg$QTY.per.10>=3000]<-"Tier 1"
ULP.agg$tier[ULP.agg$QTY.per.10<3000&ULP.agg$QTY.per.10>=1000]<-"Tier 2"
ULP.agg$tier[ULP.agg$is.big.box==1]<-"Big Box"
table(ULP.agg$tier)

# additional data (set 1)
X2019_SDG_E_ULP_YTD_5_6_2019_w_Analysis <- read_excel("~/Desktop/2019 SDG&E ULP YTD 5_6_2019 w Analysis.xlsx")

agg.1<-X2019_SDG_E_ULP_YTD_5_6_2019_w_Analysis %>% group_by(`Retailer Address`,City,ZIP) %>% summarise(total.bulbs=sum(SumOfNumUnits),mans=n_distinct(`MFG Name`),first.man=first(`MFG Name`))
zzz.1<-X2019_SDG_E_ULP_YTD_5_6_2019_w_Analysis %>% group_by(`Retailer Name`,`Retailer Address`,City,ZIP) %>% 
  summarise(max.1=max(`Tier-1: 3,000 or more`,na.rm = TRUE),max.2=max(`Tier-2: 1000 to 2,999`,na.rm = TRUE),max.3=max(`Tier-3: All else`,na.rm = TRUE),max.big=max(`Exclude Big Box`,na.rm = TRUE))
table(zzz.1$max.1)
table(zzz.1$max.2)
table(zzz.1$max.3)
table(zzz.1$max.big)

# clean data to geocode
out.1<-X2019_SDG_E_ULP_YTD_5_6_2019_w_Analysis %>% select(`Retailer Address`,City,ZIP) %>% mutate(clean.add=gsub("[[:punct:]]","",paste(`Retailer Address`,City,"CA",ZIP))) %>% distinct()
write.csv(out.1,"~/desktop/out.1.csv",row.names = FALSE)

out.2<-SD_ULP_data %>% select(ServiceFullAddress) %>% mutate(clean.add=gsub("[[:punct:]]","",ServiceFullAddress)) %>% distinct()
write.csv(out.2,"~/desktop/out.2.csv",row.names = FALSE)

# load geocode
code.1<-read.csv("/volumes/Projects Berkeley/428010 - SDG&E ULP Verification/CONFIDENTIAL - Data from Client/Dataset 1 Geocode.csv",stringsAsFactors = FALSE)
code.2<-read.csv("/volumes/Projects Berkeley/428010 - SDG&E ULP Verification/CONFIDENTIAL - Data from Client/Dataset 2 Geocode.csv",stringsAsFactors = FALSE)

# aggs with geocodes
agg.code.1<-left_join(agg.1,code.1,by=c("Retailer Address"="Retailer.Address","City","ZIP"))
agg.code.2<-left_join(ULP.agg,code.2,by="ServiceFullAddress")

# Merge geocoded data and clean addresses
merge.out<-SD_ULP_data %>% left_join(
  code.2 %>% select(-clean.add,-Country,-Source)) %>% #relevant fields from geocoding
  group_by(CustomerName,Number,Street,City,State,Zip) %>% 
  mutate(clean.address = first(ServiceFullAddress)) %>% 
  ungroup() %>% mutate(new.address=!paste(Latitude,Longitude)%in%paste(agg.code.1$Latitude,agg.code.1$Longitude)) #flag addresses that appear in previous geocoding
  
nrow(merge.out)==nrow(SD_ULP_data)

## correct explainable new addresses
accept.list<-c(
  "3701 UNIVERSITY AVE SAN DIEGO, CA 92105-1321",
  "1071 MORENA BLVD SAN DIEGO, CA 92110-3914",
  "455 C ST CHULA VISTA, CA 91910-1604",
  "2368 PASEO DE LAS AMERICAS, Unit: 104 SAN DIEGO, CA 92154-5225",
  "4129 UNIVERSITY AVE SAN DIEGO, CA 92105-1418"
)

merge.out$new.address[merge.out$clean.address%in%accept.list]<-FALSE
n_distinct(merge.out$clean.address[merge.out$new.address])

# write.csv(merge.out,"/volumes/Projects Berkeley/428010 - SDG&E ULP Verification/CONFIDENTIAL - Data from Client/Data - Clean Addresses.csv",row.names = FALSE)


table(agg.code.1$match<-paste(agg.code.1$Latitude,agg.code.1$Longitude)%in%paste(agg.code.2$Latitude,agg.code.2$Longitude))
agg.code.2$match<-paste(agg.code.2$Latitude,agg.code.2$Longitude)%in%paste(agg.code.1$Latitude,agg.code.1$Longitude)

table(paste(agg.code.1$Latitude,agg.code.1$Longitude)%in%paste(agg.code.2$Latitude,agg.code.2$Longitude))
table(paste(agg.code.2$Latitude,agg.code.2$Longitude)%in%paste(agg.code.1$Latitude,agg.code.1$Longitude))
  