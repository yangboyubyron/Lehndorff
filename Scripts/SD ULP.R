library(readxl)
library(dplyr)
library(lubridate)

# load data
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
    total.QTY=sum(MeasureQty),QTY.per.10=total.QTY/10,
    CustNames=n_distinct(CustomerName),CustName_1=unique(CustomerName)[1],CustName_2=ifelse(CustNames>1,unique(CustomerName)[2],"NA"))

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

#save out
# write.csv(ULP.agg,file = "/volumes/Projects Berkeley/428010 - SDG&E ULP Verification/CONFIDENTIAL - Data from Client/PrimaryLighting_Aggregated.csv",row.names = FALSE)

# agg2 monthly bulbs
# agg2<-SD_ULP_data %>% group_by(ServiceFullAddress) %>% mutate(dates=n_distinct(InstallDate),max.date=max(InstallDate)) %>% 
#   filter(InstallDate<max.date) %>% 
#   group_by(ServiceFullAddress) %>% 
#   summarise(big.box=max(big.box),total.bulbs=sum(MeasureQty),start=min(SD_ULP_data$InstallDate),max=max(SD_ULP_data$InstallDate),months=time_length(max-start,"months"),per.month=total.bulbs/12,
#     CustNames=n_distinct(CustomerName),CustName_1=unique(CustomerName)[1],CustName_2=ifelse(CustNames>1,unique(CustomerName)[2],"NA"))
# table(between(agg2$per.month,1000,3000))
# table(agg2$per.month>3000,agg2$big.box)
# table(agg2$per.month<1000)
