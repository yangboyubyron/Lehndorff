library(readxl)
library(dplyr)
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
  summarise(shipments=n_distinct(InstallDate),total.sold=sum(MeasureQty),is.big.box=max(big.box),
    CustNames=n_distinct(CustomerName),CustName_1=unique(CustomerName)[1],CustName_2=ifelse(CustNames>1,unique(CustomerName)[2],"NA"))

# calculate bulbs per month

# define tiers
ULP.agg$tier<-"Tier 3"
ULP.agg$tier[ULP.agg$total.sold>=3000]<-"Tier 1"
ULP.agg$tier[ULP.agg$total.sold<3000&ULP.agg$total.sold>=1000]<-"Tier 2"
ULP.agg$tier[ULP.agg$is.big.box==1]<-"Big Box"
table(ULP.agg$tier)
