# ComEd Needs Propensity
library(dplyr)
library(pROC)
library(xlsx)
library(lubridate)
options(scipen = 999)

# Load customer/usage data
# customers<-read.delim("/volumes/Projects/466002 - ComEd Needs Assessment/Confidential Data/Task 1/ComEd customer data anon/ComEd Residential Base 101618.txt",stringsAsFactors = FALSE)
# use_2016<-read.delim("/volumes/Projects/466002 - ComEd Needs Assessment/Confidential Data/Task 1/ComEd customer data anon/2016 to 2017 Usage for Accounts 101618.txt",stringsAsFactors = FALSE)
# use_2017<-read.delim("/volumes/Projects/466002 - ComEd Needs Assessment/Confidential Data/Task 1/ComEd customer data anon/2017 to 2018 Usage for Accounts 101618.txt",stringsAsFactors = FALSE)
# save(list = c("customers","use_2016","use_2017"),file="/volumes/Projects/466002 - ComEd Needs Assessment/Confidential Data/Task 1/ComEd customer data anon/customers and usage.RData")

load(file="/volumes/Projects/466002 - ComEd Needs Assessment/Confidential Data/Task 1/ComEd customer data anon/customers and usage.RData")

# tracking/census
census<-read.xlsx("/volumes/Projects/466002 - ComEd Needs Assessment/Confidential Data/Task 1/Latest matrix - estimated ie density by geography.xlsx",sheetName = "Sheet1")
PRIZM<-read.xlsx("/volumes/Projects/466002 - ComEd Needs Assessment/Confidential Data/Task 1/PRIZM Premier Master Demographic Spreadsheet External 2017 HL.xlsx",sheetName = "Essentials") %>% mutate(code_merge=as.numeric(Full_Code))

# within LI, high users - avg monthly, minimum monthly, cooling in summer, heat with elec, summer/winter sholder ratio
# Usage aggregation
use_2016$month<-month(as.Date(paste(as.character(use_2016$MM_ADJ_BILLING_YEARMO),"01"),format="%Y%m%d"))
use_2017$month<-month(as.Date(paste(as.character(use_2017$MM_ADJ_BILLING_YEARMO),"01"),format="%Y%m%d"))

use_agg<-bind_rows(use_2016,use_2017) %>%
  group_by(ID,month) %>%
  summarise(avg=mean(BILLING_USAGE_QTY,na.rm = TRUE)) %>%
  group_by(ID) %>% 
  summarise(
    M01=max(avg[month==1],-Inf),
    M02=max(avg[month==2],-Inf),
    M03=max(avg[month==3],-Inf),
    M04=max(avg[month==4],-Inf),
    M05=max(avg[month==5],-Inf),
    M06=max(avg[month==6],-Inf),
    M07=max(avg[month==7],-Inf),
    M08=max(avg[month==8],-Inf),
    M09=max(avg[month==9],-Inf),
    M10=max(avg[month==10],-Inf),
    M11=max(avg[month==11],-Inf),
    M12=max(avg[month==12],-Inf))

use_agg[use_agg==-Inf]<-NA
table(is.na(use_agg))

use_agg$min<-apply(use_agg %>% select(-ID),1,FUN=min,na.rm=TRUE)
use_agg$avg<-apply(use_agg %>% select(-ID,-min),1,FUN=mean)
use_agg$S_JJA<-apply(use_agg %>% select(M06,M07,M08),1,FUN=mean)
use_agg$W_DJF<-apply(use_agg %>% select(M12,M01,M02),1,FUN=mean)

# metrics
use_agg$sm_ratio<-use_agg$S_JJA/use_agg$min
use_agg$wm_ratio<-use_agg$W_DJF/use_agg$min
use_agg$sa_ratio<-use_agg$S_JJA/use_agg$avg
use_agg$wa_ratio<-use_agg$W_DJF/use_agg$avg
use_agg$am_ratio<-use_agg$avg/use_agg$min
use_agg$sw_ratio<-use_agg$S_JJA/use_agg$W_DJF
summary(use_agg)


table(round(use_agg$ss_ratio))

use_agg2<-use_agg %>%
  group_by(ID) %>% 
  mutate(
    min=min(c(M01,M02,M03,M04,M05,M06,M07,M08,M09,M10,M11,M12),na.rm = TRUE),
    s_JJA=mean(c(M06,M07,M08),na.rm = TRUE),
    w_DJF=mean(c(M12,M01,M02),na.rm=TRUE))




agg_2016<-use_2016 %>% 
  group_by(ID) %>% 
  summarise(
    total_use_2016=sum(BILLING_USAGE_QTY),
    summer_use_2016=sum(BILLING_USAGE_QTY[MM_ADJ_BILLING_YEARMO==201706|MM_ADJ_BILLING_YEARMO==201707|MM_ADJ_BILLING_YEARMO==201708]),
    winter_use_2016=sum(BILLING_USAGE_QTY[MM_ADJ_BILLING_YEARMO==201612|MM_ADJ_BILLING_YEARMO==201701|MM_ADJ_BILLING_YEARMO==201702]))

agg_2017<-use_2017 %>% 
  group_by(ID) %>% 
  summarise(
    total_use_2017=sum(BILLING_USAGE_QTY),
    summer_use_2017=sum(BILLING_USAGE_QTY[MM_ADJ_BILLING_YEARMO==201806|MM_ADJ_BILLING_YEARMO==201807|MM_ADJ_BILLING_YEARMO==201808]),
    winter_use_2017=sum(BILLING_USAGE_QTY[MM_ADJ_BILLING_YEARMO==201712|MM_ADJ_BILLING_YEARMO==201801|MM_ADJ_BILLING_YEARMO==201802]))

modeling_data<-left_join(customers,agg_2016,by="ID") %>% left_join(agg_2017,"ID") %>% left_join(census,"ID") %>% left_join(PRIZM,by=c("PRIZM.Code"="code_merge"))
table(is.na(modeling_data$Full_Code))

# prizm + census for likelood of LI scale of 1-9 -> surveys. 
