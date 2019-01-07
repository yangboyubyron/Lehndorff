# ComEd Needs Propensity
library(dplyr)
library(pROC)
library(xlsx)
options(scipen = 999)

# Load customer/usage data
# customers<-read.delim("/volumes/Projects/466002 - ComEd Needs Assessment/Confidential Data/Task 1/ComEd customer data anon/ComEd Residential Base 101618.txt",stringsAsFactors = FALSE)
# use_2016<-read.delim("/volumes/Projects/466002 - ComEd Needs Assessment/Confidential Data/Task 1/ComEd customer data anon/2016 to 2017 Usage for Accounts 101618.txt",stringsAsFactors = FALSE)
# use_2017<-read.delim("/volumes/Projects/466002 - ComEd Needs Assessment/Confidential Data/Task 1/ComEd customer data anon/2017 to 2018 Usage for Accounts 101618.txt",stringsAsFactors = FALSE)
# save(list = c("customers","use_2016","use_2017"),file="/volumes/Projects/466002 - ComEd Needs Assessment/Confidential Data/Task 1/ComEd customer data anon/customers and usage.RData")

load(file="/volumes/Projects/466002 - ComEd Needs Assessment/Confidential Data/Task 1/ComEd customer data anon/customers and usage.RData")

# tracking/census
census<-read.xlsx("/volumes/Projects/466002 - ComEd Needs Assessment/Confidential Data/Task 1/Latest matrix - estimated ie density by geography.xlsx",sheetName = "Sheet1")
PRIZM<-read.xlsx("/volumes/Projects/466002 - ComEd Needs Assessment/Confidential Data/Task 1/PRIZM Premier Master Demographic Spreadsheet External 2017.xlsx",sheetName = "Essentials",startRow=4) %>% mutate(code_merge=as.numeric(Code.2))
