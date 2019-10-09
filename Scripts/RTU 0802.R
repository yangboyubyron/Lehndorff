# RTU 0802
library(readxl)
permits_novato <- read_excel("/volumes/Projects Berkeley/401006 - PG&E MSA and Tech Assistance CWA/PG&E RTU Recruitment/Data - Confidential/2018_07_12_permit records.xlsx", 
    col_types = c("text", "text", "text", 
        "text", "text", "text", "text", "text", 
        "text", "text", "text", "date", "text", 
        "text", "text"))

permits_sanraf <- read_excel("/volumes/Projects Berkeley/401006 - PG&E MSA and Tech Assistance CWA/PG&E RTU Recruitment/Data - Confidential/2018_07_12_permit records.xlsx", 
    sheet = "2018_07_San Rafael")

RTUin<-read.csv("/volumes/Projects Berkeley/401006 - PG&E MSA and Tech Assistance CWA/PG&E RTU Recruitment/Data - Confidential/old sample/RTU_Frame_and_Sample.csv",stringsAsFactors = FALSE)

ES_data<-read.csv("/volumes/Projects Berkeley/401006 - PG&E MSA and Tech Assistance CWA/PG&E RTU Recruitment/Data - Confidential/EnergySolutions Data_032019/Selected Program Data for PG&E C&S project.csv")
