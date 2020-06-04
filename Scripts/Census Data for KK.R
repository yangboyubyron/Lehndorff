# Accessing Census Data
library(dplyr)

#### Setup to customize the data access function ####

census_variables<-read.csv("/volumes/Projects/444003 - Avista LINA Phase I/Data - CONFIDENTIAL/census_variables.csv",stringsAsFactors = FALSE)
useful_vars<-census_variables %>% filter(!grepl("PUERTO RICO",Concept)&!grepl("Percent",Label)&!grepl("YEARS AND OVER IN THE",Concept))
hh_vars<-useful_vars %>% filter(Concept=="HOUSEHOLDS AND FAMILIES")
hh_inc_vars<-useful_vars %>% filter(grepl("household",Label,ignore.case = TRUE)&grepl("income",Label,ignore.case = TRUE))

# Household income by source: S1902_C01_001E:S1902_C01_011E,S1902_C03_001E:S1902_C03_011E,S1901_C01_012E:S1901_C01_013E
inc.source.var<-c(
  paste0("S1902_C01_00",1:9,"E"),paste0("S1902_C01_0",10:11,"E"),paste0("S1902_C03_00",1:9,"E"),paste0("S1902_C03_0",10:11,"E"),"S1901_C01_012E","S1901_C01_013E"
)
# numb of hhs by race of householder: S1903_C01_001E:S1903_C01_010E
race.var<-c(paste0("S1903_C01_00",1:9,"E"),"S1903_C01_010E")

# " age of householder: S1903_C01_011E:S1903_C01_014E
age.var<-paste0("S1903_C01_0",11:14,"E")

# median inc race, age: S1903_C03_001E:S1903_C03_014E
med.inc.race.var<-c(paste0("S1903_C03_00",1:9,"E"),paste0("S1903_C03_0",10:14,"E"))

# median inc by family/household structure: S1903_C03_016E:S1903_C03_040E
med.inc.fam.var<-paste0("S1903_C03_0",16:40,"E")

# hhs by income bucket: S2503_C01_002E:S2503_C01_013E
inc.bucket.var<-c(paste0("S2503_C01_00",2:9,"E"),paste0("S2503_C01_0",10:13,"E"))

# hhs avg size, avg family size: S1101_C01_001E:S1101_C01_004E
hhs.size.var<-paste0("S1101_C01_00",1:4,"E")

# hhs structure (by presence): S1101_C01_010E:S1101_C01_015E
struct.var<-paste0("S1101_C01_0",10:15,"E")

# hhs by building type,tenure: S1101_C01_016E:S1101_C01_020E
build.var<-paste0("S1101_C01_0",16:20,"E")

# hhs by language: S1602_C01_001E:S1602_C01_005E,S1602_C03_001E
lang.var<-c(paste0("S1602_C01_00",1:5,"E"),"S1602_C03_001E")

# disability: S2201_C01_023E,S2201_C01_024E
disab.var<-c("S2201_C01_023E","S2201_C01_024E")

# census vars
census.vars<-bind_rows(
  census_variables %>% filter(Name%in%inc.source.var) %>% select(Name,Label,Concept) %>% mutate(type="Income by Source"),
  census_variables %>% filter(Name%in%race.var) %>% select(Name,Label,Concept) %>% mutate(type="Householder Race"),
  census_variables %>% filter(Name%in%age.var) %>% select(Name,Label,Concept) %>% mutate(type="Householder Age"),
  census_variables %>% filter(Name%in%med.inc.race.var) %>% select(Name,Label,Concept) %>% mutate(type="Median Income by Race"),
  census_variables %>% filter(Name%in%med.inc.fam.var) %>% select(Name,Label,Concept) %>% mutate(type="Median Income by Family Type"),
  census_variables %>% filter(Name%in%inc.bucket.var) %>% select(Name,Label,Concept) %>% mutate(type="Income by Bucket"),
  census_variables %>% filter(Name%in%hhs.size.var) %>% select(Name,Label,Concept) %>% mutate(type="Household Size"),
  census_variables %>% filter(Name%in%struct.var) %>% select(Name,Label,Concept) %>% mutate(type="Family Structure"),
  census_variables %>% filter(Name%in%build.var) %>% select(Name,Label,Concept) %>% mutate(type="Building Type"),
  census_variables %>% filter(Name%in%lang.var) %>% select(Name,Label,Concept) %>% mutate(type="Language"),
  census_variables %>% filter(Name%in%disab.var) %>% select(Name,Label,Concept) %>% mutate(type="Disability")
)

#### data access function and output####
acs_pull <- function(state = 17) {
  require(jsonlite)
  require(dplyr)
  vars<-c("NAME","S0101_C01_001E","S1901_C01_012E",
    "S1902_C01_007E","S2201_C03_001E","S1902_C01_009E",
    "S2503_C01_002E","S2503_C01_003E","S2503_C01_004E","S2503_C01_005E","S2503_C01_006E","S2503_C01_007E","S2503_C01_008E","S2503_C01_009E","S2503_C01_010E","S2503_C01_011E","S2503_C01_012E",
    "S1101_C01_001E","S1101_C01_002E","S1101_C01_003E","S1101_C01_004E","S1101_C01_010E","S1101_C01_011E","S1101_C01_012E","S1101_C01_013E",
    "S1101_C01_019E","S1101_C01_020E","S1602_C01_001E","S1602_C01_002E",
    "S1602_C03_001E","S2201_C01_023E","S2201_C01_024E","S1401_C01_010E",
    "S2502_C01_001E","S2502_C01_004E")

  query=paste0("https://api.census.gov/data/2017/acs/acs5/subject?get=",paste(vars,collapse = ","),"&for=tract:*&in=state:",toString(state), "&key=623020359418e43f907eddc1c27bbf7b9814d102")

  data <- data.frame(fromJSON(query))[-1,]
  
  colnames(data) <- c(
    "Name","total_population","median_inc",
    "SSI","SNAP","SNAP_cash",
    "inc_ls5k","inc_5k10k","inc_10k15k","inc_15k20k","inc_20k25k","inc_25k35k","inc_35k50k","inc_50k75k","inc_75k100k","inc_100k150k","inc_g150k",
    "households","avg_size","families","family_size","hhs_with_children","hhs_with_seniors","live_alone","senior_alone",
    "owner_occ","renter_occ","all_households","spanish_speaking",
    "limited_eng_hhs","hhs_with_disab","hhs_no_disab","pop_in_college",
    "occ_hhs","Native_hhs",
    "state", "county", "tract")
  data[,2:(length(colnames(data))-3)] <- as.numeric(sapply(data[,2:(length(colnames(data))-3)],as.vector))
  data <- data %>%
    group_by(Name) %>%
    mutate(CENSUS_TRACT_CODE = paste(state,county,tract,sep = "")) %>% 
    ungroup()
  data
}

census_pull.save<-acs_pull(state = 53)

census_pull.save[census_pull.save==-666666666.0]<-NA
