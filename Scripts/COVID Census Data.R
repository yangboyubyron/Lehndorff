library(tigris)
OR<-tracts("OR")@data
WA<-tracts("WA")@data
ID<-tracts("ID")@data
MT<-tracts("MT")@data
CA<-tracts("CA")@data

tract_data<-bind_rows(OR,WA,ID,MT,CA)

acs_pull <- function(state = 17) {
  require(jsonlite)
  require(dplyr)
  vars<-c("NAME","S0101_C01_001E","S1901_C01_012E",
    "S1101_C01_001E","S1101_C01_002E","S1101_C01_003E","S1101_C01_004E","S1101_C01_010E","S1101_C01_011E","S1101_C01_012E","S1101_C01_013E",
    "S1101_C01_019E","S1101_C01_020E","S1602_C01_001E","S1602_C01_002E",
    "S1602_C03_001E","S2201_C01_023E","S2201_C01_024E","S1401_C01_010E")

  query=paste0("https://api.census.gov/data/2017/acs/acs5/subject?get=",paste(vars,collapse = ","),"&for=tract:*&in=state:",toString(state), "&key=623020359418e43f907eddc1c27bbf7b9814d102")

  data <- data.frame(fromJSON(query))[-1,]
  
  colnames(data) <- c(
    "Name","total_population","median_inc",
    "households","avg_size","families","family_size","hhs_with_children","hhs_with_seniors","live_alone","senior_alone",
    "owner_occ","renter_occ","all_households","spanish_speaking",
    "limited_eng_hhs","hhs_with_disab","hhs_no_disab","pop_in_college",
    "state", "county", "tract")
  data[,2:(length(colnames(data))-3)] <- as.numeric(sapply(data[,2:(length(colnames(data))-3)],as.vector))
  data <- data %>%
    group_by(Name) %>%
    mutate(CENSUS_TRACT_CODE = paste(state,county,tract,sep = "")) %>% 
    ungroup()
  data
}

WA_census<-acs_pull(state = "53")
OR_census<-acs_pull(state = "41")
ID_census<-acs_pull(state = "16")
MT_census<-acs_pull(state = "30")
CA_census<-acs_pull(state = "06")
census_pull<-bind_rows(WA_census,OR_census,ID_census,MT_census,CA_census)

census_pull[census_pull==-666666666.0]<-NA

tract_density<-
  census_pull %>% 
  left_join(
    tract_data %>% 
      select(GEOID,ALAND) %>% 
      mutate(sq_miles=as.numeric(ALAND)/2.59e+6),
    by=c("CENSUS_TRACT_CODE"="GEOID")) %>% 
  mutate(density=total_population/sq_miles)
nrow(tract_density)==nrow(census_pull)

write.csv(tract_density,"~/desktop/COVID_Census_data.csv",row.names = FALSE)
