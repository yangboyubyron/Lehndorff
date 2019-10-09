# Ingo CA LI EB
# https://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?pid=ACS_pums_csv_2013_2017&prodType=document

library(dplyr);library(readr)

hca<-read_csv("/volumes/Projects/444003 - Avista LINA Phase I/Data - CONFIDENTIAL/ACS Data/csv_hca/psam_h06.csv", 
    col_types = cols(SERIALNO = col_character()))

# important columns and definitions
select.cols<-c(
  "SERIALNO",#Housing unit/GQ person serial number 2013000000001..2017999999999 .Unique identifier
  "PUMA",#public use microdata area code (PUMA) based on 2010 Census definition (areas with population of 100,000 or more, use with ST for unique code)
  "WGTP",#Housing Unit Weight
      # 00000 .Group Quarter placeholder record
      # 00001..09999 .Integer weight of housing unit
  "ADJHSG",#Adjustment factor for housing dollar amounts (6 implied decimal places) 
      # 1054015 .2013 factor
      # 1036463 .2014 factor
      # 1034680 .2015 factor
      # 1021505 .2016 factor
      # 1000000 .2017 factor
  "ADJINC",#Adjustment factor for income and earnings dollar amounts (6 implied decimal places)
      # 1061971 .2013 factor (1.007549 * 1.05401460)
      # 1045195 .2014 factor (1.008425 * 1.03646282)
      # 1035988 .2015 factor (1.001264 * 1.03468042)
      # 1029257 .2016 factor (1.007588 * 1.02150538)
      # 1011189 .2017 factor (1.011189 * 1.00000000)
  "NP",#Number of persons associated with this housing record 
      # 00 .Vacant unit
      # 01 .One person record (one person in household or any person .in group quarters)
      # 02..20 .Number of person records (number of persons in household)
  "TYPE",#Type of unit
      # 1 .Housing unit
      # 2 .Institutional group quarters
      # 3 .Noninstitutional group quarters
  "BLD",#Units in structure
      # bb .N/A (GQ)
      # 01 .Mobile home or trailer
      # 02 .One-family house detached
      # 03 .One-family house attached
      # 04 .2 Apartments
      # 05 .3-4 Apartments
      # 06 .5-9 Apartments
      # 07 .10-19 Apartments
      # 08 .20-49 Apartments
      # 09 .50 or more apartments
      # 10 .Boat, RV, van, etc.
  "ELEP",#Electricity (monthly cost, use ADJHSG to adjust values 3 and over to constant dollars)
      # bbb      .N/A (GQ/vacant)
      # 001      .Included in rent or in condo fee
      # 002      .No charge or electricity not used
      # 003..999 .$3 to $999 (Rounded and top-coded)
  "FULP",#Fuel cost (yearly cost for fuels other than gas and electricity, use ADJHSG to adjust values 3 and over to constant dollars)
      # bbbb .N/A (GQ/vacant)
      # 0001 .Included in rent or in condo fee 
      # 0002 .No charge or these fuels not used 
      # 0003..9999 .$3 to $9999 (Rounded and top-coded)
  "GASP",#Gas (monthly cost, use ADJHSG to adjust GASP values 4 and over to constant dollars)
      # bbb      .N/A (GQ/vacant)
      # 001      .Included in rent or in condo fee
      # 002      .Included in electricity payment
      # 003      .No charge or gas not used
      # 004..999 .$4 to $999 (Rounded and top-coded)
  "HFL",#House heating fuel
      # b .N/A (GQ/vacant)
      # 1 .Utility gas
      # 2 .Bottled, tank, or LP gas
      # 3 .Electricity
      # 4 .Fuel oil, kerosene, etc.
      # 5 .Coal or coke
      # 6 .Wood
      # 7 .Solar energy
      # 8 .Other fuel
      # 9 .No fuel used
  "TEN",#Tenure
      # b .N/A (GQ/vacant)
      # 1 .Owned with mortgage or loan (include home equity loans) 2 .Owned free and clear
      # 3 .Rented
      # 4 .Occupied without payment of rent
  "YBL",#When structure first built
      # bb .N/A (GQ)
      # 01 .1939 or earlier
      # 02 .1940 to 1949
      # 03 .1950 to 1959
      # 04 .1960 to 1969
      # 05 .1970 to 1979
      # 06 .1980 to 1989
      # 07 .1990 to 1999
      # 08 .2000 to 2004
      # 09 .2005
      # 10 .2006
      # 11 .2007
      # 12 .2008
      # 13 .2009
      # 14 .2010
      # 15 .2011
      # 16 .2012
      # 17 .2013
      # 18 .2014
      # 19 .2015
      # 20 .2016
      # 21 .2017
  "HINCP"#Household income (past 12 months, use ADJINC to adjust HINCP to constant dollars)R^%
      # bbbbbbbb N/A
      # 00000000 No Family Income
      # -0059999 Loss > $59,999
      # -0059998..-0000001 .Loss of $1 to $59,998
      # 00000001 .$1 or Break even
      # 00000002..99999999 .Total family income in dollars (Components .are rounded)
)

# Confirm useful columns are in data
table(select.cols%in%colnames(hca))
select.cols[select.cols%in%colnames(hca)]

# pull useful columns to make smaller datasets
hca.select<-hca %>% select(one_of(select.cols))

# PUMS recode function based on column definitons
pums_recode<-function(field,value){
  output<-"ERROR OUTPUT NOT DEFINED"
  if(field=="WGTP"){
    output<-as.numeric(value)
  }
  if(field=="ADJHSG"){
    output<-value/1000000
  }
  if(field=="ADJINC"){#Adjustment factor for income and earnings dollar amounts (6 implied decimal places)
    output<-value/1000000
  }
  if(field=="NP"){#Number of persons associated with this housing record
    output<-as.numeric(value)
  }
  if(field=="TYPE"){#Type of unit
    if(value==1){output<-"Housing unit"}
    if(value==2){output<-"Institutional group quarters"}
    if(value==3){output<-"Noninstitutional group quarters"}
  }
  if(field=="BLD"){#Units in structure
    if(is.na(value)|value=="bb"){output<-NA}else{
      if(value=="01"){output<-"Mobile home or trailer"}
      if(value=="02"){output<-"One-family house detached"}
      if(value=="03"){output<-"One-family house attached"}
      if(value=="04"){output<-"2 Apartments"}
      if(value=="05"){output<-"3-4 Apartments"}
      if(value=="06"){output<-"5-9 Apartments"}
      if(value=="07"){output<-"10-19 Apartments"}
      if(value=="08"){output<-"20-49 Apartments"}
      if(value=="09"){output<-"50 or more apartments"}
      if(value=="10"){output<-"Boat, RV, van, etc."}
    }
  }
  if(field=="ELEP"){#Electricity (monthly cost, use ADJHSG to adjust values 3 and over to constant dollars)
    if(is.na(value)|value=="001"|value=="002"){
      output<-NA
      }else{
      output<-as.numeric(value)
    }
  }
  if(field=="FS"){#Yearly food stamp/Supplemental Nutrition Assistance Program (SNAP) recipiency
    if(is.na(value)|value=="b"){output<-NA}else{
      if(value==1){output<-"Yes"}
      if(value==2){output<-"No"}
    }
  }
  if(field=="FULP"){#Fuel cost (yearly cost for fuels other than gas and electricity, use ADJHSG to adjust values 3 and over to constant dollars)
    if(is.na(value)|value=="0001"|value=="0002"){
      output<-NA
      }else{
      output<-as.numeric(value)
    }
  }
  if(field=="GASP"){#Gas (monthly cost, use ADJHSG to adjust GASP values 4 and over to constant dollars)
    if(is.na(value)|value=="001"|value=="002"|value=="003"){
      output<-NA
      }else{
      output<-as.numeric(value)
    }
  }
  if(field=="HFL"){#House heating fuel
    if(is.na(value)|value=="b"){output<-NA}else{
      if(value==1){output<-"Utility gas"}
      if(value==2){output<-"Bottled, tank, or LP gas"}
      if(value==3){output<-"Electricity"}
      if(value==4){output<-"Fuel oil, kerosene, etc."}
      if(value==5){output<-"Coal or coke"}
      if(value==6){output<-"Wood"}
      if(value==7){output<-"Solar energy"}
      if(value==8){output<-"Other fuel"}
      if(value==9){output<-"No fuel used"}
    }
  }
  if(field=="TEN"){#Tenure
    if(is.na(value)|value=="b"){output<-NA}else{
      if(value==1){output<-"Owned with mortgage or loan"}
      if(value==2){output<-"Owned free and clear"}
      if(value==3){output<-"Rented"}
      if(value==4){output<-"Occupied without payment of rent"}
    }
  }
  if(field=="YBL"){#When structure first built
    if(is.na(value)|value=="bb"){output<-NA}else{
      if(value=="01"){output<-"1939 or earlier"}
      if(value=="02"){output<-"1940 to 1949"}
      if(value=="03"){output<-"1950 to 1959"}
      if(value=="04"){output<-"1960 to 1969"}
      if(value=="05"){output<-"1970 to 1979"}
      if(value=="06"){output<-"1980 to 1989"}
      if(value=="07"){output<-"1990 to 1999"}
      if(value=="08"){output<-"2000 to 2004"}
      if(value=="09"){output<-"2005"}
      if(value=="10"){output<-"2006"}
      if(value=="11"){output<-"2007"}
      if(value=="12"){output<-"2008"}
      if(value=="13"){output<-"2009"}
      if(value=="14"){output<-"2010"}
      if(value=="15"){output<-"2011"}
      if(value=="16"){output<-"2012"}
      if(value=="17"){output<-"2013"}
      if(value=="18"){output<-"2014"}
      if(value=="19"){output<-"2015"}
      if(value=="20"){output<-"2016"}
      if(value=="21"){output<-"2017"}
    }
  }
  if(field=="FES"){#Family type and employment status
    if(is.na(value)|value=="b"){output<-NA}else{
      if(value==1){output<-"Married-couple family: Husband and wife in LF"}
      if(value==2){output<-"Married-couple family: Husband in labor force, wife not in LF"}
      if(value==3){output<-"Married-couple family: Husband not in LF, wife in LF"}
      if(value==4){output<-"Married-couple family: Neither husband nor wife in LF"}
      if(value==5){output<-"Other family: Male householder, no wife present, in LF"}
      if(value==6){output<-"Other family: Male householder, no wife present, not in LF"}
      if(value==7){output<-"Other family: Female householder, no husband present, in LF "}
      if(value==8){output<-"Other family: Female householder, no husband present, not in .LF"}
    }
  }
  if(field=="FINCP"){#Family income (past 12 months, use ADJINC to adjust FINCP to constant dollars)
    if(is.na(value)|value=="bbbbbbbb"){
      output<-NA
      }else{
      output<-as.numeric(value)
    }
  }
  if(field=="FPARC"){#Family presence and age of related children
    if(is.na(value)|value=="b"){output<-NA}else{
      if(value==1){output<-"With related children under 5 years only"}
      if(value==2){output<-"With related children 5 to 17 years only"}
      if(value==3){output<-"With related children under 5 years and 5 to 17 years"}
      if(value==4){output<-"No related children"}
    }
  }
  if(field=="HHL"){#Household language
    if(is.na(value)|value=="b"){output<-NA}else{
      if(value==1){output<-"English only"}
      if(value==2){output<-"Spanish"}
      if(value==3){output<-"Other Indo-European languages"}
      if(value==4){output<-"Asian and Pacific Island languages"}
      if(value==5){output<-"Other language"}
    }
  }
  if(field=="LNGI"){#Household language
    if(is.na(value)|value=="b"){output<-NA}else{
      if(value==1){output<-"Spoken English Fluency"}
      if(value==2){output<-"Limited Spoken English"}
    }
  }
  if(field=="HINCP"){#Household income (past 12 months, use ADJINC to adjust HINCP to constant dollars)
    if(is.na(value)|value=="bbbbbbbb"){
      output<-NA
      }else{
      output<-as.numeric(value)
    }
  }
  if(field=="R65"){#Presence of persons 65 years and over in household (unweighted)
    if(is.na(value)|value=="b"){output<-NA}else{
      if(value==0){output<-"No person 65 and over"}
      if(value==1){output<-"1 person 65 and over"}
      if(value==2){output<-"2 or more persons 65 and over"}
    }
  }
  if(field=="DDRS"){#Self-care difficulty
    if(is.na(value)|value=="b"){output<-NA}else{
      if(value==1){output<-"Yes"}
      if(value==2){output<-"No"}
    }
  }
  if(field=="SSIP"){# Supplementary Security Income past 12 months (use ADJINC to adjust SSIP to constant dollars)
    if(is.na(value)|value=="bbbbb"){
      output<-NA
      }else{
      output<-as.numeric(value)
    }
  }
  if(field=="SSP"){#Social Security income past 12 months (use ADJINC to adjust SSP to constant dollars)
    if(is.na(value)|value=="bbbbb"){
      output<-NA
      }else{
      output<-as.numeric(value)
    }
  }
  if(field=="PAP"){#Public assistance income past 12 months (use ADJINC to adjust to constant dollars)
    if(is.na(value)|value=="bbbbb"){
      output<-NA
      }else{
      output<-as.numeric(value)
    }
  }
  if(field=="WAGP"){#Wages or salary income past 12 months (use ADJINC to adjust WAGP to constant dollars)
    if(is.na(value)|value=="bbbbbb"){
      output<-NA
      }else{
      output<-as.numeric(value)
    }
  }
  if(field=="AGEP"){#Age
    if(is.na(value)|value=="bbbbbb"){
      output<-NA
      }else{
      output<-as.numeric(value)
    }
  }
    if(field=="SCHG"){#Grade level attending
    if(is.na(value)|value=="bb"){output<-NA}else{
      if(value=="01"){output<-"Preschool"}
      if(value=="02"){output<-"Kindergarten"}
      if(value=="03"){output<-"1st Grade"}
      if(value=="04"){output<-"2nd Grade"}
      if(value=="05"){output<-"3rd Grade"}
      if(value=="06"){output<-"4th Grade"}
      if(value=="07"){output<-"5th Grade"}
      if(value=="08"){output<-"6th Grade"}
      if(value=="09"){output<-"7th Grade"}
      if(value=="10"){output<-"8th Grade"}
      if(value=="11"){output<-"9th Grade"}
      if(value=="12"){output<-"10th Grade"}
      if(value=="13"){output<-"11th Grade"}
      if(value=="14"){output<-"12th Grade"}
      if(value=="15"){output<-"Undergraduate College"}
      if(value=="16"){output<-"Graduate College"}
    }
  }
  return(output)
}

# test function
table(sapply(hca.select$NP,FUN=pums_recode,field="NP"),hca.select$NP,exclude = NULL)

# Apply recode
## Household
hca.select$WGTP_recode<-sapply(hca.select$WGTP,FUN = pums_recode,field="WGTP")
hca.select$ADJHSG_recode<-sapply(hca.select$ADJHSG,FUN = pums_recode,field="ADJHSG")
hca.select$ADJINC_recode<-sapply(hca.select$ADJINC,FUN = pums_recode,field="ADJINC")
hca.select$NP_recode<-sapply(hca.select$NP,FUN = pums_recode,field="NP")
hca.select$TYPE_recode<-sapply(hca.select$TYPE,FUN = pums_recode,field="TYPE")
hca.select$BLD_recode<-sapply(hca.select$BLD,FUN = pums_recode,field="BLD")
hca.select$ELEP_recode<-sapply(hca.select$ELEP,FUN = pums_recode,field="ELEP")
hca.select$FULP_recode<-sapply(hca.select$FULP,FUN = pums_recode,field="FULP")
hca.select$GASP_recode<-sapply(hca.select$GASP,FUN = pums_recode,field="GASP")
hca.select$HFL_recode<-sapply(hca.select$HFL,FUN = pums_recode,field="HFL")
hca.select$TEN_recode<-sapply(hca.select$TEN,FUN = pums_recode,field="TEN")
hca.select$YBL_recode<-sapply(hca.select$YBL,FUN = pums_recode,field="YBL")
hca.select$HINCP_recode<-sapply(hca.select$HINCP,FUN = pums_recode,field="HINCP")

# Calculate %FPL
FPL<-function(year,np,inc,output="%FPL"){
  output<-"ERROR NOT DEFINED"
  if(np==0){
    return(NA)
    break
    }
  if(year==2013){
    fpl<-11490+np*4020
    p.fpl<-inc/fpl
  }else if(year==2014){
    fpl<-11670+np*4060
    p.fpl<-inc/fpl
  }else if(year==2015){
    fpl<-11770+np*4160
    p.fpl<-inc/fpl
  }else if(year==2016){
    if(np==1){fpl<-12071}
    if(np==2){fpl<-15397}
    if(np==3){fpl<-18850}
    if(np==4){fpl<-24230}
    if(np==5){fpl<-28695}
    if(np==6){fpl<-32473}
    if(np==7){fpl<-36927}
    if(np==8){fpl<-40968}
    if(np>8){fpl<-40968+(np-8)*4160}
    p.fpl<-inc/fpl
  }else if(year==2017){
    fpl<-12060+np*4180
    p.fpl<-inc/fpl
  }
  output<-ifelse(exists("p.fpl"),p.fpl,output)
  return(output)
}

# bound income (?)
hca.select$HINCP_recode[hca.select$HINCP_recode<=0&!is.na(hca.select$HINCP_recode)]<-.0001

# calculate %FPL
n_distinct(hca.select$SERIALNO)==nrow(hca.select)
hca.select2<-hca.select %>% mutate(year=as.numeric(substr(SERIALNO,1,4))) %>% 
  group_by(SERIALNO) %>% mutate(FPL=FPL(year=year,np=NP_recode,inc=HINCP_recode))

# calculate electricity burden (top bound to 200%)
hca.select2$E.EB<-(hca.select2$ELEP_recode)/((hca.select2$HINCP_recode)/12)
hca.select2$E.EB[hca.select2$E.EB>2]<-2
quantile(round(hca.select2$E.EB,3),na.rm = TRUE,probs = seq(0,1,.01))*100

# calculate gas burden (top bound to 200%)
hca.select2$G.EB<-(hca.select2$GASP_recode)/((hca.select2$HINCP_recode)/12)
hca.select2$G.EB[hca.select2$G.EB>2]<-2
quantile(round(hca.select2$G.EB,3),na.rm = TRUE,probs = seq(0,1,.01))*100

# fuel type
hca.select2$fuel_type=NA
hca.select2$fuel_type[is.na(hca.select2$ELEP_recode)&is.na(hca.select2$GASP_recode)]<-"Does not pay fuel costs"
hca.select2$fuel_type[(hca.select2$ELEP_recode>0)&is.na(hca.select2$GASP_recode)]<-"Electric Only"
hca.select2$fuel_type[is.na(hca.select2$ELEP_recode)&(hca.select2$GASP_recode>0)]<-"Gas Only"
hca.select2$fuel_type[(hca.select2$ELEP_recode>0)&(hca.select2$GASP_recode>0)]<-"Electric and Gas"
table(hca.select2$fuel_type,exclude = NULL)

# summary
LI.EB.Summary_FT<-hca.select2 %>% filter(!is.na(HINCP_recode)) %>% 
  group_by(`Above 200% FPL`=FPL>2,fuel_type) %>% 
  summarise(
    `Total Households`=sum(WGTP_recode),
    `Count with Electricity Costs`=sum(WGTP_recode[!is.na(ELEP_recode)],na.rm = TRUE),
    `Average Electric Bill`=weighted.mean(ELEP_recode,WGTP_recode,na.rm = TRUE),
    `Average Electric Energy Burden`=weighted.mean(E.EB,WGTP_recode,na.rm = TRUE),
    `Count with Gas Costs`=sum(WGTP_recode[!is.na(GASP_recode)],na.rm = TRUE),
    `Average Gas Bill`=weighted.mean(GASP_recode,WGTP_recode,na.rm = TRUE),
    `Average Gas Energy Burden`=weighted.mean(G.EB,WGTP_recode,na.rm = TRUE)
    )

xlsx::write.xlsx(LI.EB.Summary_FT %>% data.frame(),file="/users/lehndorff/desktop/CA LI EB.xlsx",row.names = FALSE,sheetName = "By Fuel Type")

LI.EB.Summary_HF<-hca.select2 %>% filter(!is.na(HINCP_recode)) %>% 
  group_by(`Above 200% FPL`=FPL>2,HFL_recode) %>% 
  summarise(
    `Total Households`=sum(WGTP_recode),
    `Count with Electricity Costs`=sum(WGTP_recode[!is.na(ELEP_recode)],na.rm = TRUE),
    `Average Electric Bill`=weighted.mean(ELEP_recode,WGTP_recode,na.rm = TRUE),
    `Average Electric Energy Burden`=weighted.mean(E.EB,WGTP_recode,na.rm = TRUE),
    `Count with Gas Costs`=sum(WGTP_recode[!is.na(GASP_recode)],na.rm = TRUE),
    `Average Gas Bill`=weighted.mean(GASP_recode,WGTP_recode,na.rm = TRUE),
    `Average Gas Energy Burden`=weighted.mean(G.EB,WGTP_recode,na.rm = TRUE)
    )

xlsx::write.xlsx(LI.EB.Summary_HF %>% data.frame(),file="/users/lehndorff/desktop/CA LI EB.xlsx",row.names = FALSE,sheetName = "By Reported Heating Fuel",append = TRUE)
