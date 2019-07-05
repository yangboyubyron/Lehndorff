# SD Sampling
library(readxl)
library(dplyr)
library(lubridate)
library(xlsx)

# load data with clean addresses
clean.adds<-read.csv("/volumes/Projects Berkeley/428010 - SDG&E ULP Verification/CONFIDENTIAL - Data from Client/Data - Clean Addresses.csv",stringsAsFactors = FALSE)

# aggregate to calculate tiers
tier.def<-clean.adds %>% group_by(clean.address,big.box) %>% summarise(total.bulbs=sum(MeasureQty),bulb.6=total.bulbs/6)

# total bulbs per 6
tier.def$tier<-"Not Defined"
tier.def$tier[tier.def$bulb.6<1000]<-"Tier 3"
tier.def$tier[tier.def$bulb.6>=3000]<-"Tier 1"
tier.def$tier[tier.def$bulb.6<3000&tier.def$bulb.6>=1000]<-"Tier 2"
tier.def$tier[tier.def$big.box==1]<-"Big Box"
table(tier.def$tier)

# Join tier defs back on
w.tier<-clean.adds %>% left_join(tier.def %>% select(-big.box),by="clean.address") %>% filter(!big.box)

# define bulb type
w.tier$bulb.type<-"NOT DEFINED"
w.tier$bulb.type[grepl("LED Candelabra",w.tier$MeasureName)]<-"Candelabra"
w.tier$bulb.type[grepl("LED globe",w.tier$MeasureName)]<-"Globe"
w.tier$bulb.type[grepl("LED R/BR",w.tier$MeasureName,fixed = TRUE)]<-"R/BR"
table(w.tier$bulb.type)

# Tier summary
tier.sum<-w.tier %>% group_by(tier) %>% 
  summarise(
    sites=n_distinct(clean.address),
    total.bulbs=sum(MeasureQty),
    `R/BR %`=sum(MeasureQty[bulb.type=="R/BR"])/total.bulbs,
    `Globe %`=sum(MeasureQty[bulb.type=="Globe"])/total.bulbs,
    `Candelabra %`=sum(MeasureQty[bulb.type=="Candelabra"])/total.bulbs,
    percent.of.sites=sites/n_distinct(w.tier$clean.address))
# write.xlsx(tier.sum %>% data.frame(),sheetName = "Type by Tier", file="/volumes/Projects Berkeley/428010 - SDG&E ULP Verification/CONFIDENTIAL - Data from Client/Sampling Tables.xlsx",row.names = FALSE,append = FALSE)

# manufacturer by tier
man.tier<-w.tier %>% group_by(ContractorName) %>% 
  summarise(
    total.bulbs=sum(MeasureQty),
    Tier.1=sum(MeasureQty[tier=="Tier 1"])/total.bulbs,
    Tier.2=sum(MeasureQty[tier=="Tier 2"])/total.bulbs,
    Tier.3=sum(MeasureQty[tier=="Tier 3"])/total.bulbs
  )
# write.xlsx(man.tier %>% data.frame(),sheetName = "Tier by Manuf", file="/volumes/Projects Berkeley/428010 - SDG&E ULP Verification/CONFIDENTIAL - Data from Client/Sampling Tables.xlsx",row.names = FALSE,append = TRUE)

# manufacturer by bulb type
man.bulb<-w.tier %>% group_by(ContractorName) %>% 
  summarise(
    total.bulbs=sum(MeasureQty),
    `R/BR %`=sum(MeasureQty[bulb.type=="R/BR"])/total.bulbs,
    `Globe %`=sum(MeasureQty[bulb.type=="Globe"])/total.bulbs,
    `Candelabra %`=sum(MeasureQty[bulb.type=="Candelabra"])/total.bulbs
  )
# write.xlsx(man.bulb %>% data.frame(),sheetName = "Bulb by Manuf", file="/volumes/Projects Berkeley/428010 - SDG&E ULP Verification/CONFIDENTIAL - Data from Client/Sampling Tables.xlsx",row.names = FALSE,append = TRUE)

# sampling
site.agg<-w.tier %>% group_by(clean.address,tier) %>% summarise(total.bulbs=sum(total.bulbs))

site.agg$target<-NA
site.agg$target[site.agg$tier=="Tier 1"]<-32
site.agg$target[site.agg$tier=="Tier 2"]<-55
site.agg$target[site.agg$tier=="Tier 3"]<-27
table(site.agg$target,exclude = NULL)

set.seed(883507)
rand.rank<-site.agg %>% group_by(tier) %>% 
  mutate(
    rand=runif(n()),
    rank=rank(rand,ties.method = "first"),
    in.sample=rank<=target)
table(rand.rank$in.sample,rand.rank$tier)

# sampling impact
w.sample<-w.tier %>% left_join(rand.rank %>% ungroup() %>% select(clean.address,in.sample),"clean.address")
# write.csv(w.sample,"/volumes/Projects Berkeley/428010 - SDG&E ULP Verification/CONFIDENTIAL - Data from Client/Sample Data.csv", row.names = FALSE)

# Tier sample summary
tier.sample<-w.sample %>% group_by(tier) %>% 
  summarise(
    sites=n_distinct(clean.address),sites.in.sample=n_distinct(clean.address[in.sample]),
    total.bulbs=sum(MeasureQty),perc.sample.bulbs=sum(MeasureQty[in.sample])/total.bulbs)
sum(w.sample$MeasureQty[w.sample$in.sample])/sum(w.sample$MeasureQty)
# write.xlsx(tier.sample %>% data.frame(),sheetName = "Sample by Tier", file="/volumes/Projects Berkeley/428010 - SDG&E ULP Verification/CONFIDENTIAL - Data from Client/Sampling Tables.xlsx",row.names = FALSE,append = TRUE)

manf.sample<-w.sample %>% group_by(ContractorName) %>% 
  summarise(
    total.bulbs=sum(MeasureQty),perc.sample.bulbs=sum(MeasureQty[in.sample])/total.bulbs
  )
# write.xlsx(manf.sample %>% data.frame(),sheetName = "Sample by Manuf", file="/volumes/Projects Berkeley/428010 - SDG&E ULP Verification/CONFIDENTIAL - Data from Client/Sampling Tables.xlsx",row.names = FALSE,append = TRUE)

type.sample<-w.sample %>% group_by(bulb.type) %>% 
  summarise(
    total.bulbs=sum(MeasureQty),perc.sample.bulbs=sum(MeasureQty[in.sample])/total.bulbs
  )
# write.xlsx(type.sample %>% data.frame(),sheetName = "Sample by Bulb", file="/volumes/Projects Berkeley/428010 - SDG&E ULP Verification/CONFIDENTIAL - Data from Client/Sampling Tables.xlsx",row.names = FALSE,append = TRUE)


