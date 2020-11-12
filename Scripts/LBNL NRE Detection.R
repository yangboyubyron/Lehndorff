# LBNL NRE Detection Application
# devtools::install_github("LBNL-ETA/RMV2.0")
# devtools::install_github("LBNL-ETA/nre")
library(dplyr)

Site.8448 <- read.csv("/volumes/Projects JBK/416034 - NEEA EULR/Analysis/Synthetic Data/Site 8448.csv",stringsAsFactors = F)
weather<-read.csv("/volumes/Projects JBK/416034 - NEEA EULR/Analysis/Synthetic Data/select_weather.csv",stringsAsFactors = F)

comb<-Site.8448 %>% distinct() %>% 
  left_join(weather %>% distinct(),by="min_t") %>% 
  group_by(ee_site_id,regname,min_t) %>% 
  filter(n()==1)

pre_nre<-comb %>% 
  filter(as.Date(date)<=as.Date("2020-03-15")) %>% 
  select(ee_site_id,regname,min_t,temp_f,est_avg_power) %>% 
  mutate(time=format(as.POSIXct(min_t),"%m/%d/%y %H:%M"),eload=est_avg_power,Temp=temp_f) %>% 
  ungroup() %>% 
  select(ee_site_id,regname,time,eload,Temp)

post_nre<-comb %>% 
  filter(as.Date(date)>as.Date("2020-03-15")) %>% 
  select(ee_site_id,regname,min_t,temp_f,est_avg_power) %>% 
  mutate(time=format(as.POSIXct(min_t),"%m/%d/%y %H:%M"),eload=est_avg_power,Temp=temp_f) %>% 
  ungroup() %>% 
  select(ee_site_id,regname,time,eload,Temp)

gbm_res <- RMV2.0::gbm_baseline(train_Data = pre_nre %>% filter(regname=="Mains") %>% select(-ee_site_id,-regname) %>% ungroup(),
                                pred_Data  = post_nre %>% filter(regname=="Mains") %>% select(-ee_site_id,-regname) %>% ungroup(),
                                variables = c("Temp", "tow"),
                                ncores=4)

post_Data <- dplyr::select(gbm_res$pred,time,eload,Temp)
y_pred <- gbm_res$prediction
predicted_post_Data <- post_Data
predicted_post_Data$eload <- y_pred

nre_detected <- nre::nre_detect(post_Data,predicted_post_Data)

dissim_tab <- nre_detected$dissim_tab
idx_nre <- which(dissim_tab$date > "2017-06-30" & dissim_tab$date < "2017-08-11")
idx_nre_2 <- which(dissim_tab$date == "2017-02-20" | dissim_tab$date == "2017-01-16")
idx_nre <- c(idx_nre_2,idx_nre)
dissim_tab$diss_nre <- NA
dissim_tab$diss_nre[idx_nre] <- dissim_tab$dissimilarity[idx_nre]
post_d <- nre::convert_to_daily(post)
post_d$eload_nre <- NA
post_d$eload_nre[idx_nre] <- post_d$eload[idx_nre]
post_d$eload_nre2 <- NA
post_d$eload_nre2[idx_nre_2] <- post_d$eload[idx_nre_2]
