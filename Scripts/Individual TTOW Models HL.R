library(dplyr)
library(ggplot2)

## Data import and merge ----
# ami <- readr::read_csv("/Volumes/Projects/~ Closed Projects/419012 - SCE HOPPs AMI/Data/Sample Data for Ted/Sample_AMI.csv")
ami <-readr::read_csv("/volumes/Projects/~ Closed Projects/419012 - SCE HOPPs AMI/Data/SumHVAC_60min_2.csv")
ami$said<-ami$Site
ami$said[ami$Site==1]<-1
ami$said[ami$Site==2]<-2
ami$said[ami$Site==3]<-3
ami$said[ami$Site==4]<-4
ami$said[ami$Site==5]<-5
ami$said[ami$Site==6]<-7
ami$said[ami$Site==7]<-8
ami$said[ami$Site==8]<-434127
ami$said[ami$Site==9]<-13497992
ami$said[ami$Site==10]<-32748371
table(ami$said)

ami$readdate<-ami$Date.Time

ami$kwh<-ami$sumHVACWh/1000

ami<-ami %>% select(said,readdate,kwh)

weather <- readr::read_csv("/Volumes/Projects/~ Closed Projects/419012 - SCE HOPPs AMI/Data/Sample Data for Ted/Sample_Weather.csv")
weather <- distinct(weather, stationid, readdate, .keep_all=TRUE)
track <- readr::read_csv("/Volumes/Projects/~ Closed Projects/419012 - SCE HOPPs AMI/Data/Sample Data for Ted/Sample_Tracking.csv")
track$min_part_date <- as.Date(track$min_part_date, "%m/%d/%y")
track$max_part_date <- as.Date(track$max_part_date, "%m/%d/%y")

dta <- ami %>% left_join(select(track, said, stationid, min_part_date, max_part_date), by="said") %>%
  left_join(weather, by=c("stationid", "readdate"))

dta$post <- ifelse(as.Date(dta$readdate)>dta$max_part_date, 1, ifelse(as.Date(dta$readdate)<dta$min_part_date, 0, NA))
dta <- select(dta, said, readdate, kwh, temp, post)
rm(ami, weather)

dta$readdate <- as.POSIXct(dta$readdate)
dta$cdh <- ifelse(dta$temp>65, dta$temp-65, 0)
dta$hdh <- ifelse(dta$temp<65, 65-dta$temp, 0)
dta$date <- substr(dta$readdate, 1, 10)
dta <- dta %>% group_by(said, date) %>%
  mutate(cdd=round(mean(cdh, na.rm=TRUE)), hdd=round(mean(hdh, na.rm=TRUE))) %>%
  ungroup() %>% select(-cdh, -hdh)


dta$readdate <- as.POSIXlt(round(as.numeric(dta$readdate)/(15*60))*(15*60), origin='1970-01-01', tz="GMT")
dta$tow <- format(dta$readdate, "%u-%R") # "%u = DOW(1-7)
length(unique(dta$tow))==7*96

# Define component temperatures
dta$base <- dta$temp
dta$Tc6 <- ifelse(dta$temp>90, dta$temp-90, 0)
dta$Tc5 <- ifelse(dta$temp>80, ifelse(dta$temp-80>10, 10, dta$temp-80), 0)
dta$Tc4 <- ifelse(dta$temp>70, ifelse(dta$temp-70>10, 10, dta$temp-70), 0)
dta$Tc3 <- ifelse(dta$temp>60, ifelse(dta$temp-60>10, 10, dta$temp-60), 0)
dta$Tc2 <- ifelse(dta$temp>50, ifelse(dta$temp-50>10, 10, dta$temp-50), 0)
dta$Tc1 <- ifelse(dta$temp<50, dta$temp, 50)


## Define plotting fxn to share across trials
plot_ttow <- function(x=dta_test, pre_test=TRUE, coord=c(18, 7), method="TTOW", color_labs=NULL){
  # x$weekend <- ifelse(x$weekend==0, "Weekend", "Weekday")
  dta_plot <- x %>% select(-readdate) %>% mutate(hour=substr(tow, 3, 4)) %>%
    group_by(said, hour) %>% summarize(actual=mean(kwh), predicted=mean(fit), lwr=mean(lwr), upr=mean(upr),
                                       sse=sum((fit-kwh)^2, na.rm=TRUE), n_obs = n())
  dta_plot$hour <- as.numeric(dta_plot$hour)
  pred_lab <- sprintf("Predicted (%s model %s)", method, ifelse(pre_test==TRUE, "sample pre", "est of post"))
  scale_ht <- (max(dta_plot$upr) - min(dta_plot$upr))/4
  if (is.null(color_labs)){
    color_labs <- c("Actual", pred_lab)
  }
  plot <- ggplot(data=dta_plot) +
    labs(title=sprintf("%s load shape for ID #%d", ifelse(pre_test==TRUE, "Pre-period HO", "Post-period"), id_plot), y="Energy Usage (kWh)", x="Hour of Day", color=NULL) +
    theme(legend.position="bottom") +
    geom_line(aes(x=hour, y=predicted, group=said, color=color_labs[2])) +
    geom_ribbon(aes(x=hour, ymax=upr, ymin=lwr, group=said), fill="red", alpha=0.1) +
    geom_line(aes(x=hour, y=actual, group=said, color=color_labs[1])) +
    scale_colour_manual("", breaks = color_labs, values = c("blue", "red")) +
    annotate("rect", xmin=coord[1]-5, xmax=coord[1]+5, ymin=coord[2]-scale_ht, ymax=coord[2]+scale_ht, alpha=1, fill="white")  #+
    #facet_grid(.~weekend)
  if (pre_test==TRUE){
    day <- dta_plot %>% summarize(
      act=sum(actual), pred=sum(predicted), diff=sum(predicted-actual)*100/sum(predicted),
      rmse=sqrt(sum(sse)/sum(n_obs)), cv_rmse=sqrt(sum(sse)/sum(n_obs))*24/sum(actual))
    plot +  annotate("text", label=paste(
      paste("Actual Daily kWh:",round(day$act, 2)),
      paste("Predicted Daily kWh:",round(day$pred, 2)),
      paste("% Difference: ", round(day$diff, 1), "%", sep=""),
      paste("RMSE:", round(day$rmse,2)),
      paste("CV(RMSE): ", round(day$cv_rmse,2), sep=""),
      sep="\n"),  x=coord[1], y=coord[2], color="black", size=3)
  } else {
    plot + annotate("text", label=paste(
      paste("Actual Daily kWh:",round(sum(dta_plot$actual), 2)),
      paste("Predicted Daily kWh:",round(sum(dta_plot$predicted), 2)),
      paste("Savings kWh: ", round(sum(dta_plot$predicted-dta_plot$actual), 2)),
      paste("Savings %: ", round(sum(dta_plot$predicted-dta_plot$actual)*100/sum(dta_plot$predicted), 1), "%", sep=""),
      # paste("CV(RMSE): ", round(sqrt(sum(savings$sse)/sum(savings$n_obs))/sum(savings$wt.act),2), sep=""),
      sep="\n"),  x=coord[1], y=coord[2], color="black", size=3)
  }
}

# remove bad data
save_dta<-dta
dta<-dta %>% filter(!(said==8&as.Date(readdate)>="2016-10-20"&as.Date(readdate)<="2017-02-01"))

# Select holdout sample weeks
# set.seed(71614) #HO1
# set.seed(194834) #HO2
set.seed(813098) #HO3

dta$holdout <- (as.numeric(strftime(dta$readdate, '%V')) %in% sample(1:53, 16))*1
table(dta$holdout,dta$said)

## 2. Estimate occupied hours across entire week, use this to define typical hours of operation ----
# said; Sample ID
sel_id <- 8; id_plot<-7

# # Use sample pre to define operating schedule
dta$dow <- substr(dta$tow, 1, 1)
dta$readdate <- as.POSIXct(dta$readdate)

x <- subset(dta, said==sel_id & holdout==0 & post==0 & is.na(temp)==FALSE)
dow_limits <- x  %>%
  summarize(p10=quantile(kwh, probs = c(0.2)), p90=quantile(kwh, probs = c(0.8)))
lim <- with(dow_limits, p10+0.1*(p90-p10))
avg <- x %>% 
  select(-readdate) %>%
  group_by(dow, tow) %>%
  summarize(kwh=mean(kwh)) %>% ungroup() %>%
  mutate(flag=kwh>lim) %>%
  group_by(dow) %>%
  summarize(start=min(tow[flag==TRUE]), end=max(tow[flag==TRUE]))

dta_full <- subset(dta, said==sel_id & is.na(temp)==FALSE) %>% left_join(avg, by="dow")
dta_full$operating <- ifelse(is.na(dta_full$start), 0, (dta_full$tow>dta_full$start & dta_full$tow<dta_full$end)*1)

# ggplot(data=subset(dta_full, holdout==0 & post==0) %>% group_by(said, dow, tow) %>% summarize(kwh=mean(kwh))) +
#   geom_point(aes(x=tow, y=kwh, color=dow)) +
#   geom_line(aes(x=tow, y=lim, group=said), color="red")


# TTOW - Random sample pre-period model vs. holdout pre
# ttow_op_lm_test <- lm(kwh ~ tow + Tc1 + Tc2 + Tc3 + Tc4 + Tc5 + Tc6, data=subset(dta_full, holdout==0 & post==0 & operating==1))
# ttow_non_lm_test <- lm(kwh ~ tow + Tc1 + Tc2 + Tc3 + Tc4 + Tc5 + Tc6, data=subset(dta_full, holdout==0 & post==0 & operating==0))
# 
# pred_ho_op <- predict(ttow_op_lm_test, subset(dta_full, holdout==1 & post==0 & operating==1), interval="confidence", level=0.95) %>%
#   unlist() %>% matrix(., ncol = 3) %>% as.data.frame()
# pred_ho_non <- predict(ttow_non_lm_test, subset(dta_full, holdout==1 & post==0 & operating==0), interval="confidence", level=0.95) %>%
#   unlist() %>% matrix(., ncol = 3) %>% as.data.frame()
# dta_test <- data.frame(bind_rows(subset(dta_full, holdout==1 & post==0 & operating==1), subset(dta_full, holdout==1 & post==0 & operating==0)),
#                        fit=c(pred_ho_op$V1, pred_ho_non$V1),
#                        lwr=c(pred_ho_op$V2, pred_ho_non$V2),
#                        upr=c(pred_ho_op$V3, pred_ho_non$V3))
# # plot_ttow(dta_test, pre_test=TRUE, coord=c(15, 5))


# # TTOW - Full pre-period model vs. actual post----
# ttow_op_lm <- lm(kwh ~ tow + Tc1 + Tc2 + Tc3 + Tc4 + Tc5 + Tc6, data=subset(dta_full,  post==0 & operating==1))
# ttow_non_lm <- lm(kwh ~ tow + Tc1 + Tc2 + Tc3 + Tc4 + Tc5 + Tc6, data=subset(dta_full, post==0 & operating==0))
#
# pred_pre_op <- predict(ttow_op_lm, subset(dta_full, post==0 & operating==1), interval="confidence", level=0.95) %>%
#   unlist() %>% matrix(., ncol = 3) %>% as.data.frame()
# pred_pre_non <- predict(ttow_non_lm, subset(dta_full, post==0 & operating==0), interval="confidence", level=0.95) %>%
#   unlist() %>% matrix(., ncol = 3) %>% as.data.frame()
# dta_pre <- data.frame(bind_rows(subset(dta_full, post==0 & operating==1), subset(dta_full, post==0 & operating==0)),
#                        fit=c(pred_pre_op$V1, pred_pre_non$V1),
#                        lwr=c(pred_pre_op$V2, pred_pre_non$V2),
#                        upr=c(pred_pre_op$V3, pred_pre_non$V3))
# # plot_ttow(dta_pre, pre_test=TRUE, coord=c(15, 5))
#
#
# pred_post_op <- predict(ttow_op_lm, subset(dta_full, post==1 & operating==1), interval="confidence", level=0.95) %>%
#   unlist() %>% matrix(., ncol = 3) %>% as.data.frame()
# pred_post_non <- predict(ttow_non_lm, subset(dta_full, post==1 & operating==0), interval="confidence", level=0.95) %>%
#   unlist() %>% matrix(., ncol = 3) %>% as.data.frame()
# dta_post <- data.frame(bind_rows(subset(dta_full, post==1 & operating==1), subset(dta_full, post==1 & operating==0)),
#                        fit=c(pred_post_op$V1, pred_post_non$V1),
#                        lwr=c(pred_post_op$V2, pred_post_non$V2),
#                        upr=c(pred_post_op$V3, pred_post_non$V3))
# # plot_ttow(dta_post, pre_test=FALSE, coord=c(15, 5))




# ## AMICS Indiv - Random sample pre-period model vs. holdout pre ----
# hopps_daybins <- readr::read_csv("/Volumes/Projects/~ Closed Projects/419012 - SCE HOPPs AMI/Data/hopps_daybins.csv")
hopps_daybins$date <- as.character(hopps_daybins$date)
st_map <- select(track, stationid, said)

# Random sample pre-period model vs. holdout pre
dta_full <- left_join(dta_full, st_map, by="said") %>% left_join(hopps_daybins, by=c("stationid", "date"))
dta_full$hour <- format(dta_full$readdate, "%H")
amics_lm_test <- dta_full %>% subset(holdout==0 & post==0) %>% group_by(said, day_bin, hour) %>% summarize(fit=mean(kwh, na.rm=TRUE), lwr=fit*0.99, upr=fit*1.01)
pred_ho <- dta_full %>% subset(holdout==1 & post==0) %>% left_join(amics_lm_test, by=c("said", "day_bin", "hour"))
plot_ttow(subset(pred_ho, is.na(fit)==FALSE), pre_test=TRUE, coord=c(13, 2), "AMICS")
# ggsave(file = paste0("~/desktop/AMI HVAC Outputs/pre_HO3_",id_plot,".jpg"))

# # Full pre-period model vs. actual post
amics_lm <- dta_full %>% subset(post==0) %>% group_by(said, day_bin, hour) %>% summarize(fit=mean(kwh, na.rm=TRUE), lwr=fit*0.99, upr=fit*1.01)
pred_pre <- dta_full %>% subset(post==0) %>% left_join(amics_lm, by=c("said", "day_bin", "hour"))
plot_ttow(subset(pred_pre, is.na(fit)==FALSE), pre_test=TRUE, coord=c(13, 2), "AMICS") + labs(title=sprintf("Pre-period full load shape for ID %s", id_plot))
# ggsave(file = paste0("~/desktop/AMI HVAC Outputs/pre_full_",id_plot,".jpg"))
pred_post <- dta_full %>% subset(post==1) %>% left_join(amics_lm, by=c("said", "day_bin", "hour"))
plot_ttow(subset(pred_post, is.na(fit)==FALSE), pre_test=FALSE, coord=c(15, 1), "AMICS")
# ggsave(file = paste0("~/desktop/AMI HVAC Outputs/post_",id_plot,".jpg"))

ggplot(pred_pre)+
  geom_point(aes(x=readdate,y=kwh),size=.01,color="blue")+
  geom_point(aes(x=readdate,y=fit),size=.01,color="red")

ggplot(pred_post)+
  geom_point(aes(x=readdate,y=kwh),size=.01,color="blue")+
  geom_point(aes(x=readdate,y=fit),size=.01,color="red")

amics_lm_test_hvac<-amics_lm_test
pred_ho_hvac<-pred_ho
amics_lm_hvac<-amics_lm
pred_pre_hvac<-pred_pre
pred_post_hvac<-pred_post

save(amics_lm_test_hvac,pred_ho_hvac, amics_lm_hvac, pred_pre_hvac, pred_post_hvac,
     file=sprintf("~/desktop/AMI HVAC Outputs/amics_ttow_id%s.Rdata", id_plot))

rm(amics_lm_test_hvac,amics_lm_test,
pred_ho_hvac,pred_ho,
amics_lm_hvac,amics_lm,
pred_pre_hvac,pred_pre,
pred_post_hvac,pred_post)


# Model fit stats
# fit_stats <- function(amics, ttow, overlap=TRUE){
#   if(overlap==TRUE){
#     amics <- semi_join(amics, ttow, by=c("said", "readdate"))
#     ttow <- semi_join(ttow, amics, by=c("said", "readdate"))
#   }
#
#   amics_out <- amics %>% mutate(model="amics", said=sel_id, avg=mean(kwh, na.rm=TRUE)) %>%
#     summarize(model=first(model), said=first(said),
#       TSS=sum((kwh-avg)^2),
#       ESS=sum((fit-avg)^2),
#       R_sq=ESS/TSS, n=n(),
#       SSE=sum((fit-kwh)^2),
#       RMSE=sqrt(SSE/(n-23)),
#       CV_RMSE=RMSE/mean(kwh),
#       NMBE=sum(fit-avg)/sum(avg))
#   ttow_out <- ttow %>% mutate(model="ttow", said=sel_id, avg=mean(kwh, na.rm=TRUE)) %>%
#     summarize(model=first(model), said=first(said),
#       TSS=sum((kwh-avg)^2),
#       ESS=sum((fit-avg)^2),
#       R_sq=ESS/TSS, n=n(),
#       SSE=sum((fit-kwh)^2),
#       RMSE=sqrt(SSE/(n-23)),
#       CV_RMSE=RMSE/mean(kwh),
#       NMBE=sum(fit-avg)/sum(avg))
#   bind_rows(amics=amics_out, ttow=ttow_out) %>% select(-ESS, -TSS)
# }

load("/Volumes/Projects/~ Closed Projects/419012 - SCE HOPPs AMI/Data/Outputs/amics_ttow_id7.Rdata")

# Overall holdout error (already stored)
fit_stats(amics=subset(pred_ho, is.na(fit)==FALSE),
          ttow=subset(dta_test, is.na(fit)==FALSE))

# 1. Hot max temp -- subset of holdout days where temp reached 85 at some point
days <- subset(pred_ho, temp>85)$date %>% unique()
fit1 <- fit_stats(amics=subset(pred_ho, is.na(fit)==FALSE & date %in% days),
          ttow=subset(dta_test, is.na(fit)==FALSE))
# 2. Hot days -- subset of holdout days with CDD of 20+ (i.e. average daily temp ~ 85)
fit2 <- fit_stats(amics=subset(pred_ho, is.na(fit)==FALSE & cdd>=20),
          ttow=subset(dta_test, is.na(fit)==FALSE))
# 3. Peak -- SCE Business TOU rate on-peak: weekdays (excluding holidays) June to Sept 12-6
fit3 <- fit_stats(amics=subset(pred_ho, is.na(fit)==FALSE & weekend==0 & holiday==0 & substr(date, 6, 7) %in% c("06", "07", "08", "09") & hour %in% 12:18),
          ttow=subset(dta_test, is.na(fit)==FALSE))
# 4. Mid-Peak -- SCE Business TOU rate mid-peak: weekdays (excluding holidays) June to Sept 8-12am and 6-11pm or Oct-May 8am-9pm
fit4 <- fit_stats(amics=subset(pred_ho, is.na(fit)==FALSE & weekend==0 & holiday==0 & ((substr(date, 6, 7) %in% c("06", "07", "08", "09") & hour %in% sprintf("%02d", c(8:12, 18:23))) |
                                                                               (substr(date, 6, 7) %in% c("06", "07", "08", "09")==FALSE & hour %in% sprintf("%02d", 8:21)))),
          ttow=subset(dta_test, is.na(fit)==FALSE))

bind_cols(fit1[-c(1:2)], fit2[-c(1:2)], fit3[-c(1:2)], fit4[-c(1:2)])


# # Full sample model ----
# fit_stats(amics=subset(pred_pre, is.na(fit)==FALSE), ttow=subset(dta_pre, is.na(fit)==FALSE), overlap=FALSE)

plot_ttow(subset(pred_ho, is.na(fit)==FALSE), pre_test=TRUE, coord=c(13, 5), "AMICS")
plot_ttow(subset(dta_test, is.na(fit)==FALSE), pre_test=TRUE, coord=c(13, 5))


ho_tests <- readxl::read_excel("Desktop/HOPPs prelim results.xlsx", sheet = "Sheet2")
ho_tests$said <- as.factor(ho_tests$said)
ggplot(data=ho_tests) +
  geom_point(aes(x=said, y=n, color=model))
ggplot(data=ho_tests) +
  geom_point(aes(x=said, y=CV_RMSE, color=model))
# ggplot(data=subset(dta_full, holdout==0 & post==0) %>% group_by(said, dow, tow) %>% summarize(kwh=mean(kwh))) +
#   geom_point(aes(x=tow, y=kwh, color=dow)) +
#   geom_line(aes(x=tow, y=lim, group=said), color="red")


# Plot comparisons
plot_ttow(dta_test, pre_test=TRUE, coord=c(13, 5))
plot_ttow(subset(pred_ho, is.na(fit)==FALSE), pre_test=TRUE, coord=c(13, 5))

plot_ttow(subset(dta_test, dow %in% c("0", "6")==FALSE), pre_test=TRUE, coord=c(13, 5))
plot_ttow(subset(pred_ho, is.na(fit)==FALSE & weekend==0), pre_test=TRUE, coord=c(13, 5))

plot_ttow(subset(dta_test, format(readdate, '%m') %in% c("06", "08", "08")), pre_test=TRUE, coord=c(13, 5))
plot_ttow(subset(pred_ho, is.na(fit)==FALSE & season=="summer"), pre_test=TRUE, coord=c(13, 5))


## Descriptive plots for Pwpt
# 1. Variety of pre-period days for single customer, show the variation that our models are trying to explain
set.seed(67522)
sample_dt <- pred_pre %>% subset(holdout==0 & post==0) %>% select(date) %>% distinct()
sample_dt <- sample(sample_dt$date, 50, replace = FALSE, prob = NULL)
pred_pre$hour <- format(pred_pre$readdate, "%H") %>% as.numeric()
plot_dta <- pred_pre %>% subset(date %in% sample_dt) %>% group_by(date, hour) %>%
  summarize(actual=mean(kwh, na.rm=TRUE), predicted=mean(fit, na.rm=TRUE))
ggplot() +
  geom_line(data=plot_dta, aes(x=hour, y=actual, group=date, color=date)) +
  labs(title=sprintf("Sample of pre-period days, site #%d", id_plot), y="kWh", x="Hour of day", color=NULL) +
  guides(color=FALSE)

# 2. Average vs. model prediction across variety of days, show what we are able to explain (full sample model)
set.seed(2158)
sample_dt_ext <- sample(sample_dt, 6, replace = FALSE, prob = NULL)
# sample_dt_ext <- c("2016-09-04", "2016-09-07", "2016-10-11", "2016-10-24", "2016-10-27", "2016-12-11")
plot_dta_samp <- pred_pre %>% subset(date %in% sample_dt_ext) %>% group_by(date, hour) %>%
  summarize(actual=mean(kwh, na.rm=TRUE), predicted=mean(fit, na.rm=TRUE))

ggplot() +
  geom_line(data=plot_dta_samp, aes(x=hour, y=actual, group=date, color="Actual")) +
  geom_line(data=plot_dta_samp, aes(x=hour, y=predicted, group=date, color="Predicted")) +
  facet_wrap(~date, ncol=3) +
  scale_colour_manual("", breaks = c("Actual", "Predicted"), values = c("blue", "red")) +
  theme(legend.position="bottom") +
  labs(title=sprintf("Predicted vs. actual pre-period load shapes, site #%d", id_plot), y="kWh", x="Hour of day", color=NULL)

ggplot() +
  geom_line(data=plot_dta_samp, aes(x=hour, y=actual, group=date, color=date)) +
  labs(title=sprintf("Sample of pre-period days, site #%d", id_plot), y="kWh", x="Hour of day", color=NULL) +
  theme(legend.position="right")

plot_dta_samp_avg <- plot_dta_samp %>% group_by(hour) %>%
  summarize(actual=mean(actual, na.rm=TRUE), predicted=mean(predicted, na.rm=TRUE))
ggplot() +
  geom_line(data=plot_dta_samp, aes(x=hour, y=actual, group=date, color=date)) +
  labs(title=sprintf("Sample of pre-period days, site #%d", id_plot), y="kWh", x="Hour of day", color=NULL) +
  theme(legend.position="right")




load("/Volumes/Projects/~ Closed Projects/419012 - SCE HOPPs AMI/Data/Outputs/amics_ttow_id1.Rdata")
id_plot <- 1
pos <- c(17, 6)

#3. Overall average holdout result for a single HO test
lab <- c("Actual Holdout Pre", "Predicted Pre (TTOW model)", "Predicted Pre (AMICS model)")
plot_ttow(dta_test, pre_test=TRUE, coord=pos, method="TTOW", color_labs=lab[1:2])
plot_ttow(subset(pred_ho, is.na(fit)==FALSE), pre_test=TRUE, coord=pos, method="AMICS", color_labs=lab[-2])

#4. Overall average load for the full pre-period model
lab <- c("Actual Pre", "Predicted Pre (TTOW model)", "Predicted Pre (AMICS model)")
plot_ttow(dta_pre, pre_test=TRUE, coord=pos, method="TTOW", color_labs=lab[1:2])
plot_ttow(subset(pred_pre, is.na(fit)==FALSE), pre_test=TRUE, coord=pos, method="AMICS", color_labs=lab[-2])

lab <- c("Actual Post", "Predicted Post (TTOW model)", "Predicted Post (AMICS model)")
plot_ttow(dta_post, pre_test=FALSE, coord=pos, method="TTOW", color_labs=lab[1:2])
plot_ttow(subset(pred_post, is.na(fit)==FALSE), pre_test=FALSE, coord=pos, method="AMICS", color_labs=lab[-2])




# ## Weather sensitive kWh vs. submetered HVAC
# hvac <- readr::read_csv("/Volumes/Projects/~ Closed Projects/419012 - SCE HOPPs AMI/Data/SumHVAC_60min.csv")
# hvac$kwh_hvac <- hvac$sumHVACWh/1000
#
# hvac <- left_join(hvac, select(track, said, min_part_date, max_part_date, stationid), by=c("Site"="said"))
# hvac$pre <- (as.Date(hvac$Date.Time)<hvac$min_part_date)*1
# hvac$post <- (as.Date(hvac$Date.Time)>hvac$max_part_date)*1
# hvac <- left_join(hvac, weather, by=c("stationid", "Date.Time"="readdate"))
#
# dta_meter <- dta
# dta <- select(hvac, said=Site, readdate=Date.Time, kwh=kwh_hvac, pre, post, temp)


## 3. Try out new temp components limits (actual temps), but use the same operational definition ----
# Actual dist of observed temperatures
tp <- quantile(subset(dta_full, post==0)$temp, probs = c(0.1, 0.3, 0.5, 0.7, 0.9))
dta_full$Tc6 <- ifelse(dta_full$temp>tp[5], dta_full$temp-tp[5], 0)
dta_full$Tc5 <- ifelse(dta_full$temp>tp[4], ifelse(dta_full$temp-tp[4]>tp[5]-tp[4], tp[5]-tp[4], dta_full$temp-tp[4]), 0)
dta_full$Tc4 <- ifelse(dta_full$temp>tp[3], ifelse(dta_full$temp-tp[3]>tp[4]-tp[3], tp[4]-tp[3], dta_full$temp-tp[3]), 0)
dta_full$Tc3 <- ifelse(dta_full$temp>tp[2], ifelse(dta_full$temp-tp[2]>tp[3]-tp[2], tp[3]-tp[2], dta_full$temp-tp[2]), 0)
dta_full$Tc2 <- ifelse(dta_full$temp>tp[1], ifelse(dta_full$temp-tp[1]>tp[2]-tp[1], tp[2]-tp[1], dta_full$temp-tp[1]), 0)
dta_full$Tc1 <- ifelse(dta_full$temp<tp[1], dta_full$temp, tp[1])

# Random sample pre-period model vs. holdout pre
ttow_op_lm <- lm(kwh ~ tow + Tc1 + Tc2 + Tc3 + Tc4 + Tc5 + Tc6, data=subset(dta_full, holdout==0 & post==0 & operating==1))
ttow_non_lm <- lm(kwh ~ tow + Tc1 + Tc2 + Tc3 + Tc4 + Tc5 + Tc6, data=subset(dta_full, holdout==0 & post==0 & operating==0))

pred_ho_op <- predict(ttow_op_lm, subset(dta_full, holdout==1 & post==0 & operating==1), interval="confidence", level=0.95) # prediction interval also possible
pred_ho_non <- predict(ttow_non_lm, subset(dta_full, holdout==1 & post==0 & operating==0), interval="confidence", level=0.95) # prediction interval also possible
pred_ho_op <- as.data.frame(matrix(unlist(pred_ho_op), ncol = 3))
pred_ho_non <- as.data.frame(matrix(unlist(pred_ho_non), ncol = 3))

dta_test <- data.frame(bind_rows(subset(dta_full, holdout==1 & post==0 & operating==1), subset(dta_full, holdout==1 & post==0 & operating==0)),
                       fit=c(pred_ho_op$V1, pred_ho_non$V1),
                       lwr=c(pred_ho_op$V2, pred_ho_non$V2),
                       upr=c(pred_ho_op$V3, pred_ho_non$V3))
plot_ttow(dta_test, pre_test=TRUE, coord=c(12, 12))


## 4. Use new TC and op definition, but truncate TOW to the hour ----

# Random sample pre-period model vs. holdout pre
ttow_op_lm <- lm(kwh ~ tow + Tc1 + Tc2 + Tc3 + Tc4 + Tc5 + Tc6, data=subset(dta_full, holdout==0 & post==0 & operating==1))
ttow_non_lm <- lm(kwh ~ tow + temp, data=subset(dta_full, holdout==0 & post==0 & operating==0))

pred_ho_op <- predict(ttow_op_lm, subset(dta_full, holdout==1 & post==0 & operating==1), interval="confidence", level=0.95) # prediction interval also possible
pred_ho_non <- predict(ttow_non_lm, subset(dta_full, holdout==1 & post==0 & operating==0), interval="confidence", level=0.95) # prediction interval also possible
pred_ho_op <- as.data.frame(matrix(unlist(pred_ho_op), ncol = 3))
pred_ho_non <- as.data.frame(matrix(unlist(pred_ho_non), ncol = 3))

dta_test <- data.frame(bind_rows(subset(dta_full, holdout==1 & post==0 & operating==1), subset(dta_full, holdout==1 & post==0 & operating==0)),
                       fit=c(pred_ho_op$V1, pred_ho_non$V1),
                       lwr=c(pred_ho_op$V2, pred_ho_non$V2),
                       upr=c(pred_ho_op$V3, pred_ho_non$V3))
plot_ttow(dta_test, pre_test=TRUE, coord=c(12, 12))


# Look at HO error on subsets of days
dta_test <- dta_test %>% group_by(substr(readdate, 1, 10)) %>%
  mutate(avg_temp=mean(temp), avg_cdd=mean(ifelse(temp>65, temp-65, 0)), avg_hdd=mean(ifelse(temp<65, 65-temp, 0)))
plot_ttow(subset(dta_test, dow=="6"), pre_test=TRUE, coord=c(12, 14))
plot_ttow(subset(dta_test, avg_cdd>7), pre_test=TRUE, coord=c(12, 10))
plot_ttow(subset(dta_test, avg_hdd>7), pre_test=TRUE, coord=c(12, 10))
