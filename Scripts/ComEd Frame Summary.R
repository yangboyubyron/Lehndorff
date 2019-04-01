# ComEd Sample Frame Summary
load("/volumes/Projects/466002 - ComEd Needs Assessment/Confidential Data/Task 1/ComEd customer data anon/usage LI and rate code.RData")

frame<-w_rate %>% 
  filter(avg*12>11000|(avg*12>9500&SFMF=="MF"))

frame$e_heat[frame$e_heat=="Gas Heat"]<-"Non-Elec Heat"
frame$avg.annual<-frame$avg*12

frame.summary<-frame %>% 
  group_by(IE=LI_score>.8,SFMF,Heating=e_heat) %>% 
  summarise(
    c_9.5_11=sum(avg.annual>=9500&avg.annual<11000),
    c_11_12.5=sum(avg.annual>=11000&avg.annual<12500),
    c_12.5_14=sum(avg.annual>=12500&avg.annual<14000),
    c_g14=sum(avg.annual>=14000&avg.annual<=34000)) %>% 
  arrange(IE,desc(SFMF),Heating)

# write.csv(frame.summary,"~/desktop/ComEd Frame Summary TEMP.csv")
