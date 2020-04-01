# BPA Cleaning
library(ggplot2)
library(dplyr)

bind_for_excel<-function(x,y){
  x_out<-as.data.frame((rbind(colnames(x),as.matrix(x),"")))
  colnames(x_out)<-""
  
  y_out<-as.data.frame((rbind(colnames(y),as.matrix(y),"")))
  colnames(y_out)<-""
  
  output<-rbind(x_out,y_out)
  return(output)
  
}

sent_surveys<-readxl::read_xlsx("/volumes/Projects/420002 - BPA Impact Evaluation/TO 1 - residential HVAC analysis/Res DHP Customer survey/Contact Data/Survey Intake.xlsx") %>% 
  filter(!is.na(CustomerID))

raw_data<-read.csv("~/desktop/BPA/qualtrics_data.csv",stringsAsFactors = FALSE)

col_desc<-raw_data[1,] 
data_fields<-data_frame(`Question_Number`=colnames(col_desc),`Question`=as.character(as.vector(col_desc)))
write.csv(data_fields,"/volumes/Projects/420002 - BPA Impact Evaluation/TO 1 - Residential HVAC analysis/Res DHP customer survey/Survey_Fields.csv",row.names = FALSE)

survey_data<-raw_data %>% filter(!is.na(as.numeric(Duration..in.seconds.))) %>% filter(ResponseId!="R_2Sug1qiN2K40Jhi")
survey_data$Q59_1_TEXT[survey_data$ResponseId=="R_cIojC5XnBTsvfTX"]<-2687
write.csv(survey_data,"/volumes/Projects/420002 - BPA Impact Evaluation/TO 1 - Residential HVAC analysis/Res DHP customer survey/Survey_Data.csv")

survey_data<-read.csv("/volumes/Projects/420002 - BPA Impact Evaluation/TO 1 - Residential HVAC analysis/Res DHP customer survey/Survey_Data.csv",stringsAsFactors = FALSE)
table(survey_data$Q59_1_TEXT%in%sent_surveys$CustomerID,exclude = NULL)
table(sent_surveys$CustomerID%in%survey_data$Q59_1_TEXT,as.numeric(sent_surveys$Response),exclude = NULL)

survey_weights<-read.csv("/volumes/Projects/420002 - BPA Impact Evaluation/TO 1 - residential HVAC analysis/Res DHP Customer survey/Contact Data/survey_weights.csv",stringsAsFactors = FALSE)
table(survey_data$Q59_1_TEXT%in%survey_weights$CustomerID)

data_utility<-left_join(survey_data,survey_weights,by=c("Q59_1_TEXT"="CustomerID"))
# write.csv(data_utility,"/volumes/Projects/420002 - BPA Impact Evaluation/TO 1 - Residential HVAC analysis/Res DHP customer survey/Survey_data_weight.csv",row.names=FALSE)

model_results<-read.csv("/volumes/Projects/420002 - BPA Impact Evaluation/TO 1 - Residential HVAC analysis/Res DHP customer survey/BPA DHP Usage Data.csv")

for_excel<-data.frame(Question="",Count=NA)

for(i in colnames(survey_data)){
  if(i %in% colnames(survey_data)[1:18]){next}
  
  data<-survey_data %>% select(i)
  if(n_distinct(data)>7){next}
  
  wo_blanks<-data[data!=""]
  if(length(wo_blanks)==0){next}
  variety<-max(table(wo_blanks)/length(wo_blanks))<.85
  is.interesting<-length(wo_blanks)>10&variety
  
  if(is.interesting){
    data[data==""]<-"BLANK"
    print(paste(i,col_desc[1,i]))
    print(table(data))
    colnames(data)<-"column"
    out<-data %>% group_by(column) %>% summarise(count=n())
    colnames(out)<-c(paste(i,col_desc[1,i]))
    
    for_excel<-bind_for_excel(for_excel,out)
    
  } else {
    next
  }
  
}

colnames(for_excel)<-c("Questions", "Counts")

excel_out<-for_excel %>% filter(!(is.na(Counts)&Questions==""))
excel_out$Counts[is.na(excel_out$Counts)]<-""

write.csv(excel_out,"~/desktop/Initial Results.csv",row.names = FALSE)

savings_list<-c("Q7", "Q11", "Q13","Q18","Q19","Q22","Q23","Q24_1","Q24_2","Q24_3","Q24_4","Q26","Q28","Q30","Q31","Q35","Q40","Q41","Q42","Q43")
table(savings_list%in%colnames(col_desc))

NEB_list<-c("Q44","Q45_2")
table(NEB_list%in%colnames(col_desc))

demo_list<-c("Q51","Q52","Q53","Q54","Q57_2")
table(demo_list%in%colnames(col_desc))

indep_list<-c("Q11", "Q13","Q18","Q19","Q22","Q23","Q24_1","Q24_2","Q24_3","Q24_4","Q26","Q28","Q30","Q31","Q35","Q40","Q41","Q51","Q52","Q53","Q54","Q57_2")
dep_list<-c("Q42","Q43")

for(i in indep_list){
  for(j in dep_list){
    dep_var<-col_desc[1,j]
    indep_var<-col_desc[1,i]
    
    plot_dat<-survey_data %>% select(all_of(i),all_of(j))
    colnames(plot_dat)<-c("indep","dep")
    plot_dat$dep[plot_dat$dep==""]<-"BLANK"
    plot_dat$dep<-gsub(" energy bill","",plot_dat$dep)
    plot_dat$dep<-factor(plot_dat$dep,levels = rev(c("Much higher","Slightly higher","About the same","Slightly lower","Much lower","BLANK")))
    
    var_plot<-ggplot(plot_dat %>% filter(indep!=""))+
      geom_bar(aes(x=dep,fill=dep))+
      facet_grid(indep~.,scales = "free_y")+
      scale_fill_manual(values = c("black","dark blue","blue","gray70","pink","red"))+
      theme(
        strip.text.y = element_text(angle = 0),
        legend.position = "bottom")+
      labs(title=paste(i,indep_var),fill=NULL,x=dep_var)
    ggsave(var_plot,file=paste0("~/desktop/Survey Plots/",j,"_",i,".jpg"),width=8,height=6,device = "jpeg")
    
     
  }
}

model_survey<-inner_join(model_results,survey_data,by=c("CustomerID"="Q59_1_TEXT")) %>% 
  select(CustomerID:vbdd_savings,all_of(c(indep_list,dep_list,NEB_list)))

# savings v usage change


test_model<-lm(vbdd_savings ~ Q22  + Q23 +Q30  +Q40 +  Q41   + Q52  + Q54  + Q57_2 + Q43  + Q45_2,model_survey)
summary(test_model)

ggplot(model_survey)+
  geom_histogram(aes(x=vbdd_savings),binwidth = 5)+
  facet_grid(Q22~.)+
  theme(strip.text.y = element_text(angle = 0))

test<-survey_data %>% select(a="Q42",b="Q28")
test$a[test$a==""]<-"BLANK"
test$a<-factor(test$a,levels = rev(c("Much higher energy bill","Slightly higher energy bill","About the same","Slightly lower energy bill","Much lower energy bill","BLANK")))

table(test$b,test$a)

ggplot(test %>% filter(b!=""))+
  geom_bar(aes(x=a,fill=a))+
  facet_grid(b~.,scales = "free_y")+
  scale_fill_manual(values = c("black","dark blue","blue","gray70","pink","red"))+
  theme(strip.text.y = element_text(angle = 0))+
  labs(title=paste(i,col_desc[1,i]))
