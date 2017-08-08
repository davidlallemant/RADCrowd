#test this RadCrowd Project


rm(list=ls(all=TRUE))

require(ggplot2)

setwd("/Users/dlallemant/Documents/RadCrowd/Analysis")

Exp2results<-read.csv(file = "results_exp2a_Aug5.csv",header = T)
DamageData<-read.csv(file = "XS_AOI_with_damagedata.csv",header = T)


AllData_Exp2<-cbind(Exp2results,DamageData[sapply(Exp2results$UID, function(x) which(x==DamageData$ID)),])

fit1 <- lm(answer ~ Mean, data=AllData_Exp2)
fit2 <- lm(answer ~ CDF05, data=AllData_Exp2)
fit3 <- lm(answer ~ CDF5, data=AllData_Exp2)
fit4 <- lm(answer ~ CDF20, data=AllData_Exp2)
fit5 <- lm(answer ~ CDF45, data=AllData_Exp2)
fit6 <- lm(answer ~ CDF80, data=AllData_Exp2)
fit7 <- lm(answer ~ CDF100, data=AllData_Exp2)

anova(fit1,fit2,fit3,fit4,fit5,fit6,fit7, test="F")

summary(fit1)
summary(fit2)
summary(fit3)
summary(fit4)
summary(fit5)
summary(fit6)
summary(fit7)

ggplot(data = AllData_Exp2,aes(x=answer,y=Mean))+geom_point()+stat_smooth(method="lm")

Exp3results<-read.csv(file = "results_exp3a_Aug5.csv",header = T)
AllData_Exp3<-cbind(Exp3results,DamageData[sapply(Exp3results$UID, function(x) which(x==DamageData$ID)),])

ggplot(data = AllData_Exp3,aes(x=answer,y=Mean))+geom_point()+stat_smooth(method="lm")


#Exp 2 Identifying best contributors
unique_users<-unique(Exp2results$user_id)
User_index<-lapply(unique_users, function (x) which(x==Exp2results$user_id))

User_power<-as.numeric(lapply(User_index, function (x) lm(answer ~ Mean, data=AllData_Exp2[x,])$coefficients[2]))
user_contribution<- as.numeric(lapply(User_index,function(x) length(x)))
User_time_med<-as.numeric(lapply(User_index, function (x) median(AllData_Exp2$runtime[x])))
User_time_mean<-as.numeric(lapply(User_index, function (x) mean(AllData_Exp2$runtime[x])))
User_time_var<-as.numeric(lapply(User_index, function (x) var(AllData_Exp2$runtime[x])))

User_data_Exp2<-data.frame(user_id=unique_users, user_contribution, User_power, User_time_med,User_time_mean,User_time_var)


User_data_Exp2_sorted<-User_data_Exp2[order(User_data_Exp2$User_power, decreasing = T),]

Data_by_contributor=function(contributor_list, results_data){
  new_results_data<-results_data[0,]
  for (i in 1:length(contributor_list)){
  inds <- which(results_data$user_id %in% contributor_list[i])
  new_results_data=rbind(new_results_data,results_data[inds,])
  }
  return(new_results_data)
}

Exp2results_sorted_by_user=Data_by_contributor(User_data_Exp2_sorted$user_id,AllData_Exp2)

Rational_evaluators<-User_data_Exp2_sorted$user_id[which(User_data_Exp2_sorted$User_power>0.01)]
Results_Exp2_Rational_evaluators<-Data_by_contributor(Rational_evaluators,AllData_Exp2)
ggplot(data = Results_Exp2_Rational_evaluators,aes(x=answer,y=Mean))+geom_point()+stat_smooth(method="lm")

Dilient_evaluators<-User_data_Exp2_sorted$user_id[which(User_data_Exp2_sorted$User_time_med>10)]
Results_Exp2_Diligent_evaluators<-Data_by_contributor(Dilient_evaluators,AllData_Exp2)

baseline=ggplot()+stat_smooth(method="lm",data = AllData_Exp2,aes(x=answer,y=Mean))
baseline

#diligent=baseline+stat_smooth(method="lm",data = Results_Exp2_Diligent_evaluators,aes(x=answer,y=Mean),color="red")
#diligent

rational=baseline+stat_smooth(method="lm",data = Results_Exp2_Rational_evaluators,aes(x=answer,y=Mean), color="red")
rational

Result_weights_by_contributor<- function(contributor_list, results_data, weights){
  results_data=cbind(results_data,weights=rep(1,length(results_data$UID)))
  new_results_data<-cbind(results_data[0,])
  for (i in 1:length(contributor_list)){
    inds <- which(results_data$user_id %in% contributor_list[i])
    new_results_data=rbind(new_results_data,results_data[inds,])
    new_results_data$weights[which(new_results_data$user_id==contributor_list[i])]=weights[i]
  }
  return(new_results_data)
}


Results_Exp2_power_weighted<-Result_weights_by_contributor(User_data_Exp2_sorted$user_id,AllData_Exp2,weights=sapply(User_data_Exp2_sorted$User_power,function(x) ifelse(x>0,x,0)))

rational_weighted=rational+stat_smooth(method="lm",data = Results_Exp2_power_weighted,aes(x=answer,y=Mean, weight=weights), color="yellow3")
rational_weighted

weights=sapply(User_data_Exp2_sorted$User_power,function(x) ifelse(x>0,x,0))/User_data_Exp2_sorted$user_contribution
Results_Exp2_power_weighted_equal=Result_weights_by_contributor(User_data_Exp2_sorted$user_id,AllData_Exp2,weights=weights)

rational_weighted_equal=rational_weighted+stat_smooth(method="lm",data = Results_Exp2_power_weighted_equal,aes(x=answer,y=Mean, weight=weights), color="green")
rational_weighted_equal

