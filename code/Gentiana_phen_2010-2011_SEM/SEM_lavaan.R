#SEM with lavaan
#General SEM, one for each study year
#Using traits standardized within each population
#predation standardized within each population
#fitness relativized within each poppulation

#2010
data10 <- read.table(
  "data1_2010.txt",
  header=TRUE, sep="\t", na.strings="NA", dec=",", strip.white=TRUE)
head(data10)
str(data10)

#Standardize  reproductive traits and n_eggs (and fitness)

mean_h_shoot<-aggregate(h_shoot~LokalID, data10, mean )
mean_n_fl<-aggregate(n_fl~LokalID, data10, mean )
mean_most_adv<-aggregate(most_adv~LokalID, data10, mean )
mean_n_eggs<-aggregate(n_eggs~LokalID, data10, mean )
mean_n_intact_fruits<-aggregate(n_intact_fruits~LokalID, data10, mean )
mean_attack<-aggregate(attack~LokalID, data10, mean )


sd_h_shoot<-aggregate(h_shoot~LokalID, data10, sd )
sd_n_fl<-aggregate(n_fl~LokalID, data10, sd )
sd_most_adv<-aggregate(most_adv~LokalID, data10, sd )
sd_n_eggs<-aggregate(n_eggs~LokalID, data10, sd )
sd_n_intact_fruits<-aggregate(n_intact_fruits~LokalID, data10, sd )
sd_attack<-aggregate(attack~LokalID, data10, sd )

data10<-merge(data10,mean_h_shoot, by="LokalID")
data10<-merge(data10,mean_n_fl, by="LokalID")
data10<-merge(data10,mean_most_adv, by="LokalID")
data10<-merge(data10,mean_n_eggs, by="LokalID")
data10<-merge(data10,mean_n_intact_fruits, by="LokalID")
data10<-merge(data10,mean_attack, by="LokalID")

head(data10)
names(data10)

names(data10)[6:7]<-c("h_shoot","n_fl")
names(data10)[9:10]<-c("most_adv","n_eggs")
names(data10)[14]<-c("n_intact_fruits")
names(data10)[16]<-c("attack")
names(data10)[17:22]<-c("h_shoot_mean","n_fl_mean","most_adv_mean","n_eggs_mean","n_intact_fruits_mean","attack_mean")

data10<-merge(data10,sd_h_shoot, by="LokalID")
data10<-merge(data10,sd_n_fl, by="LokalID")
data10<-merge(data10,sd_most_adv, by="LokalID")
data10<-merge(data10,sd_n_eggs, by="LokalID")
data10<-merge(data10,sd_n_intact_fruits, by="LokalID")
data10<-merge(data10,sd_attack, by="LokalID")

names(data10)
names(data10)[6:7]<-c("h_shoot","n_fl")
names(data10)[9:10]<-c("most_adv","n_eggs")
names(data10)[14]<-c("n_intact_fruits")
names(data10)[16]<-c("attack")
names(data10)[23:28]<-c("h_shoot_sd","n_fl_sd","most_adv_sd","n_eggs_sd","n_intact_fruits_sd","attack_sd")

names(data10)

data10$h_shoot_sta<-(data10$h_shoot-data10$h_shoot_mean)/data10$h_shoot_sd
data10$n_fl_sta<-(data10$n_fl-data10$n_fl_mean)/data10$n_fl_sd
data10$most_adv_sta<-(data10$most_adv-data10$most_adv_mean)/data10$most_adv_sd
data10$n_eggs_sta<-(data10$n_eggs-data10$n_eggs_mean)/data10$n_eggs_sd
data10$n_intact_fruits_sta<-(data10$n_intact_fruits-data10$n_intact_fruits_mean)/data10$n_intact_fruits_sd
data10$attack_sta<-(data10$attack-data10$attack_mean)/data10$attack_sd


summary(data10,by="LokalID")
aggregate(h_shoot_sta~LokalID, data10, sd )
aggregate(n_fl_sta~LokalID, data10, sd )
aggregate(most_adv_sta~LokalID, data10, sd )
aggregate(n_eggs_sta~LokalID, data10, sd )
aggregate(n_intact_fruits_sta~LokalID, data10, sd )
aggregate(attack_sta~LokalID, data10, sd )

#Relativize fitness

data10$n_intact_fruits_rel<-data10$n_intact_fruits/data10$n_intact_fruits_mean
aggregate(n_intact_fruits_rel~LokalID, data10, mean )


# Model 2010, eggs

library(lavaan)

data10_pred<-subset(data10,predator==1&LokalID!="Göt016")
data10_pred$LokalID<-droplevels(data10_pred$LokalID)
data10_pred<-data10_pred[complete.cases(data10_pred[6:10]),]

#Traits and eggs standardized within populations,
#fitness relativized wthing populations
model10lavaan<-'n_eggs_sta ~ most_adv_sta + n_fl_sta + h_shoot_sta 
                n_intact_fruits_rel ~ n_fl_sta + h_shoot_sta + n_eggs_sta
                most_adv_sta ~~ n_fl_sta
                most_adv_sta ~~ h_shoot_sta
                n_fl_sta ~~ h_shoot_sta'

summary(sem(model10lavaan, data=data10_pred,std.ov=F,fixed.x=F,estimator="MLM"),
        standardized = T,fit.measures=T,rsquare=T)
#p<0.05

#All variables standardized accross populations
model10lavaan<-'n_eggs ~ most_adv + n_fl + h_shoot 
                n_intact_fruits ~  most_adv + n_fl + n_eggs
                most_adv ~~ n_fl
                most_adv ~~ h_shoot
                n_fl ~~ h_shoot'

summary(sem(model10lavaan, data=data10_pred,std.ov=F,fixed.x=F,estimator="MLM"),
        standardized = T,fit.measures=T,rsquare=T)
#p=0.074/0.062 with MLM

#All variables standardized within populations
model10lavaan<-'n_eggs_sta ~ most_adv_sta + n_fl_sta
                n_intact_fruits_sta ~ most_adv_sta + n_fl_sta + h_shoot_sta + n_eggs_sta
                most_adv_sta ~~ n_fl_sta
                most_adv_sta ~~ h_shoot_sta
                n_fl_sta ~~ h_shoot_sta'

summary(sem(model10lavaan, data=data10_pred,std.ov=F,fixed.x=F,estimator="MLM"),
        standardized = T,fit.measures=T,rsquare=T)
#p<0.360/0.418 with MLM! --> OK?
