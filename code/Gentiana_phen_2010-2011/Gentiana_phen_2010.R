#Read data
#2010
data10 <- read.table(
  "data1_2010.txt",
  header=TRUE, sep="\t", na.strings="NA", dec=",", strip.white=TRUE)
head(data10)
summary(data10)
str(data10)
plot(data10$LokalID)
table(data10$LokalID)
plot(data10$Lokal)
table(data10$Lokal)
plot(data10$LokalID,data10$Lokal)

#Standardize  reproductive traits

mean_n_intact_fruits<-aggregate(n_intact_fruits~LokalID, data10, mean )
mean_n_seeds<-aggregate(n_seeds~LokalID, data10, mean )

data10<-merge(data10,mean_n_intact_fruits, by="LokalID")
data10<-merge(data10,mean_n_seeds, by="LokalID")

head(data10)

mean_n_shoots<-aggregate(n_shoots~LokalID, data10, mean )
mean_h_shoot<-aggregate(h_shoot~LokalID, data10, mean )
mean_n_fl<-aggregate(n_fl~LokalID, data10, mean )
mean_phen_index<-aggregate(phen_index~LokalID, data10, mean )
mean_most_adv<-aggregate(most_adv~LokalID, data10, mean )

sd_n_shoots<-aggregate(n_shoots~LokalID, data10, sd )
sd_h_shoot<-aggregate(h_shoot~LokalID, data10, sd )
sd_n_fl<-aggregate(n_fl~LokalID, data10, sd )
sd_phen_index<-aggregate(phen_index~LokalID, data10, sd )
sd_most_adv<-aggregate(most_adv~LokalID, data10, sd )

data10<-merge(data10,mean_n_shoots, by="LokalID")
data10<-merge(data10,mean_h_shoot, by="LokalID")
data10<-merge(data10,mean_n_fl, by="LokalID")
data10<-merge(data10,mean_phen_index, by="LokalID")
data10<-merge(data10,mean_most_adv, by="LokalID")

head(data10)
names(data10)

names(data10)[5:9]<-c("n_shoots","h_shoot","n_fl","phen_index","most_adv")
names(data10)[14:17]<-c("n_intact_fruits","n_seeds","n_intact_fruits_mean","n_seeds_mean")
names(data10)[18:22]<-c("n_shoots_mean","h_shoot_mean","n_fl_mean","phen_index_mean","most_adv_mean")

data10<-merge(data10,sd_n_shoots, by="LokalID")
data10<-merge(data10,sd_h_shoot, by="LokalID")
data10<-merge(data10,sd_n_fl, by="LokalID")
data10<-merge(data10,sd_phen_index, by="LokalID")
data10<-merge(data10,sd_most_adv, by="LokalID")

names(data10)
names(data10)[5:9]<-c("n_shoots","h_shoot","n_fl","phen_index","most_adv")
names(data10)[23:27]<-c("n_shoots_sd","h_shoot_sd","n_fl_sd","phen_index_sd","most_adv_sd")

names(data10)

data10$n_shoots_sta<-(data10$n_shoots-data10$n_shoots_mean)/data10$n_shoots_sd
data10$h_shoot_sta<-(data10$h_shoot-data10$h_shoot_mean)/data10$h_shoot_sd
data10$n_fl_sta<-(data10$n_fl-data10$n_fl_mean)/data10$n_fl_sd
data10$phen_index_sta<-(data10$phen_index-data10$phen_index_mean)/data10$phen_index_sd
data10$most_adv_sta<-(data10$most_adv-data10$most_adv_mean)/data10$most_adv_sd

summary(data10,by="LokalID")
aggregate(n_shoots_sta~LokalID, data10, sd )
aggregate(h_shoot_sta~LokalID, data10, sd )
aggregate(n_fl_sta~LokalID, data10, sd )
aggregate(phen_index_sta~LokalID, data10, sd )
aggregate(most_adv_sta~LokalID, data10, sd )

data10$n_intact_fruits_sta<-data10$n_intact_fruits/data10$n_intact_fruits_mean
data10$n_seeds_sta<-data10$n_seeds/data10$n_seeds_mean


summary(data10,by="LokalID")
aggregate(n_intact_fruits_sta~LokalID, data10, sd )
aggregate(n_seeds_sta~LokalID, data10, sd )

names(data10)

data10sta<-data10[c(1:4,28:32,10:13,33,34)]
head(data10sta)
names(data10sta)<-c("LokalID","predator","Lokal","Individ","n_shoots","h_shoot","n_fl","phen_index","most_adv",
                    "n_eggs","n_pred_all","n_aborted_all","perc_undeveloped","n_intact_fruits","n_seeds")
summary(data10sta,by="LokalID")

#Effects of traits on fitness

cor(data10sta[5:9],use="pairwise.complete.obs") #phen_index and most_adv highly correlated

#Fitness=n_intact_fruits

modelfr_10<-lm(n_intact_fruits~phen_index+most_adv+n_fl+n_shoots+h_shoot+LokalID:phen_index+
                LokalID:most_adv+LokalID:n_fl+LokalID:n_shoots+LokalID:h_shoot,data=data10sta)
modelfr_10<-lm(n_intact_fruits~most_adv+n_fl+n_shoots+h_shoot+predator+predator:phen_index+
                 predator:n_fl+predator:n_shoots+predator:h_shoot,data=data10sta)
Anova(modelfr_10,type="II")
vif(modelfr_10)

modelfr1_10<-lm(n_intact_fruits~phen_index+n_fl+n_shoots+h_shoot+LokalID:phen_index+
                 LokalID:n_fl+LokalID:n_shoots+LokalID:h_shoot,data=data10sta)
Anova(modelfr1_10,type="II")
summary(lm(n_intact_fruits~phen_index+n_fl+n_shoots+h_shoot,data=data10sta))

modelfr2_10<-lm(n_intact_fruits~most_adv+n_fl+n_shoots+h_shoot+LokalID:most_adv+
                 LokalID:n_fl+LokalID:n_shoots+LokalID:h_shoot,data=data10sta)
Anova(modelfr2_10,type="II")
summary(lm(n_intact_fruits~most_adv+n_fl+n_shoots+h_shoot,data=data10sta))

#Fitness=n_seeds

modelseed_10<-lm(n_seeds~phen_index+most_adv+n_fl+n_shoots+h_shoot+LokalID:phen_index+
                 LokalID:most_adv+LokalID:n_fl+LokalID:n_shoots+LokalID:h_shoot,data=data10sta)
Anova(modelseed_10,type="II")
vif(modelseed_10)

modelseed1_10<-lm(n_seeds~phen_index+n_fl+n_shoots+h_shoot+LokalID:phen_index+
                  LokalID:n_fl+LokalID:n_shoots+LokalID:h_shoot,data=data10sta)
Anova(modelseed1_10,type="II")

modelseed2_10<-lm(n_seeds~most_adv+n_fl+n_shoots+h_shoot+LokalID:most_adv+
                  LokalID:n_fl+LokalID:n_shoots+LokalID:h_shoot,data=data10sta)
Anova(modelseed2_10,type="II") #phen_index performs better

anova(modelseed1_10,modelseed2_10)

#Separate models for pops with/without predator

data10sta_pred<-subset(data10sta,predator==1)
data10sta_pred$LokalID<-factor(data10sta_pred$LokalID)
data10sta_npred<-subset(data10sta,predator==0)
data10sta_npred$LokalID<-factor(data10sta_npred$LokalID)

#With predator
modelfr1_pred_10<-lm(n_intact_fruits~phen_index+n_fl+n_shoots+h_shoot+LokalID:phen_index+
                  LokalID:n_fl+LokalID:n_shoots+LokalID:h_shoot,data=data10sta_pred)
Anova(modelfr1_pred_10,type="II")
summary(modelfr1_pred_10)

modelseed1_pred_10<-lm(n_seeds~phen_index+n_fl+n_shoots+h_shoot+LokalID:phen_index+
                    LokalID:n_fl+LokalID:n_shoots+LokalID:h_shoot,data=data10sta_pred)
Anova(modelseed1_pred_10,type="II")
summary(modelseed1_pred_10)

#Without predator
modelfr1_npred_10<-lm(n_intact_fruits~phen_index+n_fl+n_shoots+h_shoot+LokalID:phen_index+
                       LokalID:n_fl+LokalID:n_shoots+LokalID:h_shoot,data=data10sta_npred)
Anova(modelfr1_npred_10,type="II")
summary(lm(n_intact_fruits~phen_index+n_fl+n_shoots+h_shoot,data=data10sta_npred))

modelseed1_npred_10<-lm(n_seeds~phen_index+n_fl+n_shoots+h_shoot+LokalID:phen_index+
                         LokalID:n_fl+LokalID:n_shoots+LokalID:h_shoot,data=data10sta_npred)
Anova(modelseed1_npred_10,type="II")
summary(modelseed1_npred_10)

#Linear selection gradients

library(lme4)
library(lattice)

#Fitness=n_intact_fruits

data10staLokalID <- split(data10sta, data10sta$LokalID)

lsel_fr_10 <- lapply(data10staLokalID, function (x) {
  lm(n_intact_fruits ~ phen_index+n_fl+n_shoots+h_shoot, 
      data = x)
})

lsel_fr_10

lselcoefs_fr_10 <- vector("list", 20) # create list

for (i in 1:20)  #this is 20 LokalID
{
  lselcoefs_fr_10[[i]]<-summary(lsel_fr_10[[i]])$coefficients
}

names(lselcoefs_fr_10)<-names(lsel_fr_10)
lselcoefs_fr_10
lselcoefs_fr_10<-do.call("rbind",lapply(lselcoefs_fr_10,FUN=data.frame))
lselcoefs_fr_10
#Add asterisks for significance
lselcoefs_fr_10$sig <- ifelse(lselcoefs_fr_10$Pr...t.. < 0.05,"*", "")
head(lselcoefs_fr_10)
write.table(lselcoefs_fr_10,file="lselcoefs_fr_10.txt",sep="\t")

#Fitness=n_seeds

lsel_seed_10 <- lapply(data10staLokalID, function (x) {
  lm(n_seeds ~ phen_index+n_fl+n_shoots+h_shoot, 
     data = x)
})

lsel_seed_10

lselcoefs_seed_10 <- vector("list", 20) # create list

for (i in 1:20)  #this is 20 LokalID
{
  lselcoefs_seed_10[[i]]<-summary(lsel_seed_10[[i]])$coefficients
}

names(lselcoefs_seed_10)<-names(lsel_seed_10)
lselcoefs_seed_10
lselcoefs_seed_10<-do.call("rbind",lapply(lselcoefs_seed_10,FUN=data.frame))
lselcoefs_seed_10
#Add asterisks for significance
lselcoefs_seed_10$sig <- ifelse(lselcoefs_seed_10$Pr...t.. < 0.05,"*", "")
head(lselcoefs_seed_10)
write.table(lselcoefs_seed_10,file="lselcoefs_seed_10.txt",sep="\t")

#Differences in linear selection gradients between populations with/without predator
lsel_grads <- read.table(
  "lsel_grads.txt",
  header=TRUE, sep="\t", na.strings="NA", dec=",", strip.white=TRUE)
head(lsel_grads)
str(lsel_grads)
lsel_grads$pred<-as.factor(lsel_grads$pred)

summary(lm(phen_index~pred,data=subset(lsel_grads,fitness_measure=="n_intact_fruits")))
plot(subset(lsel_grads,fitness_measure=="n_intact_fruits")$pred,
     subset(lsel_grads,fitness_measure=="n_intact_fruits")$phen_index,
     xlab="Predator",ylab="Selection gradient for phen_index",
     main="Fitness=n_intact_fruits")
legend("topright", legend = "p < 0.001",bty = "n")

summary(lm(phen_index~pred,data=subset(lsel_grads,fitness_measure=="n_seeds")))
plot(subset(lsel_grads,fitness_measure=="n_seeds")$pred,
     subset(lsel_grads,fitness_measure=="n_seeds")$phen_index,
     xlab="Predator",ylab="Selection gradient for phen_index",
     main="Fitness=n_seeds")
legend("topright", legend = "p = 0.0631",bty = "n")

library(Rcmdr)

plotMeans(subset(lsel_grads,fitness_measure=="n_intact_fruits")$phen_index,
          subset(lsel_grads,fitness_measure=="n_intact_fruits")$pred,
          xlab="Predator",ylab="Selection gradient for phen_index",
          main="Fitness=n_intact_fruits")
legend("topright", legend = "p < 0.001",bty = "n")

plotMeans(subset(lsel_grads,fitness_measure=="n_seeds")$phen_index,
          subset(lsel_grads,fitness_measure=="n_seeds")$pred,
          xlab="Predator",ylab="Selection gradient for phen_index",
          main="Fitness=n_seeds")
legend("topright", legend = "p = 0.0631",bty = "n")

#Relation of linear selection gradients with intensity of predation (in populations with predator)
data10_pred<-subset(data10,predator==1)
data10_pred$LokalID<-factor(data10_pred$LokalID)
data10_npred<-subset(data10,predator==0)
data10_npred$LokalID<-factor(data10_npred$LokalID)

as.data.frame(tapply(data10_pred$n_eggs, data10_pred$LokalID, mean))

data10_pred_means <- aggregate(data10_pred[,c("n_aborted_all","n_eggs","n_pred_all"), 
                     drop=FALSE],by=list(LokalID=data10_pred$LokalID), FUN=mean)

summary(lm(subset(lsel_grads,fitness_measure=="n_intact_fruits"&pred==1)$phen_index~
             data10_pred_means$n_eggs))
plot(data10_pred_means$n_eggs,
     subset(lsel_grads,fitness_measure=="n_intact_fruits"&pred==1)$phen_index,
     xlab="Mean number of eggs",ylab="Selection gradient for phen_index")
abline(lm(subset(lsel_grads,fitness_measure=="n_intact_fruits"&pred==1)$phen_index~
             data10_pred_means$n_eggs))
legend("topright", legend = "p = 0.778",bty = "n")

summary(lm(subset(lsel_grads,fitness_measure=="n_seeds"&pred==1)$phen_index~
             data10_pred_means$n_eggs))
plot(sqrt(data10_pred_means$n_eggs),subset(lsel_grads,fitness_measure=="n_seeds"&pred==1)$phen_index)

summary(lm(subset(lsel_grads,fitness_measure=="n_intact_fruits"&pred==1)$n_fl~
             data10_pred_means$n_eggs))
par(mfrow=c(1,2))
plot(data10_pred_means$n_eggs,subset(lsel_grads,fitness_measure=="n_intact_fruits"&pred==1)$n_fl,
     xlab="Mean number of eggs",ylab="Selection gradient for n_fl",
     main="Fitness=n_intact_fruits")
abline(lm(subset(lsel_grads,fitness_measure=="n_intact_fruits"&pred==1)$n_fl~ data10_pred_means$n_eggs))
legend("topright", legend = "p = 0.00339",bty = "n")

summary(lm(subset(lsel_grads,fitness_measure=="n_seeds"&pred==1)$n_fl~
             data10_pred_means$n_eggs))
plot(data10_pred_means$n_eggs,subset(lsel_grads,fitness_measure=="n_seeds"&pred==1)$n_fl,
     xlab="Mean number of eggs",ylab="Selection gradient for n_fl",
     main="Fitness=n_seeds")
abline(lm(subset(lsel_grads,fitness_measure=="n_seeds"&pred==1)$n_fl~ data10_pred_means$n_eggs))
legend("topright", legend = "p = 0.4235",bty = "n")

summary(lm(subset(lsel_grads,fitness_measure=="n_intact_fruits"&pred==1)$h_shoot~
             data10_pred_means$n_eggs))
par(mfrow=c(1,2))
plot(data10_pred_means$n_eggs,subset(lsel_grads,fitness_measure=="n_intact_fruits"&pred==1)$h_shoot,
     xlab="Mean number of eggs",ylab="Selection gradient for h_shoot",
     main="Fitness=n_intact_fruits")
abline(lm(subset(lsel_grads,fitness_measure=="n_intact_fruits"&pred==1)$h_shoot~ data10_pred_means$n_eggs))
legend("topright", legend = "p = 0.101",bty = "n")

summary(lm(subset(lsel_grads,fitness_measure=="n_seeds"&pred==1)$h_shoot~
             data10_pred_means$n_eggs))
plot(data10_pred_means$n_eggs,subset(lsel_grads,fitness_measure=="n_seeds"&pred==1)$h_shoot,
     xlab="Mean number of eggs",ylab="Selection gradient for h_shoot",
     main="Fitness=n_seeds")
abline(lm(subset(lsel_grads,fitness_measure=="n_seeds"&pred==1)$h_shoot~ data10_pred_means$n_eggs))
legend("topright", legend = "p = 0.554",bty = "n")


summary(lm(subset(lsel_grads,fitness_measure=="n_intact_fruits"&pred==1)$phen_index~
             data10_pred_means$n_aborted_all))
plot(data10_pred_means$n_aborted_all,subset(lsel_grads,fitness_measure=="n_intact_fruits"&pred==1)$phen_index,
     xlab="Mean number of aborted",ylab="Selection gradient for phen_index",
     main="Fitness=n_intact_fruits")
abline(lm(subset(lsel_grads,fitness_measure=="n_intact_fruits"&pred==1)$phen_index~
            data10_pred_means$n_aborted_all))
legend("topright", legend = "p = 0.0483",bty = "n")

summary(lm(subset(lsel_grads,fitness_measure=="n_seeds"&pred==1)$phen_index~
             data10_pred_means$n_aborted_all))
plot(data10_pred_means$n_aborted_all,subset(lsel_grads,fitness_measure=="n_seeds"&pred==1)$phen_index,
     xlab="Mean number of aborted",ylab="Selection gradient for phen_index",
     main="Fitness=n_seeds")
abline(lm(subset(lsel_grads,fitness_measure=="n_seeds"&pred==1)$phen_index~
            data10_pred_means$n_aborted_all))
legend("topright", legend = "p = 0.431",bty = "n")

summary(lm(subset(lsel_grads,fitness_measure=="n_intact_fruits"&pred==1)$n_fl~
             data10_pred_means$n_aborted_all))
par(mfrow=c(1,2))
plot(data10_pred_means$n_aborted_all,subset(lsel_grads,fitness_measure=="n_intact_fruits"&pred==1)$n_fl,
     xlab="N of aborted",ylab="Selection gradient for n_fl",
     main="Fitness=n_intact_fruits")
abline(lm(subset(lsel_grads,fitness_measure=="n_intact_fruits"&pred==1)$n_fl~ data10_pred_means$n_aborted_all))
legend("topright", legend = "p = 0.0853",bty = "n")

summary(lm(subset(lsel_grads,fitness_measure=="n_seeds"&pred==1)$n_fl~
             data10_pred_means$n_aborted_all))
plot(data10_pred_means$n_aborted_all,subset(lsel_grads,fitness_measure=="n_seeds"&pred==1)$n_fl,
     xlab="N of aborted",ylab="Selection gradient for n_fl",
     main="Fitness=n_seeds")
abline(lm(subset(lsel_grads,fitness_measure=="n_seeds"&pred==1)$n_fl~ data10_pred_means$n_aborted_all))
legend("topright", legend = "p = 0.278",bty = "n")

summary(lm(subset(lsel_grads,fitness_measure=="n_intact_fruits"&pred==1)$h_shoot~
             data10_pred_means$n_aborted_all))
par(mfrow=c(1,2))
plot(data10_pred_means$n_aborted_all,subset(lsel_grads,fitness_measure=="n_intact_fruits"&pred==1)$h_shoot,
     xlab="N of aborted",ylab="Selection gradient for h_shoot",
     main="Fitness=n_intact_fruits")
abline(lm(subset(lsel_grads,fitness_measure=="n_intact_fruits"&pred==1)$h_shoot~ data10_pred_means$n_aborted_all))
legend("topright", legend = "p = 0.724",bty = "n")

summary(lm(subset(lsel_grads,fitness_measure=="n_seeds"&pred==1)$h_shoot~
             data10_pred_means$n_aborted_all))
plot(data10_pred_means$n_aborted_all,subset(lsel_grads,fitness_measure=="n_seeds"&pred==1)$h_shoot,
     xlab="N of aborted",ylab="Selection gradient for h_shoot",
     main="Fitness=n_seeds")
abline(lm(subset(lsel_grads,fitness_measure=="n_seeds"&pred==1)$h_shoot~ data10_pred_means$n_aborted_all))
legend("topright", legend = "p = 0.591",bty = "n")

summary(lm(subset(lsel_grads,fitness_measure=="n_intact_fruits"&pred==1)$phen_index~
             data10_pred_means$n_pred_all))
plot(data10_pred_means$n_pred_all,subset(lsel_grads,fitness_measure=="n_intact_fruits"&pred==1)$phen_index,
     xlab="Mean pred_all",ylab="Selection gradient for phen_index",
     main="Fitness=n_intact_fruits")
abline(lm(subset(lsel_grads,fitness_measure=="n_intact_fruits"&pred==1)$phen_index~
            data10_pred_means$n_pred_all))
legend("topright", legend = "p = 0.703",bty = "n")

summary(lm(subset(lsel_grads,fitness_measure=="n_seeds"&pred==1)$phen_index~
             data10_pred_means$n_pred_all))
plot(data10_pred_means$n_pred_all,subset(lsel_grads,fitness_measure=="n_seeds"&pred==1)$phen_index,
     xlab="Mean pred_all",ylab="Selection gradient for phen_index",
     main="Fitness=n_seeds")
abline(lm(subset(lsel_grads,fitness_measure=="n_seeds"&pred==1)$phen_index~
            data10_pred_means$n_pred_all))
legend("topright", legend = "p = 0.506",bty = "n")

summary(lm(subset(lsel_grads,fitness_measure=="n_intact_fruits"&pred==1)$n_fl~
             data10_pred_means$n_pred_all))
par(mfrow=c(1,2))
plot(data10_pred_means$n_pred_all,subset(lsel_grads,fitness_measure=="n_intact_fruits"&pred==1)$n_fl,
     xlab="Mean pred_all",ylab="Selection gradient for n_fl",
     main="Fitness=n_intact_fruits")
abline(lm(subset(lsel_grads,fitness_measure=="n_intact_fruits"&pred==1)$n_fl~ data10_pred_means$n_pred_all))
legend("topright", legend = "p = 0.914",bty = "n")

summary(lm(subset(lsel_grads,fitness_measure=="n_seeds"&pred==1)$n_fl~
             data10_pred_means$n_pred_all))
plot(data10_pred_means$n_pred_all,subset(lsel_grads,fitness_measure=="n_seeds"&pred==1)$n_fl,
     xlab="Mean pred_all",ylab="Selection gradient for n_fl",
     main="Fitness=n_seeds")
abline(lm(subset(lsel_grads,fitness_measure=="n_seeds"&pred==1)$n_fl~ data10_pred_means$n_pred_all))
legend("topright", legend = "p = 0.455",bty = "n")

summary(lm(subset(lsel_grads,fitness_measure=="n_intact_fruits"&pred==1)$h_shoot~
             data10_pred_means$n_pred_all))
par(mfrow=c(1,2))
plot(data10_pred_means$n_pred_all,subset(lsel_grads,fitness_measure=="n_intact_fruits"&pred==1)$h_shoot,
     xlab="Mean pred_all",ylab="Selection gradient for h_shoot",
     main="Fitness=n_intact_fruits")
abline(lm(subset(lsel_grads,fitness_measure=="n_intact_fruits"&pred==1)$h_shoot~ data10_pred_means$n_pred_all))
legend("topright", legend = "p = 0.638",bty = "n")

summary(lm(subset(lsel_grads,fitness_measure=="n_seeds"&pred==1)$h_shoot~
             data10_pred_means$n_pred_all))
plot(data10_pred_means$n_pred_all,subset(lsel_grads,fitness_measure=="n_seeds"&pred==1)$h_shoot,
     xlab="Mean pred_all",ylab="Selection gradient for h_shoot",
     main="Fitness=n_seeds")
abline(lm(subset(lsel_grads,fitness_measure=="n_seeds"&pred==1)$h_shoot~ data10_pred_means$n_pred_all))
legend("topright", legend = "p = 0.584",bty = "n")

#Effects of traits on interaction intensity

int1_10<-lm(n_eggs~phen_index+n_fl+n_shoots+h_shoot+LokalID*phen_index+
                   LokalID*n_fl+LokalID*n_shoots+LokalID*h_shoot,data=subset(data10sta,predator==1))
Anova(int1_10,type="II")

int2_10<-lm(n_pred_all~phen_index+n_fl+n_shoots+h_shoot+LokalID*phen_index+
              LokalID*n_fl+LokalID*n_shoots+LokalID*h_shoot,data=subset(data10sta,predator==1))
Anova(int2_10,type="II")
summary(lm(n_pred_all~phen_index+n_fl+n_shoots+h_shoot,data=subset(data10sta,predator==1)))

int3_10<-lm(n_aborted_all~phen_index+n_fl+n_shoots+h_shoot+LokalID*phen_index+
              LokalID*n_fl+LokalID*n_shoots+LokalID*h_shoot,data=subset(data10sta,predator==1))
Anova(int3_10,type="II")

int4_10<-lm(perc_undeveloped~phen_index+n_fl+n_shoots+h_shoot+LokalID*phen_index+
              LokalID*n_fl+LokalID*n_shoots+LokalID*h_shoot,data=subset(data10sta,predator==1))
Anova(int4_10,type="II") #Very few observations!

#Effects of traits on interaction intensity: models for each population

data10sta_pred<-subset(data10sta,predator==1)
levels(data10sta_pred$LokalID)
data10sta_pred$LokalID <- factor(data10sta_pred$LokalID)
levels(data10sta_pred$LokalID)

data10sta_predLokalID <- split(data10sta_pred, data10sta_pred$LokalID)

int_1_10_pops <- lapply(data10sta_predLokalID, function (x) {
  lm(n_eggs ~ phen_index+n_fl+n_shoots+h_shoot, 
     data = x)
})

int_1_10_pops

int_1_10_pops_coefs <- vector("list", 11) # create list

for (i in 1:11)  #this is 11 LokalID
{
  int_1_10_pops_coefs[[i]]<-summary(int_1_10_pops[[i]])$coefficients
}

names(int_1_10_pops_coefs)<-names(int_1_10_pops)
int_1_10_pops_coefs
int_1_10_pops_coefs<-do.call("rbind",lapply(int_1_10_pops_coefs,FUN=data.frame))
int_1_10_pops_coefs
#Add asterisks for significance
int_1_10_pops_coefs$sig <- ifelse(int_1_10_pops_coefs$Pr...t.. < 0.05,"*", "")
head(int_1_10_pops_coefs)
write.table(int_1_10_pops_coefs,file="int_1_10_pops_coefs.txt",sep="\t")

int_2_10_pops <- lapply(data10sta_predLokalID, function (x) {
  lm(n_pred_all ~ phen_index+n_fl+n_shoots+h_shoot, 
     data = x)
})

int_2_10_pops

int_2_10_pops_coefs <- vector("list", 10) # create list

for (i in 1:10)  #this is 10 LokalID
{
  int_2_10_pops_coefs[[i]]<-summary(int_2_10_pops[[i]])$coefficients
}

names(int_2_10_pops_coefs)<-names(int_2_10_pops)
int_2_10_pops_coefs
int_2_10_pops_coefs<-do.call("rbind",lapply(int_2_10_pops_coefs,FUN=data.frame))
int_2_10_pops_coefs
#Add asterisks for significance
int_2_10_pops_coefs$sig <- ifelse(int_2_10_pops_coefs$Pr...t.. < 0.05,"*", "")
head(int_2_10_pops_coefs)
write.table(int_2_10_pops_coefs,file="int_2_10_pops_coefs.txt",sep="\t")

int_3_10_pops <- lapply(data10sta_predLokalID, function (x) {
  lm(n_aborted_all ~ phen_index+n_fl+n_shoots+h_shoot, 
     data = x)
})

int_3_10_pops

int_3_10_pops_coefs <- vector("list", 10) # create list

for (i in 1:10)  #this is 10 LokalID
{
  int_3_10_pops_coefs[[i]]<-summary(int_3_10_pops[[i]])$coefficients
}

names(int_3_10_pops_coefs)<-names(int_3_10_pops)
int_3_10_pops_coefs
int_3_10_pops_coefs<-do.call("rbind",lapply(int_3_10_pops_coefs,FUN=data.frame))
int_3_10_pops_coefs
#Add asterisks for significance
int_3_10_pops_coefs$sig <- ifelse(int_3_10_pops_coefs$Pr...t.. < 0.05,"*", "")
head(int_3_10_pops_coefs)
write.table(int_3_10_pops_coefs,file="int_3_10_pops_coefs.txt",sep="\t")

##Effects of traits on interaction intensity: graphs for each population
head(data10_pred)
str(data10_pred)

#2010

xyplot(n_eggs ~ phen_index|LokalID, data = data10_pred, 
       auto.key = list(corner = c(0, .98)), cex = 1.5)

library(ggplot2)

science_theme = theme_bw(base_family = "serif")+theme(panel.grid.major = element_line(size = 0, color = "white"),
     axis.line = element_line(size=.7, color = "black"), text = element_text(size=16),
     axis.title.x = element_text(vjust=-0.50),axis.title.y = element_text(vjust=0.70))+
     theme( plot.background = element_blank() ,panel.grid.major = element_blank() ,
     panel.grid.minor = element_blank() , panel.border = element_blank() ,
     panel.background = element_blank() ) +  theme(axis.line = element_line(color = 'black'))

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#CC6666", "#9999CC", "#66CC99")


ggplot(data10_pred, aes(x=phen_index, y=n_eggs)) + 
  geom_point(shape=20,colour="grey",size=3) +
  geom_smooth(method=lm, se=FALSE, colour="black",size=1)+ facet_wrap( ~ LokalID, ncol=4) +
  labs(x = "Phenology index", y = "Number of eggs")+
  science_theme

ggplot(data10_pred, aes(x=phen_index, y=n_eggs)) + 
  geom_smooth(method=lm, se=FALSE, colour="black",size=1)+ facet_wrap( ~ LokalID, ncol=4) +
  labs(x = "Phenology index", y = "Number of eggs")+
  science_theme

ggplot(data10_pred, aes(x=phen_index, y=n_eggs,color=LokalID)) +
  geom_smooth(method=lm, se=FALSE, size=0.7,fullrange=T)+ 
  scale_x_continuous(breaks=c(0,1,2,3,4,5,6))+
  labs(x = "Phenology index", y = "Number of eggs")+
  science_theme+scale_colour_manual(values=cbbPalette)

ggplot(data10_pred, aes(x=most_adv, y=n_eggs,color=LokalID)) +
  geom_smooth(method=lm, se=FALSE, size=0.7,fullrange=T)+ 
  scale_x_continuous(breaks=c(0,1,2,3,4,5,6))+
  labs(x = "Most advanced bud", y = "Number of eggs")+
  science_theme+scale_colour_manual(values=cbbPalette)

ggplot(data10_pred, aes(x=n_fl, y=n_eggs,color=LokalID)) +
  geom_smooth(method=lm, se=FALSE, size=0.7,fullrange=T)+ 
  labs(x = "Number of flowers", y = "Number of eggs")+
  science_theme+scale_colour_manual(values=cbbPalette)

#2011

ggplot(data11_pred, aes(x=phen_index, y=n_eggs,color=LokalID)) +
  geom_smooth(method=lm, se=FALSE, size=0.7,fullrange=T)+
  scale_x_continuous(breaks=c(0,1,2,3,4,5,6))+
  labs(x = "Phenology index", y = "Number of eggs")+
  science_theme+scale_colour_manual(values=cbbPalette)

ggplot(data11_pred, aes(x=most_adv, y=n_eggs,color=LokalID)) +
  geom_smooth(method=lm, se=FALSE, size=1,fullrange=T)+ 
  scale_x_continuous(breaks=c(0,1,2,3,4,5,6))+
  labs(x = "Most advanced bud", y = "Number of eggs")+
  science_theme+scale_colour_manual(values=cbbPalette)


#Significance?
#2010
phen10 <- lapply(data10sta_predLokalID, function (x) {
  lm(n_eggs ~ most_adv, 
     data = x)
})

phen10coefs<-vector("list", 11) # create list

for (i in 1:11)  #this is 11 LokalID
{
  phen10coefs[[i]]<-summary(phen10[[i]])$coefficients
}

phen10coefs

names(phen10coefs)<-names(phen10)
phen10coefs
phen10coefs<-do.call("rbind",lapply(phen10coefs,FUN=data.frame))
phen10coefs
#Add asterisks for significance
phen10coefs$sig <- ifelse(phen10coefs$Pr...t.. < 0.05,"*", "")
phen10coefs

#2011
phen11 <- lapply(data11sta_predLokalID, function (x) {
  lm(n_eggs ~ most_adv, 
     data = x)
})

phen11coefs<-vector("list", 11) # create list

for (i in 1:11)  #this is 11 LokalID
{
  phen11coefs[[i]]<-summary(phen11[[i]])$coefficients
}

phen11coefs

names(phen11coefs)<-names(phen11)
phen11coefs
phen11coefs<-do.call("rbind",lapply(phen11coefs,FUN=data.frame))
phen11coefs
#Add asterisks for significance
phen11coefs$sig <- ifelse(phen11coefs$Pr...t.. < 0.05,"*", "")
phen11coefs
  



  
  
  
#SEM: traits, interaction intensity, fitness --> SEE AGAIN!
library(lavaan)
library(semPlot)
sem1<-' # regressions
            n_intact_fruits_t ~ phen_index_t
            n_intact_fruits_t ~ n_eggs_t
            n_eggs_t ~ phen_index_t
'
data10_pred$n_intact_fruits_t<-sqrt(data10_pred$n_intact_fruits)
data10_pred$n_eggs_t<-sqrt(data10_pred$n_eggs)
data10_pred$phen_index_t<-sqrt(data10_pred$phen_index)


fit1 <- sem(sem1, data = data10_pred, estimator= "MLM")
summary(fit1, standardized = TRUE,fit.measures=TRUE)
semPaths(fit1,what="stand")











