#Read data
#2011
data11 <- read.table(
  "data1_2011.txt",
  header=TRUE, sep="\t", na.strings="NA", dec=",", strip.white=TRUE)
head(data11)
summary(data11)
str(data11)
plot(data11$LokalID)
table(data11$LokalID)
plot(data11$Lokal)
table(data11$Lokal)
plot(data11$LokalID,data11$Lokal)

#Standardize  reproductive traits

mean_n_intact_fruits<-aggregate(n_intact_fruits~LokalID, data11, mean )
mean_n_seeds<-aggregate(n_seeds~LokalID, data11, mean )

data11<-merge(data11,mean_n_intact_fruits, by="LokalID")
data11<-merge(data11,mean_n_seeds, by="LokalID")

head(data11)

mean_n_shoots<-aggregate(n_shoots~LokalID, data11, mean )
mean_h_shoot<-aggregate(h_shoot~LokalID, data11, mean )
mean_n_fl<-aggregate(n_fl~LokalID, data11, mean )
mean_phen_index<-aggregate(phen_index~LokalID, data11, mean )
mean_most_adv<-aggregate(most_adv~LokalID, data11, mean )

sd_n_shoots<-aggregate(n_shoots~LokalID, data11, sd )
sd_h_shoot<-aggregate(h_shoot~LokalID, data11, sd )
sd_n_fl<-aggregate(n_fl~LokalID, data11, sd )
sd_phen_index<-aggregate(phen_index~LokalID, data11, sd )
sd_most_adv<-aggregate(most_adv~LokalID, data11, sd )

data11<-merge(data11,mean_n_shoots, by="LokalID")
data11<-merge(data11,mean_h_shoot, by="LokalID")
data11<-merge(data11,mean_n_fl, by="LokalID")
data11<-merge(data11,mean_phen_index, by="LokalID")
data11<-merge(data11,mean_most_adv, by="LokalID")

head(data11)
names(data11)

names(data11)[5:9]<-c("n_shoots","h_shoot","n_fl","phen_index","most_adv")
names(data11)[14:17]<-c("n_intact_fruits","n_seeds","n_intact_fruits_mean","n_seeds_mean")
names(data11)[18:22]<-c("n_shoots_mean","h_shoot_mean","n_fl_mean","phen_index_mean","most_adv_mean")

data11<-merge(data11,sd_n_shoots, by="LokalID")
data11<-merge(data11,sd_h_shoot, by="LokalID")
data11<-merge(data11,sd_n_fl, by="LokalID")
data11<-merge(data11,sd_phen_index, by="LokalID")
data11<-merge(data11,sd_most_adv, by="LokalID")

names(data11)
names(data11)[5:9]<-c("n_shoots","h_shoot","n_fl","phen_index","most_adv")
names(data11)[23:27]<-c("n_shoots_sd","h_shoot_sd","n_fl_sd","phen_index_sd","most_adv_sd")

names(data11)

data11$n_shoots_sta<-(data11$n_shoots-data11$n_shoots_mean)/data11$n_shoots_sd
data11$h_shoot_sta<-(data11$h_shoot-data11$h_shoot_mean)/data11$h_shoot_sd
data11$n_fl_sta<-(data11$n_fl-data11$n_fl_mean)/data11$n_fl_sd
data11$phen_index_sta<-(data11$phen_index-data11$phen_index_mean)/data11$phen_index_sd
data11$most_adv_sta<-(data11$most_adv-data11$most_adv_mean)/data11$most_adv_sd

summary(data11,by="LokalID")
aggregate(n_shoots_sta~LokalID, data11, sd )
aggregate(h_shoot_sta~LokalID, data11, sd )
aggregate(n_fl_sta~LokalID, data11, sd )
aggregate(phen_index_sta~LokalID, data11, sd )
aggregate(most_adv_sta~LokalID, data11, sd )

data11$n_intact_fruits_sta<-data11$n_intact_fruits/data11$n_intact_fruits_mean
data11$n_seeds_sta<-data11$n_seeds/data11$n_seeds_mean


summary(data11,by="LokalID")
aggregate(n_intact_fruits_sta~LokalID, data11, sd )
aggregate(n_seeds_sta~LokalID, data11, sd )

names(data11)

data11sta<-data11[c(1:4,28:32,10:13,33,34)]
head(data11sta)
names(data11sta)<-c("LokalID","predator","Lokal","Individ","n_shoots","h_shoot","n_fl","phen_index","most_adv",
                    "n_eggs","n_pred_all","n_aborted_all","perc_undeveloped","n_intact_fruits","n_seeds")
summary(data11sta,by="LokalID")

#Effects of traits on fitness

cor(data11sta[5:9],use="pairwise.complete.obs") #phen_index and most_adv highly correlated

#Fitness=n_intact_fruits

modelfr_11<-lm(n_intact_fruits~phen_index+most_adv+n_fl+n_shoots+h_shoot+LokalID:phen_index+
                 LokalID:most_adv+LokalID:n_fl+LokalID:n_shoots+LokalID:h_shoot,data=data11sta)
library(car)
Anova(modelfr_11,type="II")
vif(modelfr_11)

modelfr1_11<-lm(n_intact_fruits~phen_index+n_fl+n_shoots+h_shoot+LokalID:phen_index+
                  LokalID:n_fl+LokalID:n_shoots+LokalID:h_shoot,data=data11sta)
Anova(modelfr1_11,type="II")

modelfr2_11<-lm(n_intact_fruits~most_adv+n_fl+n_shoots+h_shoot+LokalID:most_adv+
                  LokalID:n_fl+LokalID:n_shoots+LokalID:h_shoot,data=data11sta)
Anova(modelfr2_11,type="II")

#Fitness=n_seeds

modelseed_11<-lm(n_seeds~phen_index+most_adv+n_fl+n_shoots+h_shoot+LokalID:phen_index+
                   LokalID:most_adv+LokalID:n_fl+LokalID:n_shoots+LokalID:h_shoot,data=data11sta)
Anova(modelseed_11,type="II")
vif(modelseed_11)

modelseed1_11<-lm(n_seeds~phen_index+n_fl+n_shoots+h_shoot+LokalID:phen_index+
                    LokalID:n_fl+LokalID:n_shoots+LokalID:h_shoot,data=data11sta)
Anova(modelseed1_11,type="II")
summary(lm(n_seeds~phen_index+n_fl+n_shoots+h_shoot,data=data11sta))

modelseed2_11<-lm(n_seeds~most_adv+n_fl+n_shoots+h_shoot+LokalID:most_adv+
                    LokalID:n_fl+LokalID:n_shoots+LokalID:h_shoot,data=data11sta)
Anova(modelseed2_11,type="II") 
summary(lm(n_seeds~most_adv+n_fl+n_shoots+h_shoot,data=data11sta))


#Separate models for pops with/without predator

data11sta_pred<-subset(data11sta,predator==1)
data11sta_pred$LokalID<-factor(data11sta_pred$LokalID)
data11sta_npred<-subset(data11sta,predator==0)
data11sta_npred$LokalID<-factor(data11sta_npred$LokalID)

#With predator
modelfr1_pred_11<-lm(n_intact_fruits~phen_index+n_fl+n_shoots+h_shoot+LokalID:phen_index+
                       LokalID:n_fl+LokalID:n_shoots+LokalID:h_shoot,data=data11sta_pred)
Anova(modelfr1_pred_11,type="II")
summary(modelfr1_pred_11)

modelseed1_pred_11<-lm(n_seeds~phen_index+n_fl+n_shoots+h_shoot+LokalID:phen_index+
                         LokalID:n_fl+LokalID:n_shoots+LokalID:h_shoot,data=data11sta_pred)
Anova(modelseed1_pred_11,type="II")
summary(modelseed1_pred_11)

#Without predator
modelfr1_npred_11<-lm(n_intact_fruits~phen_index+n_fl+n_shoots+h_shoot+LokalID:phen_index+
                        LokalID:n_fl+LokalID:n_shoots+LokalID:h_shoot,data=data11sta_npred)
Anova(modelfr1_npred_11,type="II")
summary(modelfr1_npred_11)

modelseed1_npred_11<-lm(n_seeds~phen_index+n_fl+n_shoots+h_shoot+LokalID:phen_index+
                          LokalID:n_fl+LokalID:n_shoots+LokalID:h_shoot,data=data11sta_npred)
Anova(modelseed1_npred_11,type="II")
summary(modelseed1_npred_11)

#Linear selection gradients

library(lme4)
library(lattice)

#Fitness=n_intact_fruits

data11staLokalID <- split(data11sta, data11sta$LokalID)

lsel_fr_11 <- lapply(data11staLokalID, function (x) {
  lm(n_intact_fruits ~ phen_index+n_fl+n_shoots+h_shoot, 
     data = x)
})

lsel_fr_11

lselcoefs_fr_11 <- vector("list", 16) # create list

for (i in 1:16)  #this is 16 LokalID
{
  lselcoefs_fr_11[[i]]<-summary(lsel_fr_11[[i]])$coefficients
}

names(lselcoefs_fr_11)<-names(lsel_fr_11)
lselcoefs_fr_11
lselcoefs_fr_11<-do.call("rbind",lapply(lselcoefs_fr_11,FUN=data.frame))
lselcoefs_fr_11
#Add asterisks for significance
lselcoefs_fr_11$sig <- ifelse(lselcoefs_fr_11$Pr...t.. < 0.05,"*", "")
head(lselcoefs_fr_11)
write.table(lselcoefs_fr_11,file="lselcoefs_fr_11.txt",sep="\t")

#Fitness=n_seeds

lsel_seed_11 <- lapply(data11staLokalID, function (x) {
  lm(n_seeds ~ phen_index+n_fl+n_shoots+h_shoot, 
     data = x)
})

lsel_seed_11
summary(lsel_seed_11$Bor012)

hist(subset(data11,LokalID="Bor012")$n_fl)
hist(subset(data11,LokalID="Ale010")$n_fl)



lselcoefs_seed_11 <- vector("list", 16) # create list

for (i in 1:16)  #this is 16 LokalID
{
  lselcoefs_seed_11[[i]]<-summary(lsel_seed_11[[i]])$coefficients
}

names(lselcoefs_seed_11)<-names(lsel_seed_11)
lselcoefs_seed_11
lselcoefs_seed_11<-do.call("rbind",lapply(lselcoefs_seed_11,FUN=data.frame))
lselcoefs_seed_11
#Add asterisks for significance
lselcoefs_seed_11$sig <- ifelse(lselcoefs_seed_11$Pr...t.. < 0.05,"*", "")
head(lselcoefs_seed_11)
write.table(lselcoefs_seed_11,file="lselcoefs_seed_11.txt",sep="\t")

#Differences in linear selection gradients between populations with/without predator
lsel_grads <- read.table(
  "lsel_grads_11.txt",
  header=TRUE, sep="\t", na.strings="NA", dec=",", strip.white=TRUE)
head(lsel_grads)
str(lsel_grads)
lsel_grads$pred<-as.factor(lsel_grads$pred)

par(mfrow=c(1,2))

summary(lm(phen_index~pred,data=subset(lsel_grads,fitness_measure=="n_intact_fruits")))
plot(subset(lsel_grads,fitness_measure=="n_intact_fruits")$pred,
     subset(lsel_grads,fitness_measure=="n_intact_fruits")$phen_index,
     xlab="Predator",ylab="Selection gradient for phen_index",
     main="Fitness=n_intact_fruits")
legend("topright", legend = "p = 0.006",bty = "n")

summary(lm(phen_index~pred,data=subset(lsel_grads,fitness_measure=="n_seeds")))
plot(subset(lsel_grads,fitness_measure=="n_seeds")$pred,
     subset(lsel_grads,fitness_measure=="n_seeds")$phen_index,
     xlab="Predator",ylab="Selection gradient for phen_index",
     main="Fitness=n_seeds")
legend("topright", legend = "p = 0.679",bty = "n")

library(Rcmdr)

plotMeans(subset(lsel_grads,fitness_measure=="n_intact_fruits")$phen_index,
          subset(lsel_grads,fitness_measure=="n_intact_fruits")$pred,
          xlab="Predator",ylab="Selection gradient for phen_index",
          main="Fitness=n_intact_fruits")
legend("topright", legend = "p = 0.006",bty = "n")

plotMeans(subset(lsel_grads,fitness_measure=="n_seeds")$phen_index,
          subset(lsel_grads,fitness_measure=="n_seeds")$pred,
          xlab="Predator",ylab="Selection gradient for phen_index",
          main="Fitness=n_seeds")
legend("topright", legend = "p = 0.679",bty = "n")

summary(lm(h_shoot~pred,data=subset(lsel_grads,fitness_measure=="n_intact_fruits")))
plot(subset(lsel_grads,fitness_measure=="n_intact_fruits")$pred,
     subset(lsel_grads,fitness_measure=="n_intact_fruits")$h_shoot,
     xlab="Predator",ylab="Selection gradient for h_shoot",
     main="Fitness=n_intact_fruits")
legend("topright", legend = "p = 0.0318",bty = "n")

summary(lm(h_shoot~pred,data=subset(lsel_grads,fitness_measure=="n_seeds")))
plot(subset(lsel_grads,fitness_measure=="n_seeds")$pred,
     subset(lsel_grads,fitness_measure=="n_seeds")$h_shoot,
     xlab="Predator",ylab="Selection gradient for h_shoot",
     main="Fitness=n_seeds")
legend("topright", legend = "p = 0.243",bty = "n")

plotMeans(subset(lsel_grads,fitness_measure=="n_intact_fruits")$h_shoot,
          subset(lsel_grads,fitness_measure=="n_intact_fruits")$pred,
          xlab="Predator",ylab="Selection gradient for h_shoot",
          main="Fitness=n_intact_fruits")
legend("topright", legend = "p = 0.0318",bty = "n")

plotMeans(subset(lsel_grads,fitness_measure=="n_seeds")$h_shoot,
          subset(lsel_grads,fitness_measure=="n_seeds")$pred,
          xlab="Predator",ylab="Selection gradient for h_shoot",
          main="Fitness=n_seeds")
legend("topright", legend = "p = 0.243",bty = "n")

#Relation of linear selection gradients with intensity of predation (in populations with predator)
data11_pred<-subset(data11,Predator==1)
data11_pred$LokalID<-factor(data11_pred$LokalID)
data11_npred<-subset(data11,Predator==0)
data11_npred$LokalID<-factor(data11_npred$LokalID)

as.data.frame(tapply(data11_pred$n_eggs, data11_pred$LokalID, mean))

data11_pred_means <- aggregate(data11_pred[,c("n_aborted_all","n_eggs","n_pred_all"), 
                                           drop=FALSE],by=list(LokalID=data11_pred$LokalID), FUN=mean)

summary(lm(subset(lsel_grads,fitness_measure=="n_intact_fruits"&pred==1)$phen_index~
             data11_pred_means$n_eggs))
plot(data11_pred_means$n_eggs,
     subset(lsel_grads,fitness_measure=="n_intact_fruits"&pred==1)$phen_index,
     xlab="Mean number of eggs", ylab="Selection gradient for phen_index")
abline(lm(subset(lsel_grads,fitness_measure=="n_intact_fruits"&pred==1)$phen_index~
             data11_pred_means$n_eggs))
legend("topright", legend = "p = 0.258",bty = "n")

summary(lm(subset(lsel_grads,fitness_measure=="n_seeds"&pred==1)$phen_index~
             data11_pred_means$n_eggs))
plot(sqrt(data11_pred_means$n_eggs),subset(lsel_grads,fitness_measure=="n_seeds"&pred==1)$phen_index)

summary(lm(subset(lsel_grads,fitness_measure=="n_intact_fruits"&pred==1)$n_fl~
             data11_pred_means$n_eggs))

plot(data11_pred_means$n_eggs,subset(lsel_grads,fitness_measure=="n_intact_fruits"&pred==1)$n_fl,
     xlab="Mean number of eggs",ylab="Selection gradient for n_fl",
     main="Fitness=n_intact_fruits")
abline(lm(subset(lsel_grads,fitness_measure=="n_intact_fruits"&pred==1)$n_fl~ data11_pred_means$n_eggs))
legend("topright", legend = "p = 0.09327",bty = "n")

summary(lm(subset(lsel_grads,fitness_measure=="n_seeds"&pred==1)$n_fl~
             data11_pred_means$n_eggs))
plot(data11_pred_means$n_eggs,subset(lsel_grads,fitness_measure=="n_seeds"&pred==1)$n_fl,
     xlab="Mean number of eggs",ylab="Selection gradient for n_fl",
     main="Fitness=n_seeds")
abline(lm(subset(lsel_grads,fitness_measure=="n_seeds"&pred==1)$n_fl~ data11_pred_means$n_eggs))
legend("topright", legend = "p = 0.2845",bty = "n")

summary(lm(subset(lsel_grads,fitness_measure=="n_intact_fruits"&pred==1)$h_shoot~
             data11_pred_means$n_eggs))
par(mfrow=c(1,2))
plot(data11_pred_means$n_eggs,subset(lsel_grads,fitness_measure=="n_intact_fruits"&pred==1)$h_shoot,
     xlab="Mean number of eggs",ylab="Selection gradient for h_shoot",
     main="Fitness=n_intact_fruits")
abline(lm(subset(lsel_grads,fitness_measure=="n_intact_fruits"&pred==1)$h_shoot~ data11_pred_means$n_eggs))

summary(lm(subset(lsel_grads,fitness_measure=="n_seeds"&pred==1)$h_shoot~
             data11_pred_means$n_eggs))
plot(data11_pred_means$n_eggs,subset(lsel_grads,fitness_measure=="n_seeds"&pred==1)$h_shoot,
     xlab="Mean number of eggs",ylab="Selection gradient for h_shoot",
     main="Fitness=n_seeds")
abline(lm(subset(lsel_grads,fitness_measure=="n_seeds"&pred==1)$h_shoot~ data11_pred_means$n_eggs))

summary(lm(subset(lsel_grads,fitness_measure=="n_intact_fruits"&pred==1)$phen_index~
             data11_pred_means$n_aborted_all))
plot(data11_pred_means$n_aborted_all,subset(lsel_grads,fitness_measure=="n_intact_fruits"&pred==1)$phen_index,
     xlab="Mean number of aborted",ylab="Selection gradient for phen_index",
     main="Fitness=n_intact_fruits")
abline(lm(subset(lsel_grads,fitness_measure=="n_intact_fruits"&pred==1)$phen_index~
            data11_pred_means$n_aborted_all))

summary(lm(subset(lsel_grads,fitness_measure=="n_seeds"&pred==1)$phen_index~
             data11_pred_means$n_aborted_all))
plot(data11_pred_means$n_aborted_all,subset(lsel_grads,fitness_measure=="n_seeds"&pred==1)$phen_index,
     xlab="Mean number of aborted",ylab="Selection gradient for phen_index",
     main="Fitness=n_seeds")
abline(lm(subset(lsel_grads,fitness_measure=="n_seeds"&pred==1)$phen_index~
            data11_pred_means$n_aborted_all))

summary(lm(subset(lsel_grads,fitness_measure=="n_intact_fruits"&pred==1)$n_fl~
             data11_pred_means$n_aborted_all))
par(mfrow=c(1,2))
plot(data11_pred_means$n_aborted_all,subset(lsel_grads,fitness_measure=="n_intact_fruits"&pred==1)$n_fl,
     xlab="N of aborted",ylab="Selection gradient for n_fl",
     main="Fitness=n_intact_fruits")
abline(lm(subset(lsel_grads,fitness_measure=="n_intact_fruits"&pred==1)$n_fl~ data11_pred_means$n_aborted_all))

summary(lm(subset(lsel_grads,fitness_measure=="n_seeds"&pred==1)$n_fl~
             data11_pred_means$n_aborted_all))
plot(data11_pred_means$n_aborted_all,subset(lsel_grads,fitness_measure=="n_seeds"&pred==1)$n_fl,
     xlab="N of aborted",ylab="Selection gradient for n_fl",
     main="Fitness=n_seeds")
abline(lm(subset(lsel_grads,fitness_measure=="n_seeds"&pred==1)$n_fl~ data11_pred_means$n_aborted_all))

summary(lm(subset(lsel_grads,fitness_measure=="n_intact_fruits"&pred==1)$h_shoot~
             data11_pred_means$n_aborted_all))
par(mfrow=c(1,2))
plot(data11_pred_means$n_aborted_all,subset(lsel_grads,fitness_measure=="n_intact_fruits"&pred==1)$h_shoot,
     xlab="N of aborted",ylab="Selection gradient for h_shoot",
     main="Fitness=n_intact_fruits")
abline(lm(subset(lsel_grads,fitness_measure=="n_intact_fruits"&pred==1)$h_shoot~ data11_pred_means$n_aborted_all))

summary(lm(subset(lsel_grads,fitness_measure=="n_seeds"&pred==1)$h_shoot~
             data11_pred_means$n_aborted_all))
plot(data11_pred_means$n_aborted_all,subset(lsel_grads,fitness_measure=="n_seeds"&pred==1)$h_shoot,
     xlab="N of aborted",ylab="Selection gradient for h_shoot",
     main="Fitness=n_seeds")
abline(lm(subset(lsel_grads,fitness_measure=="n_seeds"&pred==1)$h_shoot~ data11_pred_means$n_aborted_all))

summary(lm(subset(lsel_grads,fitness_measure=="n_intact_fruits"&pred==1)$phen_index~
             data11_pred_means$n_pred_all))
plot(data11_pred_means$n_pred_all,subset(lsel_grads,fitness_measure=="n_intact_fruits"&pred==1)$phen_index,
     xlab="Mean pred_all",ylab="Selection gradient for phen_index",
     main="Fitness=n_intact_fruits")
abline(lm(subset(lsel_grads,fitness_measure=="n_intact_fruits"&pred==1)$phen_index~
            data11_pred_means$n_pred_all))

summary(lm(subset(lsel_grads,fitness_measure=="n_seeds"&pred==1)$phen_index~
             data11_pred_means$n_pred_all))
plot(data11_pred_means$n_pred_all,subset(lsel_grads,fitness_measure=="n_seeds"&pred==1)$phen_index,
     xlab="Mean pred_all",ylab="Selection gradient for phen_index",
     main="Fitness=n_seeds")
abline(lm(subset(lsel_grads,fitness_measure=="n_seeds"&pred==1)$phen_index~
            data11_pred_means$n_pred_all))

summary(lm(subset(lsel_grads,fitness_measure=="n_intact_fruits"&pred==1)$n_fl~
             data11_pred_means$n_pred_all))
par(mfrow=c(1,2))
plot(data11_pred_means$n_pred_all,subset(lsel_grads,fitness_measure=="n_intact_fruits"&pred==1)$n_fl,
     xlab="Mean pred_all",ylab="Selection gradient for n_fl",
     main="Fitness=n_intact_fruits")
abline(lm(subset(lsel_grads,fitness_measure=="n_intact_fruits"&pred==1)$n_fl~ data11_pred_means$n_pred_all))

summary(lm(subset(lsel_grads,fitness_measure=="n_seeds"&pred==1)$n_fl~
             data11_pred_means$n_pred_all))
plot(data11_pred_means$n_pred_all,subset(lsel_grads,fitness_measure=="n_seeds"&pred==1)$n_fl,
     xlab="Mean pred_all",ylab="Selection gradient for n_fl",
     main="Fitness=n_seeds")
abline(lm(subset(lsel_grads,fitness_measure=="n_seeds"&pred==1)$n_fl~ data11_pred_means$n_pred_all))

summary(lm(subset(lsel_grads,fitness_measure=="n_intact_fruits"&pred==1)$h_shoot~
             data11_pred_means$n_pred_all))
par(mfrow=c(1,2))
plot(data11_pred_means$n_pred_all,subset(lsel_grads,fitness_measure=="n_intact_fruits"&pred==1)$h_shoot,
     xlab="Mean pred_all",ylab="Selection gradient for h_shoot",
     main="Fitness=n_intact_fruits")
abline(lm(subset(lsel_grads,fitness_measure=="n_intact_fruits"&pred==1)$h_shoot~ data11_pred_means$n_pred_all))

summary(lm(subset(lsel_grads,fitness_measure=="n_seeds"&pred==1)$h_shoot~
             data11_pred_means$n_pred_all))
plot(data11_pred_means$n_pred_all,subset(lsel_grads,fitness_measure=="n_seeds"&pred==1)$h_shoot,
     xlab="Mean pred_all",ylab="Selection gradient for h_shoot",
     main="Fitness=n_seeds")
abline(lm(subset(lsel_grads,fitness_measure=="n_seeds"&pred==1)$h_shoot~ data11_pred_means$n_pred_all))

#Effects of traits on interaction intensity

int1_11<-lm(n_eggs~phen_index+n_fl+n_shoots+h_shoot+LokalID*phen_index+
              LokalID*n_fl+LokalID*n_shoots+LokalID*h_shoot,data=subset(data11sta,predator==1))
Anova(int1_11,type="II")

int2_11<-lm(n_pred_all~phen_index+n_fl+n_shoots+h_shoot+LokalID*phen_index+
              LokalID*n_fl+LokalID*n_shoots+LokalID*h_shoot,data=subset(data11sta,predator==1))
Anova(int2_11,type="II")
summary(lm(n_pred_all~phen_index+n_fl+n_shoots+h_shoot,data=subset(data11sta,predator==1)))

int3_11<-lm(n_aborted_all~phen_index+n_fl+n_shoots+h_shoot+LokalID*phen_index+
              LokalID*n_fl+LokalID*n_shoots+LokalID*h_shoot,data=subset(data11sta,predator==1))
Anova(int3_11,type="II")
summary(lm(n_aborted_all~phen_index+n_fl+n_shoots+h_shoot,data=subset(data11sta,predator==1)))

int4_11<-lm(perc_undeveloped~phen_index+n_fl+n_shoots+h_shoot+LokalID*phen_index+
              LokalID*n_fl+LokalID*n_shoots+LokalID*h_shoot,data=subset(data11sta,predator==1))
Anova(int4_11,type="II") #Very few observations!

#Effects of traits on interaction intensity: models for each population

data11sta_pred<-subset(data11sta,predator==1)
levels(data11sta_pred$LokalID)
data11sta_pred$LokalID <- factor(data11sta_pred$LokalID)
levels(data11sta_pred$LokalID)

data11sta_predLokalID <- split(data11sta_pred, data11sta_pred$LokalID)

int_1_11_pops <- lapply(data11sta_predLokalID, function (x) {
  lm(n_eggs ~ phen_index+n_fl+n_shoots+h_shoot, 
     data = x)
})

int_1_11_pops

int_1_11_pops_coefs <- vector("list", 11) # create list

for (i in 1:11)  #this is 11 LokalID
{
  int_1_11_pops_coefs[[i]]<-summary(int_1_11_pops[[i]])$coefficients
}

names(int_1_11_pops_coefs)<-names(int_1_11_pops)
int_1_11_pops_coefs
int_1_11_pops_coefs<-do.call("rbind",lapply(int_1_11_pops_coefs,FUN=data.frame))
int_1_11_pops_coefs
#Add asterisks for significance
int_1_11_pops_coefs$sig <- ifelse(int_1_11_pops_coefs$Pr...t.. < 0.05,"*", "")
head(int_1_11_pops_coefs)
write.table(int_1_11_pops_coefs,file="int_1_11_pops_coefs.txt",sep="\t")



