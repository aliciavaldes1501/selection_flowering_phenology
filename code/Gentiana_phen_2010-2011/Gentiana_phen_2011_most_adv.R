#Repeat the analyses using most_adv instead of phen_index

#Effects of traits on fitness

#Separate models for pops with/without predator

#With predator
modelfr1_pred_11<-lm(n_intact_fruits~most_adv+n_fl+n_shoots+h_shoot+LokalID:most_adv+
                       LokalID:n_fl+LokalID:n_shoots+LokalID:h_shoot,data=data11sta_pred)
Anova(modelfr1_pred_11,type="II")
summary(modelfr1_pred_11)

modelseed1_pred_11<-lm(n_seeds~most_adv+n_fl+n_shoots+h_shoot+LokalID:most_adv+
                         LokalID:n_fl+LokalID:n_shoots+LokalID:h_shoot,data=data11sta_pred)
Anova(modelseed1_pred_11,type="II")
summary(modelseed1_pred_11)

#Without predator
modelfr1_npred_11<-lm(n_intact_fruits~most_adv+n_fl+n_shoots+h_shoot+LokalID:most_adv+
                        LokalID:n_fl+LokalID:n_shoots+LokalID:h_shoot,data=data11sta_npred)
Anova(modelfr1_npred_11,type="II")
summary(modelfr1_npred_11)

modelseed1_npred_11<-lm(n_seeds~most_adv+n_fl+n_shoots+h_shoot+LokalID:most_adv+
                          LokalID:n_fl+LokalID:n_shoots+LokalID:h_shoot,data=data11sta_npred)
Anova(modelseed1_npred_11,type="II")
summary(modelseed1_npred_11)

#Linear selection gradients

library(lme4)
library(lattice)

#Fitness=n_intact_fruits

data11staLokalID <- split(data11sta, data11sta$LokalID)

lsel_fr_11 <- lapply(data11staLokalID, function (x) {
  lm(n_intact_fruits ~ most_adv+n_fl+n_shoots+h_shoot, 
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
write.table(lselcoefs_fr_11,file="lselcoefs_fr_11_most_adv.txt",sep="\t")

#Fitness=n_seeds

lsel_seed_11 <- lapply(data11staLokalID, function (x) {
  lm(n_seeds ~ most_adv+n_fl+n_shoots+h_shoot, 
     data = x)
})

lsel_seed_11

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
write.table(lselcoefs_seed_11,file="lselcoefs_seed_11_most_adv.txt",sep="\t")

#Differences in linear selection gradients between populations with/without predator
lsel_grads <- read.table(
  "lsel_grads_11_most_adv.txt",
  header=TRUE, sep="\t", na.strings="NA", dec=",", strip.white=TRUE)
head(lsel_grads)
str(lsel_grads)
lsel_grads$pred<-as.factor(lsel_grads$pred)

par(mfrow=c(1,2))

summary(lm(most_adv~pred,data=subset(lsel_grads,fitness_measure=="n_intact_fruits")))
plot(subset(lsel_grads,fitness_measure=="n_intact_fruits")$pred,
     subset(lsel_grads,fitness_measure=="n_intact_fruits")$most_adv,
     xlab="Predator",ylab="Selection gradient for most_adv",
     main="Fitness=n_intact_fruits")
legend("topright", legend = "p < 0.001",bty = "n")

summary(lm(most_adv~pred,data=subset(lsel_grads,fitness_measure=="n_seeds")))
plot(subset(lsel_grads,fitness_measure=="n_seeds")$pred,
     subset(lsel_grads,fitness_measure=="n_seeds")$most_adv,
     xlab="Predator",ylab="Selection gradient for most_adv",
     main="Fitness=n_seeds")
legend("topright", legend = "p = 0.595",bty = "n")

library(Rcmdr)
plotMeans(subset(lsel_grads,fitness_measure=="n_intact_fruits")$most_adv,
          subset(lsel_grads,fitness_measure=="n_intact_fruits")$pred,
          xlab="Predator",ylab="Selection gradient for phenology",main=NULL)
legend("top", legend ="p <0.001",bty = "n")

plotMeans(subset(lsel_grads,fitness_measure=="n_intact_fruits")$h_shoot,
          subset(lsel_grads,fitness_measure=="n_intact_fruits")$pred,
          xlab="Predator",ylab="Selection gradient for shoot height")
legend("top", legend = "p = 0.040",bty = "n")


plotMeans(subset(lsel_grads,fitness_measure=="n_intact_fruits")$most_adv,
          subset(lsel_grads,fitness_measure=="n_intact_fruits")$pred,
          xlab="Predator",ylab="Selection gradient for most_adv",
          main="Fitness=n_intact_fruits")
legend("topright", legend = "p < 0.001",bty = "n")

plotMeans(subset(lsel_grads,fitness_measure=="n_seeds")$most_adv,
          subset(lsel_grads,fitness_measure=="n_seeds")$pred,
          xlab="Predator",ylab="Selection gradient for most_adv",
          main="Fitness=n_seeds")
legend("topright", legend = "p = 0.595",bty = "n")

summary(lm(h_shoot~pred,data=subset(lsel_grads,fitness_measure=="n_intact_fruits")))
plot(subset(lsel_grads,fitness_measure=="n_intact_fruits")$pred,
     subset(lsel_grads,fitness_measure=="n_intact_fruits")$h_shoot,
     xlab="Predator",ylab="Selection gradient for h_shoot",
     main="Fitness=n_intact_fruits")
legend("topright", legend = "p < 0.0395",bty = "n")

summary(lm(h_shoot~pred,data=subset(lsel_grads,fitness_measure=="n_seeds")))
plot(subset(lsel_grads,fitness_measure=="n_seeds")$pred,
     subset(lsel_grads,fitness_measure=="n_seeds")$h_shoot,
     xlab="Predator",ylab="Selection gradient for h_shoot",
     main="Fitness=n_seeds")
legend("topright", legend = "p = 0.275",bty = "n")

plotMeans(subset(lsel_grads,fitness_measure=="n_intact_fruits")$h_shoot,
          subset(lsel_grads,fitness_measure=="n_intact_fruits")$pred,
          xlab="Predator",ylab="Selection gradient for v",
          main="Fitness=n_intact_fruits")
legend("topright", legend = "p < 0.0395",bty = "n")

plotMeans(subset(lsel_grads,fitness_measure=="n_seeds")$h_shoot,
          subset(lsel_grads,fitness_measure=="n_seeds")$pred,
          xlab="Predator",ylab="Selection gradient for h_shoot",
          main="Fitness=n_seeds")
legend("topright", legend = "p = 0.275",bty = "n")

#Graphs with ggplot2

df2<-subset(subset(lsel_grads,fitness_measure=="n_intact_fruits"))
df2
df2s <- summarySE(df2, measurevar="most_adv", groupvars=c("pred"))
df2s1 <- summarySE(df2, measurevar="h_shoot", groupvars=c("pred"))

ggplot(df2s, aes(x=pred, y=most_adv)) + 
  geom_errorbar(aes(ymin=most_adv-se, ymax=most_adv+se), width=.1,size=.7) +
  geom_line() +  geom_point(size=6)+science_theme+
  labs(x = "Predator", y = "Selection gradient for phenology")

ggplot(df2s1, aes(x=pred, y=h_shoot)) + 
  geom_errorbar(aes(ymin=h_shoot-se, ymax=h_shoot+se), width=.1,size=.7) +
  geom_line() +  geom_point(size=6)+science_theme+
  labs(x = "Predator", y = "Selection gradient for shoot height")

#Relation of linear selection gradients with intensity of predation (in populations with predator)
data11_pred<-subset(data11,Predator==1)
data11_pred$LokalID<-factor(data11_pred$LokalID)
data11_npred<-subset(data11,Predator==0)
data11_npred$LokalID<-factor(data11_npred$LokalID)

as.data.frame(tapply(data11_pred$n_eggs, data11_pred$LokalID, mean))

data11_pred_means <- aggregate(data11_pred[,c("n_aborted_all","n_eggs","n_pred_all"), 
                                           drop=FALSE],by=list(LokalID=data11_pred$LokalID), FUN=mean)

summary(lm(subset(lsel_grads,fitness_measure=="n_intact_fruits"&pred==1)$most_adv~
             data11_pred_means$n_eggs))
plot(data11_pred_means$n_eggs,
     subset(lsel_grads,fitness_measure=="n_intact_fruits"&pred==1)$most_adv,
     xlab="Mean number of eggs",ylab="Selection gradient for phenology")
abline(lm(subset(lsel_grads,fitness_measure=="n_intact_fruits"&pred==1)$most_adv~
             data11_pred_means$n_eggs))
legend("topright", legend = "R2 = 0.265; p = 0.105",bty = "n")

summary(lm(subset(lsel_grads,fitness_measure=="n_seeds"&pred==1)$most_adv~
             data11_pred_means$n_eggs))
plot(sqrt(data11_pred_means$n_eggs),subset(lsel_grads,fitness_measure=="n_seeds"&pred==1)$most_adv)

summary(lm(subset(lsel_grads,fitness_measure=="n_intact_fruits"&pred==1)$n_fl~
             data11_pred_means$n_eggs))
par(mfrow=c(1,2),family="serif",pch=20,bty = "l")

plot(data11_pred_means$n_eggs,subset(lsel_grads,fitness_measure=="n_intact_fruits"&pred==1)$n_fl,
     xlab="Mean number of eggs",ylab="Selection gradient for flower number")
abline(lm(subset(lsel_grads,fitness_measure=="n_intact_fruits"&pred==1)$n_fl~ data11_pred_means$n_eggs))
legend("topright", legend = "R2 = 0.166; p = 0.214",bty = "n")


plot(data11_pred_means$n_eggs,subset(lsel_grads,fitness_measure=="n_intact_fruits"&pred==1)$n_fl,
     xlab="Mean number of eggs",ylab="Selection gradient for n_fl",
     main="Fitness=n_intact_fruits")
abline(lm(subset(lsel_grads,fitness_measure=="n_intact_fruits"&pred==1)$n_fl~ data11_pred_means$n_eggs))

summary(lm(subset(lsel_grads,fitness_measure=="n_seeds"&pred==1)$n_fl~
             data11_pred_means$n_eggs))
plot(data11_pred_means$n_eggs,subset(lsel_grads,fitness_measure=="n_seeds"&pred==1)$n_fl,
     xlab="Mean number of eggs",ylab="Selection gradient for n_fl",
     main="Fitness=n_seeds")
abline(lm(subset(lsel_grads,fitness_measure=="n_seeds"&pred==1)$n_fl~ data11_pred_means$n_eggs))

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

summary(lm(subset(lsel_grads,fitness_measure=="n_intact_fruits"&pred==1)$most_adv~
             data11_pred_means$n_aborted_all))
plot(data11_pred_means$n_aborted_all,subset(lsel_grads,fitness_measure=="n_intact_fruits"&pred==1)$most_adv,
     xlab="Mean number of aborted",ylab="Selection gradient for most_adv",
     main="Fitness=n_intact_fruits")
abline(lm(subset(lsel_grads,fitness_measure=="n_intact_fruits"&pred==1)$most_adv~
            data11_pred_means$n_aborted_all))

summary(lm(subset(lsel_grads,fitness_measure=="n_seeds"&pred==1)$most_adv~
             data11_pred_means$n_aborted_all))
plot(data11_pred_means$n_aborted_all,subset(lsel_grads,fitness_measure=="n_seeds"&pred==1)$most_adv,
     xlab="Mean number of aborted",ylab="Selection gradient for most_adv",
     main="Fitness=n_seeds")
abline(lm(subset(lsel_grads,fitness_measure=="n_seeds"&pred==1)$most_adv~
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

summary(lm(subset(lsel_grads,fitness_measure=="n_intact_fruits"&pred==1)$most_adv~
             data11_pred_means$n_pred_all))
plot(data11_pred_means$n_pred_all,subset(lsel_grads,fitness_measure=="n_intact_fruits"&pred==1)$most_adv,
     xlab="Mean pred_all",ylab="Selection gradient for most_adv",
     main="Fitness=n_intact_fruits")
abline(lm(subset(lsel_grads,fitness_measure=="n_intact_fruits"&pred==1)$most_adv~
            data11_pred_means$n_pred_all))

summary(lm(subset(lsel_grads,fitness_measure=="n_seeds"&pred==1)$most_adv~
             data11_pred_means$n_pred_all))
plot(data11_pred_means$n_pred_all,subset(lsel_grads,fitness_measure=="n_seeds"&pred==1)$most_adv,
     xlab="Mean pred_all",ylab="Selection gradient for most_adv",
     main="Fitness=n_seeds")
abline(lm(subset(lsel_grads,fitness_measure=="n_seeds"&pred==1)$most_adv~
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

int1_11<-lm(n_eggs~most_adv+n_fl+n_shoots+h_shoot+LokalID*most_adv+
              LokalID*n_fl+LokalID*n_shoots+LokalID*h_shoot,data=subset(data11sta,predator==1))
Anova(int1_11,type="II")

int2_11<-lm(n_pred_all~most_adv+n_fl+n_shoots+h_shoot+LokalID*most_adv+
              LokalID*n_fl+LokalID*n_shoots+LokalID*h_shoot,data=subset(data11sta,predator==1))
Anova(int2_11,type="II")
summary(lm(n_pred_all~most_adv+n_fl+n_shoots+h_shoot,data=subset(data11sta,predator==1)))

int3_11<-lm(n_aborted_all~most_adv+n_fl+n_shoots+h_shoot+LokalID*most_adv+
              LokalID*n_fl+LokalID*n_shoots+LokalID*h_shoot,data=subset(data11sta,predator==1))
Anova(int3_11,type="II")
summary(lm(n_aborted_all~most_adv+n_fl+n_shoots+h_shoot,data=subset(data11sta,predator==1)))

int4_11<-lm(perc_undeveloped~most_adv+n_fl+n_shoots+h_shoot+LokalID*most_adv+
              LokalID*n_fl+LokalID*n_shoots+LokalID*h_shoot,data=subset(data11sta,predator==1))
Anova(int4_11,type="II") #Very few observations!

#Effects of traits on interaction intensity: models for each population

data11sta_pred<-subset(data11sta,predator==1)
levels(data11sta_pred$LokalID)
data11sta_pred$LokalID <- factor(data11sta_pred$LokalID)
levels(data11sta_pred$LokalID)

data11sta_predLokalID <- split(data11sta_pred, data11sta_pred$LokalID)

int_1_11_pops <- lapply(data11sta_predLokalID, function (x) {
  lm(n_eggs ~ most_adv+n_fl+n_shoots+h_shoot, 
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

