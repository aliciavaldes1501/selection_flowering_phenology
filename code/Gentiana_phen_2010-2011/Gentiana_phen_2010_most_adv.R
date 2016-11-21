#Repeat the analyses using most_adv instead of phen_index

#Effects of traits on fitness

#Separate models for pops with/without predator

#With predator
modelfr1_pred_10<-lm(n_intact_fruits~most_adv+n_fl+n_shoots+h_shoot+LokalID:most_adv+
                       LokalID:n_fl+LokalID:n_shoots+LokalID:h_shoot,data=data10sta_pred)
Anova(modelfr1_pred_10,type="II")
summary(modelfr1_pred_10)

modelseed1_pred_10<-lm(n_seeds~most_adv+n_fl+n_shoots+h_shoot+LokalID:most_adv+
                         LokalID:n_fl+LokalID:n_shoots+LokalID:h_shoot,data=data10sta_pred)
Anova(modelseed1_pred_10,type="II")
summary(modelseed1_pred_10)

#Without predator
modelfr1_npred_10<-lm(n_intact_fruits~most_adv+n_fl+n_shoots+h_shoot+LokalID:most_adv+
                        LokalID:n_fl+LokalID:n_shoots+LokalID:h_shoot,data=data10sta_npred)
Anova(modelfr1_npred_10,type="II")
summary(modelfr1_npred_10)

modelseed1_npred_10<-lm(n_seeds~most_adv+n_fl+n_shoots+h_shoot+LokalID:most_adv+
                          LokalID:n_fl+LokalID:n_shoots+LokalID:h_shoot,data=data10sta_npred)
Anova(modelseed1_npred_10,type="II")
summary(modelseed1_npred_10)

#Linear selection gradients

library(lme4)
library(lattice)

#Fitness=n_intact_fruits

data10staLokalID <- split(data10sta, data10sta$LokalID)

lsel_fr_10 <- lapply(data10staLokalID, function (x) {
  lm(n_intact_fruits ~ most_adv+n_fl+n_shoots+h_shoot, 
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
write.table(lselcoefs_fr_10,file="lselcoefs_fr_10_most_adv.txt",sep="\t")

#Fitness=n_seeds

lsel_seed_10 <- lapply(data10staLokalID, function (x) {
  lm(n_seeds ~ most_adv+n_fl+n_shoots+h_shoot, 
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
write.table(lselcoefs_seed_10,file="lselcoefs_seed_10_most_adv.txt",sep="\t")

#Differences in linear selection gradients between populations with/without predator
lsel_grads <- read.table(
  "lsel_grads_most_adv.txt",
  header=TRUE, sep="\t", na.strings="NA", dec=",", strip.white=TRUE)
head(lsel_grads)
str(lsel_grads)
lsel_grads$pred<-as.factor(lsel_grads$pred)

par(mfrow=c(1,2))

summary(lm(most_adv~pred,data=subset(lsel_grads,fitness_measure=="n_intact_fruits")))
plot(subset(lsel_grads,fitness_measure=="n_intact_fruits")$pred,
     subset(lsel_grads,fitness_measure=="n_intact_fruits")$most_adv,
     xlab="Predator",ylab="Selection gradient for phenology")
legend("topright", legend ="p =0.001",bty = "n")

summary(lm(most_adv~pred,data=subset(lsel_grads,fitness_measure=="n_seeds")))
plot(subset(lsel_grads,fitness_measure=="n_seeds")$pred,
     subset(lsel_grads,fitness_measure=="n_seeds")$most_adv,
     xlab="Predator",ylab="Selection gradient for most_adv",
     main="Fitness=n_seeds")
legend("topright", legend = "p = 0.117",bty = "n")

library(Rcmdr)
summary(lm(h_shoot~pred,data=subset(lsel_grads,fitness_measure=="n_intact_fruits")))

par(mfrow=c(1,1),bty="l",family="serif",cex=0.8)

plotMeans(subset(lsel_grads,fitness_measure=="n_intact_fruits")$most_adv,
          subset(lsel_grads,fitness_measure=="n_intact_fruits")$pred,
          xlab="Predator",ylab="Selection gradient for phenology",main=NULL)
legend("top", legend ="p =0.001",bty = "n")

plotMeans(subset(lsel_grads,fitness_measure=="n_intact_fruits")$h_shoot,
          subset(lsel_grads,fitness_measure=="n_intact_fruits")$pred,
          xlab="Predator",ylab="Selection gradient for shoot height")
legend("top", legend = "p = 0.750",bty = "n")

plotMeans(subset(lsel_grads,fitness_measure=="n_seeds")$most_adv,
          subset(lsel_grads,fitness_measure=="n_seeds")$pred,
          xlab="Predator",ylab="Selection gradient for most_adv",
          main="Fitness=n_seeds")
legend("topright", legend = "p = 0.117",bty = "n")


#Graphs with ggplot2

science_theme = theme_bw(base_family = "serif")+theme(panel.grid.major = element_line(size = 0, color = "white"),
  axis.line = element_line(size=.7, color = "black"), text = element_text(size=18),
  axis.title.x = element_text(vjust=-0.50,size=18),axis.title.y = element_text(vjust=0.70,size=18),
  axis.text.x=element_text(size=18),axis.text.y=element_text(size=18))+
    theme( plot.background = element_blank() ,panel.grid.major = element_blank() ,
         panel.grid.minor = element_blank() , panel.border = element_blank() ,
         panel.background = element_blank() ) +  theme(axis.line = element_line(color = 'black'))

df1<-subset(subset(lsel_grads,fitness_measure=="n_intact_fruits"))
df1
df1s <- summarySE(df1, measurevar="most_adv", groupvars=c("pred"))
df1s1 <- summarySE(df1, measurevar="h_shoot", groupvars=c("pred"))

ggplot(df1s, aes(x=pred, y=most_adv)) + 
  geom_errorbar(aes(ymin=most_adv-se, ymax=most_adv+se), width=.1,size=.7) +
  geom_line() +  geom_point(size=6)+science_theme+
  labs(x = "Predator", y = "Selection gradient for phenology")

ggplot(df1s1, aes(x=pred, y=h_shoot)) + 
  geom_errorbar(aes(ymin=h_shoot-se, ymax=h_shoot+se), width=.1,size=.7) +
  geom_line() +  geom_point(size=6)+science_theme+
  labs(x = "Predator", y = "Selection gradient for shoot height")

#Relation of linear selection gradients with intensity of predation (in populations with predator)
data10_pred<-subset(data10,predator==1)
data10_pred$LokalID<-factor(data10_pred$LokalID)
data10_npred<-subset(data10,predator==0)
data10_npred$LokalID<-factor(data10_npred$LokalID)

as.data.frame(tapply(data10_pred$n_eggs, data10_pred$LokalID, mean))

data10_pred_means <- aggregate(data10_pred[,c("n_aborted_all","n_eggs","n_pred_all"), 
                                           drop=FALSE],by=list(LokalID=data10_pred$LokalID), FUN=mean)

data10_pred_means <- aggregate(data10_pred[,c("n_eggs"), 
                                           drop=FALSE],by=list(LokalID=data10_pred$LokalID), FUN=sum)

summary(lm(subset(lsel_grads,fitness_measure=="n_intact_fruits"&pred==1)$most_adv~
             data10_pred_means$n_eggs))
plot(data10_pred_means$n_eggs,
     subset(lsel_grads,fitness_measure=="n_intact_fruits"&pred==1)$most_adv,
     xlab="Mean number of eggs",ylab="Selection gradient for phenology")
abline(lm(subset(lsel_grads,fitness_measure=="n_intact_fruits"&pred==1)$most_adv~
             data10_pred_means$n_eggs))
legend("topright", legend = "R2 = 0.001; p = 0.846",bty = "n")

summary(lm(subset(lsel_grads,fitness_measure=="n_seeds"&pred==1)$most_adv~
             data10_pred_means$n_eggs))
plot(sqrt(data10_pred_means$n_eggs),subset(lsel_grads,fitness_measure=="n_seeds"&pred==1)$most_adv)

summary(lm(subset(lsel_grads,fitness_measure=="n_intact_fruits"&pred==1)$n_fl~
             data10_pred_means$n_eggs))
par(mfrow=c(1,2),family="serif",pch=20,bty = "l")
plot(data10_pred_means$n_eggs,subset(lsel_grads,fitness_measure=="n_intact_fruits"&pred==1)$n_fl,
     xlab="Mean number of eggs",ylab="Selection gradient for flower number")
abline(lm(subset(lsel_grads,fitness_measure=="n_intact_fruits"&pred==1)$n_fl~ data10_pred_means$n_eggs))
legend("topright", legend = "R2 =  0.583, p = 0.006",bty = "n")




summary(lm(subset(lsel_grads,fitness_measure=="n_seeds"&pred==1)$n_fl~
             data10_pred_means$n_eggs))
plot(data10_pred_means$n_eggs,subset(lsel_grads,fitness_measure=="n_seeds"&pred==1)$n_fl,
     xlab="Mean number of eggs",ylab="Selection gradient for n_fl",
     main="Fitness=n_seeds")
abline(lm(subset(lsel_grads,fitness_measure=="n_seeds"&pred==1)$n_fl~ data10_pred_means$n_eggs))
legend("topright", legend = "p = 0.3426",bty = "n")

summary(lm(subset(lsel_grads,fitness_measure=="n_intact_fruits"&pred==1)$h_shoot~
             data10_pred_means$n_eggs))
plot(data10_pred_means$n_eggs,subset(lsel_grads,fitness_measure=="n_intact_fruits"&pred==1)$h_shoot,
     xlab="Mean number of eggs",ylab="Selection gradient for h_shoot",
     main="Fitness=n_intact_fruits")
abline(lm(subset(lsel_grads,fitness_measure=="n_intact_fruits"&pred==1)$h_shoot~ data10_pred_means$n_eggs))
legend("topright", legend = "p = 0.102",bty = "n")

summary(lm(subset(lsel_grads,fitness_measure=="n_seeds"&pred==1)$h_shoot~
             data10_pred_means$n_eggs))
plot(data10_pred_means$n_eggs,subset(lsel_grads,fitness_measure=="n_seeds"&pred==1)$h_shoot,
     xlab="Mean number of eggs",ylab="Selection gradient for h_shoot",
     main="Fitness=n_seeds")
abline(lm(subset(lsel_grads,fitness_measure=="n_seeds"&pred==1)$h_shoot~ data10_pred_means$n_eggs))
legend("topright", legend = "p = 0.545",bty = "n")


summary(lm(subset(lsel_grads,fitness_measure=="n_intact_fruits"&pred==1)$most_adv~
             data10_pred_means$n_aborted_all))
plot(data10_pred_means$n_aborted_all,subset(lsel_grads,fitness_measure=="n_intact_fruits"&pred==1)$most_adv,
     xlab="Mean number of aborted",ylab="Selection gradient for most_adv",
     main="Fitness=n_intact_fruits")
abline(lm(subset(lsel_grads,fitness_measure=="n_intact_fruits"&pred==1)$most_adv~
            data10_pred_means$n_aborted_all))
legend("topright", legend = "p = 0.1218",bty = "n")

summary(lm(subset(lsel_grads,fitness_measure=="n_seeds"&pred==1)$most_adv~
             data10_pred_means$n_aborted_all))
plot(data10_pred_means$n_aborted_all,subset(lsel_grads,fitness_measure=="n_seeds"&pred==1)$most_adv,
     xlab="Mean number of aborted",ylab="Selection gradient for most_adv",
     main="Fitness=n_seeds")
abline(lm(subset(lsel_grads,fitness_measure=="n_seeds"&pred==1)$most_adv~
            data10_pred_means$n_aborted_all))
legend("topright", legend = "p = 0.494",bty = "n")

summary(lm(subset(lsel_grads,fitness_measure=="n_intact_fruits"&pred==1)$n_fl~
             data10_pred_means$n_aborted_all))
par(mfrow=c(1,2))
plot(data10_pred_means$n_aborted_all,subset(lsel_grads,fitness_measure=="n_intact_fruits"&pred==1)$n_fl,
     xlab="N of aborted",ylab="Selection gradient for n_fl",
     main="Fitness=n_intact_fruits")
abline(lm(subset(lsel_grads,fitness_measure=="n_intact_fruits"&pred==1)$n_fl~ data10_pred_means$n_aborted_all))
legend("topright", legend = "p = 0.201",bty = "n")

summary(lm(subset(lsel_grads,fitness_measure=="n_seeds"&pred==1)$n_fl~
             data10_pred_means$n_aborted_all))
plot(data10_pred_means$n_aborted_all,subset(lsel_grads,fitness_measure=="n_seeds"&pred==1)$n_fl,
     xlab="N of aborted",ylab="Selection gradient for n_fl",
     main="Fitness=n_seeds")
abline(lm(subset(lsel_grads,fitness_measure=="n_seeds"&pred==1)$n_fl~ data10_pred_means$n_aborted_all))
legend("topright", legend = "p = 0.309",bty = "n")

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
legend("topright", legend = "p = 0.605",bty = "n")

summary(lm(subset(lsel_grads,fitness_measure=="n_intact_fruits"&pred==1)$most_adv~
             data10_pred_means$n_pred_all))
plot(data10_pred_means$n_pred_all,subset(lsel_grads,fitness_measure=="n_intact_fruits"&pred==1)$most_adv,
     xlab="Mean pred_all",ylab="Selection gradient for most_adv",
     main="Fitness=n_intact_fruits")
abline(lm(subset(lsel_grads,fitness_measure=="n_intact_fruits"&pred==1)$most_adv~
            data10_pred_means$n_pred_all))
legend("topright", legend = "p = 0.553",bty = "n")

summary(lm(subset(lsel_grads,fitness_measure=="n_seeds"&pred==1)$most_adv~
             data10_pred_means$n_pred_all))
plot(data10_pred_means$n_pred_all,subset(lsel_grads,fitness_measure=="n_seeds"&pred==1)$most_adv,
     xlab="Mean pred_all",ylab="Selection gradient for most_adv",
     main="Fitness=n_seeds")
abline(lm(subset(lsel_grads,fitness_measure=="n_seeds"&pred==1)$most_adv~
            data10_pred_means$n_pred_all))
legend("topright", legend = "p = 0.510",bty = "n")

summary(lm(subset(lsel_grads,fitness_measure=="n_intact_fruits"&pred==1)$n_fl~
             data10_pred_means$n_pred_all))
par(mfrow=c(1,2))
plot(data10_pred_means$n_pred_all,subset(lsel_grads,fitness_measure=="n_intact_fruits"&pred==1)$n_fl,
     xlab="Mean pred_all",ylab="Selection gradient for n_fl",
     main="Fitness=n_intact_fruits")
abline(lm(subset(lsel_grads,fitness_measure=="n_intact_fruits"&pred==1)$n_fl~ data10_pred_means$n_pred_all))
legend("topright", legend = "p = 0.919",bty = "n")

summary(lm(subset(lsel_grads,fitness_measure=="n_seeds"&pred==1)$n_fl~
             data10_pred_means$n_pred_all))
plot(data10_pred_means$n_pred_all,subset(lsel_grads,fitness_measure=="n_seeds"&pred==1)$n_fl,
     xlab="Mean pred_all",ylab="Selection gradient for n_fl",
     main="Fitness=n_seeds")
abline(lm(subset(lsel_grads,fitness_measure=="n_seeds"&pred==1)$n_fl~ data10_pred_means$n_pred_all))
legend("topright", legend = "p = 0.551",bty = "n")

summary(lm(subset(lsel_grads,fitness_measure=="n_intact_fruits"&pred==1)$h_shoot~
             data10_pred_means$n_pred_all))
par(mfrow=c(1,2))
plot(data10_pred_means$n_pred_all,subset(lsel_grads,fitness_measure=="n_intact_fruits"&pred==1)$h_shoot,
     xlab="Mean pred_all",ylab="Selection gradient for h_shoot",
     main="Fitness=n_intact_fruits")
abline(lm(subset(lsel_grads,fitness_measure=="n_intact_fruits"&pred==1)$h_shoot~ data10_pred_means$n_pred_all))
legend("topright", legend = "p = 0.640",bty = "n")

summary(lm(subset(lsel_grads,fitness_measure=="n_seeds"&pred==1)$h_shoot~
             data10_pred_means$n_pred_all))
plot(data10_pred_means$n_pred_all,subset(lsel_grads,fitness_measure=="n_seeds"&pred==1)$h_shoot,
     xlab="Mean pred_all",ylab="Selection gradient for h_shoot",
     main="Fitness=n_seeds")
abline(lm(subset(lsel_grads,fitness_measure=="n_seeds"&pred==1)$h_shoot~ data10_pred_means$n_pred_all))
legend("topright", legend = "p = 0.572",bty = "n")

#Effects of traits on interaction intensity

int1_10<-lm(n_eggs~most_adv+n_fl+n_shoots+h_shoot+LokalID*most_adv+
              LokalID*n_fl+LokalID*n_shoots+LokalID*h_shoot,data=subset(data10sta,predator==1))
Anova(int1_10,type="II")

int2_10<-lm(n_pred_all~most_adv+n_fl+n_shoots+h_shoot+LokalID*most_adv+
              LokalID*n_fl+LokalID*n_shoots+LokalID*h_shoot,data=subset(data10sta,predator==1))
Anova(int2_10,type="II")

int3_10<-lm(n_aborted_all~most_adv+n_fl+n_shoots+h_shoot+LokalID*most_adv+
              LokalID*n_fl+LokalID*n_shoots+LokalID*h_shoot,data=subset(data10sta,predator==1))
Anova(int3_10,type="II")

int4_10<-lm(perc_undeveloped~most_adv+n_fl+n_shoots+h_shoot+LokalID*most_adv+
              LokalID*n_fl+LokalID*n_shoots+LokalID*h_shoot,data=subset(data10sta,predator==1))
Anova(int4_10,type="II") #Very few observations!

#Effects of traits on interaction intensity: models for each population

data10sta_pred<-subset(data10sta,predator==1)
levels(data10sta_pred$LokalID)
data10sta_pred$LokalID <- factor(data10sta_pred$LokalID)
levels(data10sta_pred$LokalID)

data10sta_predLokalID <- split(data10sta_pred, data10sta_pred$LokalID)

int_1_10_pops <- lapply(data10sta_predLokalID, function (x) {
  lm(n_eggs ~ most_adv+n_fl+n_shoots+h_shoot, 
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
  lm(n_pred_all ~ most_adv+n_fl+n_shoots+h_shoot, 
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
  lm(n_aborted_all ~ most_adv+n_fl+n_shoots+h_shoot, 
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









#SEM: traits, interaction intensity, fitness
library(lavaan)
library(semPlot)
sem1<-' # regressions
n_intact_fruits_t ~ most_adv_t
n_intact_fruits_t ~ n_eggs_t
n_eggs_t ~ most_adv_t
'
data10_pred$n_intact_fruits_t<-sqrt(data10_pred$n_intact_fruits)
data10_pred$n_eggs_t<-sqrt(data10_pred$n_eggs)
data10_pred$most_adv_t<-sqrt(data10_pred$most_adv)


fit1 <- sem(sem1, data = data10_pred, estimator= "MLM")
summary(fit1, standardized = TRUE,fit.measures=TRUE)
semPaths(fit1,what="stand")
