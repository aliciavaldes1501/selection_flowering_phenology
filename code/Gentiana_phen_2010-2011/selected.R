#2010
#Read data
data10 <- read.table(
  "data1_2010.txt",
  header=TRUE, sep="\t", na.strings="NA", dec=",", strip.white=TRUE)
head(data10)
summary(data10)
str(data10)

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
names(data10)[14:15]<-c("n_intact_fruits","n_seeds")
names(data10)[17:18]<-c("n_intact_fruits_mean","n_seeds_mean")

names(data10)[19:23]<-c("n_shoots_mean","h_shoot_mean","n_fl_mean","phen_index_mean","most_adv_mean")

data10<-merge(data10,sd_n_shoots, by="LokalID")
data10<-merge(data10,sd_h_shoot, by="LokalID")
data10<-merge(data10,sd_n_fl, by="LokalID")
data10<-merge(data10,sd_phen_index, by="LokalID")
data10<-merge(data10,sd_most_adv, by="LokalID")

names(data10)
names(data10)[5:9]<-c("n_shoots","h_shoot","n_fl","phen_index","most_adv")
names(data10)[24:28]<-c("n_shoots_sd","h_shoot_sd","n_fl_sd","phen_index_sd","most_adv_sd")

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

data10sta<-data10[c(1:4,16,28:32,10:13,33,34)]
head(data10sta)
names(data10sta)<-c("LokalID","predator","Lokal","Individ","attack","n_shoots","h_shoot","n_fl","phen_index","most_adv",
                    "n_eggs","n_pred_all","n_aborted_all","perc_undeveloped","n_intact_fruits","n_seeds")
summary(data10sta,by="LokalID")

#Effects of traits on fitness

modelfr_10<-lm(n_intact_fruits~most_adv+n_fl+n_shoots+h_shoot+LokalID:most_adv+
                 LokalID:n_fl+LokalID:n_shoots+LokalID:h_shoot,data=data10sta)
library(car)
Anova(modelfr_10,type="II")

summary(lm(n_intact_fruits~most_adv+n_fl+n_shoots+h_shoot,data=data10sta))

#Linear selection gradients

library(lme4)
library(lattice)

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

#Differences in linear selection gradients between populations with/without predator
lsel_grads_10 <- read.table(
  "lsel_grads_most_adv.txt",
  header=TRUE, sep="\t", na.strings="NA", dec=",", strip.white=TRUE)
head(lsel_grads_10)
str(lsel_grads_10)
lsel_grads_10$pred<-as.factor(lsel_grads_10$pred)

par(mfrow=c(1,2))

summary(lm(most_adv~pred,data=subset(lsel_grads_10,fitness_measure=="n_intact_fruits")))
plot(subset(lsel_grads_10,fitness_measure=="n_intact_fruits")$pred,
     subset(lsel_grads_10,fitness_measure=="n_intact_fruits")$most_adv,
     xlab="Predator",ylab="Selection gradient for phenology")
legend("topright", legend ="p =0.001",bty = "n")

library(Rcmdr)
summary(lm(h_shoot~pred,data=subset(lsel_grads_10,fitness_measure=="n_intact_fruits")))

par(mfrow=c(1,1),bty="l",family="serif",col="red")

plotMeans(subset(lsel_grads_10,fitness_measure=="n_intact_fruits")$most_adv,
          subset(lsel_grads_10,fitness_measure=="n_intact_fruits")$pred,
          xlab="Predator",ylab="Selection gradient for phenology",main=NULL,
          xlim=c(0.5,2.5),ylim=c(-0.3,0.4))
legend("top", legend ="p =0.001",bty = "n")

plotMeans(subset(lsel_grads_10,fitness_measure=="n_intact_fruits")$h_shoot,
          subset(lsel_grads_10,fitness_measure=="n_intact_fruits")$pred,
          xlab="Predator",ylab="Selection gradient for shoot height")
legend("top", legend = "p = 0.750",bty = "n")

#Graphs with ggplot2

science_theme <- theme_bw(base_family = "serif")+theme(panel.grid.major = element_line(size = 0, color = "white"),
   axis.line = element_line(size=.7, color = "black"), text = element_text(size=18),
   axis.title.x = element_text(vjust=-0.50,size=18),axis.title.y = element_text(vjust=0.70,size=18),
    axis.text.x=element_text(size=18),axis.text.y=element_text(size=18))+
  theme( plot.background = element_blank() ,panel.grid.major = element_blank() ,
         panel.grid.minor = element_blank() , panel.border = element_blank() ,
         panel.background = element_blank() ) +  theme(axis.line = element_line(color = 'black'))

df1<-subset(subset(lsel_grads_10,fitness_measure=="n_intact_fruits"))
df1
df1s <- summarySE(df1, measurevar="most_adv", groupvars=c("pred"))
df1s1 <- summarySE(df1, measurevar="h_shoot", groupvars=c("pred"))

library(ggplot2)
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

data10_pred_means <- aggregate(data10_pred[,c("n_eggs"), 
                     drop=FALSE],by=list(LokalID=data10_pred$LokalID), FUN=mean)
summary(lm(subset(lsel_grads_10,fitness_measure=="n_intact_fruits"&pred==1)$most_adv~
             data10_pred_means$n_eggs))
plot(data10_pred_means$n_eggs,
     subset(lsel_grads_10,fitness_measure=="n_intact_fruits"&pred==1)$most_adv,
     xlab="Mean number of eggs",ylab="Selection gradient for phenology")
abline(lm(subset(lsel_grads_10,fitness_measure=="n_intact_fruits"&pred==1)$most_adv~
            data10_pred_means$n_eggs))
legend("topright", legend = "R2 = 0.001; p = 0.846",bty = "n")

summary(lm(subset(lsel_grads_10,fitness_measure=="n_intact_fruits"&pred==1)$n_fl~
             data10_pred_means$n_eggs))
par(mfrow=c(1,2),family="serif",pch=20,bty = "l")
plot(data10_pred_means$n_eggs,subset(lsel_grads_10,fitness_measure=="n_intact_fruits"&pred==1)$n_fl,
     xlab="Mean number of eggs",ylab="Selection gradient for flower number")
abline(lm(subset(lsel_grads_10,fitness_measure=="n_intact_fruits"&pred==1)$n_fl~ data10_pred_means$n_eggs))
legend("topright", legend = "R2 =  0.583, p = 0.006",bty = "n")

summary(lm(subset(lsel_grads_10,fitness_measure=="n_intact_fruits"&pred==1)$h_shoot~
             data10_pred_means$n_eggs))
plot(data10_pred_means$n_eggs,subset(lsel_grads_10,fitness_measure=="n_intact_fruits"&pred==1)$h_shoot,
     xlab="Mean number of eggs",ylab="Selection gradient for h_shoot",
     main="Fitness=n_intact_fruits")
abline(lm(subset(lsel_grads_10,fitness_measure=="n_intact_fruits"&pred==1)$h_shoot~ data10_pred_means$n_eggs))
legend("topright", legend = "p = 0.102",bty = "n")

#Effects of traits on interaction intensity

int1_10<-lm(n_eggs~most_adv+n_fl+n_shoots+h_shoot+LokalID*most_adv+
              LokalID*n_fl+LokalID*n_shoots+LokalID*h_shoot,data=subset(data10sta,predator==1))
Anova(int1_10,type="II")

Anova(lm(attack~most_adv+n_fl+n_shoots+h_shoot+LokalID*most_adv+
              LokalID*n_fl+LokalID*n_shoots+LokalID*h_shoot,data=subset(data10sta,predator==1)),type="II")

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

ggplot(data10_pred, aes(x=most_adv, y=attack,color=LokalID)) +
  geom_smooth(method=lm, se=FALSE, size=0.7,fullrange=T)+ 
  scale_x_continuous(breaks=c(0,1,2,3,4,5,6))+
  labs(x = "Phenology (early flowering)", y = "Attack")+
  science_theme+scale_colour_manual(values=cbbPalette)

ggplot(data10_pred, aes(x=most_adv, y=n_eggs,color=LokalID)) +
  geom_smooth(method=lm, se=FALSE, size=0.7,fullrange=T)+ 
  scale_x_continuous(breaks=c(0,1,2,3,4,5,6))+
  labs(x = "Phenology (early flowering)", y = "Number of eggs")+
  science_theme+scale_colour_manual(values=cbbPalette)

#Significance?
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
#Read data
data11 <- read.table(
  "data1_2011.txt",
  header=TRUE, sep="\t", na.strings="NA", dec=",", strip.white=TRUE)
head(data11)
summary(data11)
str(data11)

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

modelfr1_11<-lm(n_intact_fruits~most_adv+n_fl+n_shoots+h_shoot+LokalID:most_adv+
                  LokalID:n_fl+LokalID:n_shoots+LokalID:h_shoot,data=data11sta)
Anova(modelfr1_11,type="II")

#Linear selection gradients

library(lme4)
library(lattice)

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

#Differences in linear selection gradients between populations with/without predator
lsel_grads_11 <- read.table(
  "lsel_grads_11_most_adv.txt",
  header=TRUE, sep="\t", na.strings="NA", dec=",", strip.white=TRUE)
head(lsel_grads_11)
str(lsel_grads_11)
lsel_grads_11$pred<-as.factor(lsel_grads_11$pred)

par(mfrow=c(1,1),bty="l",family="serif",col="blue")

summary(lm(most_adv~pred,data=subset(lsel_grads_11,fitness_measure=="n_intact_fruits")))
plot(subset(lsel_grads_11,fitness_measure=="n_intact_fruits")$pred,
     subset(lsel_grads_11,fitness_measure=="n_intact_fruits")$most_adv,
     xlab="Predator",ylab="Selection gradient for most_adv",
     main="Fitness=n_intact_fruits")
legend("topright", legend = "p < 0.001",bty = "n")

library(Rcmdr)
plotMeans(subset(lsel_grads_11,fitness_measure=="n_intact_fruits")$most_adv,
          subset(lsel_grads_11,fitness_measure=="n_intact_fruits")$pred,
          xlab="Predator",ylab="Selection gradient for phenology",main=NULL,
          xlim=c(0.5,2.5),ylim=c(-0.3,0.4))
legend("top", legend ="p <0.001",bty = "n")

plotMeans(subset(lsel_grads_11,fitness_measure=="n_intact_fruits")$h_shoot,
          subset(lsel_grads_11,fitness_measure=="n_intact_fruits")$pred,
          xlab="Predator",ylab="Selection gradient for shoot height")
legend("top", legend = "p = 0.040",bty = "n")

summary(lm(h_shoot~pred,data=subset(lsel_grads_11,fitness_measure=="n_intact_fruits")))
plot(subset(lsel_grads_11,fitness_measure=="n_intact_fruits")$pred,
     subset(lsel_grads_11,fitness_measure=="n_intact_fruits")$h_shoot,
     xlab="Predator",ylab="Selection gradient for h_shoot",
     main="Fitness=n_intact_fruits")
legend("topright", legend = "p = 0.0395",bty = "n")

plotMeans(subset(lsel_grads_11,fitness_measure=="n_intact_fruits")$h_shoot,
          subset(lsel_grads_11,fitness_measure=="n_intact_fruits")$pred,
          xlab="Predator",ylab="Selection gradient for v",
          main="Fitness=n_intact_fruits")
legend("topright", legend = "p = 0.0395",bty = "n")

#Graphs with ggplot2

df2<-subset(subset(lsel_grads_11,fitness_measure=="n_intact_fruits"))
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

summary(lm(subset(lsel_grads_11,fitness_measure=="n_intact_fruits"&pred==1)$most_adv~
             data11_pred_means$n_eggs))
plot(data11_pred_means$n_eggs,
     subset(lsel_grads_11,fitness_measure=="n_intact_fruits"&pred==1)$most_adv,
     xlab="Mean number of eggs",ylab="Selection gradient for phenology")
abline(lm(subset(lsel_grads_11,fitness_measure=="n_intact_fruits"&pred==1)$most_adv~
            data11_pred_means$n_eggs))
legend("topright", legend = "R2 = 0.265; p = 0.105",bty = "n")

summary(lm(subset(lsel_grads_11,fitness_measure=="n_intact_fruits"&pred==1)$n_fl~
             data11_pred_means$n_eggs))
par(mfrow=c(1,2),family="serif",pch=20,bty = "l")

plot(data11_pred_means$n_eggs,subset(lsel_grads_11,fitness_measure=="n_intact_fruits"&pred==1)$n_fl,
     xlab="Mean number of eggs",ylab="Selection gradient for flower number")
abline(lm(subset(lsel_grads_11,fitness_measure=="n_intact_fruits"&pred==1)$n_fl~ data11_pred_means$n_eggs))
legend("topright", legend = "R2 = 0.166; p = 0.214",bty = "n")

plot(data11_pred_means$n_eggs,subset(lsel_grads_11,fitness_measure=="n_intact_fruits"&pred==1)$n_fl,
     xlab="Mean number of eggs",ylab="Selection gradient for n_fl",
     main="Fitness=n_intact_fruits")
abline(lm(subset(lsel_grads_11,fitness_measure=="n_intact_fruits"&pred==1)$n_fl~ data11_pred_means$n_eggs))

summary(lm(subset(lsel_grads_11,fitness_measure=="n_intact_fruits"&pred==1)$h_shoot~
             data11_pred_means$n_eggs))
par(mfrow=c(1,2))
plot(data11_pred_means$n_eggs,subset(lsel_grads_11,fitness_measure=="n_intact_fruits"&pred==1)$h_shoot,
     xlab="Mean number of eggs",ylab="Selection gradient for h_shoot",
     main="Fitness=n_intact_fruits")
abline(lm(subset(lsel_grads_11,fitness_measure=="n_intact_fruits"&pred==1)$h_shoot~ data11_pred_means$n_eggs))

#Effects of traits on interaction intensity

int1_11<-lm(n_eggs~most_adv+n_fl+n_shoots+h_shoot+LokalID*most_adv+
              LokalID*n_fl+LokalID*n_shoots+LokalID*h_shoot,data=subset(data11sta,predator==1))
Anova(int1_11,type="II")

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

##Effects of traits on interaction intensity: graphs for each population
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

#Ants
#Read data
ants_mean_max <- read.table(
  "ants_mean_max.txt",
  header=TRUE, sep="\t", na.strings="NA", dec=",", strip.white=TRUE)

ants_mean_max
str(ants_mean_max)

ants_mean_max$logants<-log(ants_mean_max$ants_max+0.025)
logistic<-glm(pred~logants,data=ants_mean_max,family=binomial)
summary(logistic)
par(mfrow=c(1,1),family="serif")
plot(ants_mean_max$logants,ants_mean_max$pred,xlab="Myrmica abundance (log)",ylab="M. alcon presence",bty="l",pch=20,family="serif")
curve(predict(logistic, data.frame(logants=x), type="resp"), add=TRUE) 
legend("topleft", legend = "p = 0.045",bty = "n") 

ants_mean_max$LokalID<-ants_mean_max$pop
antstry<-merge(ants_mean_max,data10,by="LokalID")
antstry<-merge(ants_mean_max,data11,by="LokalID")

summary(with(subset(antstry,pred==1),lm(n_eggs~logants)))     ###SIGNIF!! -but negative effect!
with(subset(antstry,pred==1),plot(logants,n_eggs))
abline(with(subset(antstry,pred==1),lm(n_eggs~logants)))
logistic2<-glm(attack~logants,data=subset(antstry,pred==1),family=binomial)
summary(logistic2)
with(subset(antstry,pred==1),plot(logants,attack))
curve(predict(logistic2, data.frame(logants=x), type="resp"), add=TRUE) 

###################################################################################
ants_intensity <- read.table(
  "ants_intensity.txt",
  header=TRUE, sep="\t", na.strings="NA", dec=",", strip.white=TRUE)
ants_intensity

summary(lm(meaneggs~logants,data=subset(ants_intensity,year==2010)))
summary(lm(meaneggs~logants,data=subset(ants_intensity,year==2011)))
summary(lm(propattack~logants,data=subset(ants_intensity,year==2010)))
summary(lm(propattack~logants,data=subset(ants_intensity,year==2011)))               


par(mfrow=c(1,1),col="black",pch=20,family="serif",bty="l",cex=1.5,cex.axis=0.7,cex.lab=0.7)
plot(ants_mean_max$logants,ants_mean_max$pred,xlab="Myrmica abundance (log)",ylab="M. alcon presence",bty="l",pch=20,family="serif")
curve(predict(logistic, data.frame(logants=x), type="resp"), add=TRUE) 
legend("topleft", legend = "p = 0.045",bty = "n") 

with(ants_intensity,plot(logants,meaneggs,col = c("black","grey")[as.factor(year)],
     xlab="Myrmica abundance (log)",ylab="Mean number of eggs"))
with(ants_intensity,plot(logants,propattack,col = c("black","grey")[as.factor(year)],
     xlab="Myrmica abundance (log)",ylab="Proportion of plants attacked"))
###################################################################################

#Do butterflies reduce fitness?
#All pops
par(mfrow=c(1,2))
#2010
data10$predator<-as.factor(data10$predator)
summary(lm(n_intact_fruits~predator,data=data10))
plotMeans(data10$n_intact_fruits,data10$predator)

#2011
data11$Predator<-as.factor(data11$Predator)
summary(lm(n_intact_fruits~Predator,data=data11))
plotMeans(data11$n_intact_fruits,data11$Predator)

#Pops with predator
#2010
head(data10_pred)
data10_pred$attacked<-ifelse(data10_pred$n_eggs==0,0,1)

summary(lm(n_intact_fruits~attacked,data=data10_pred))
plotMeans(data10_pred$n_intact_fruits,data10_pred$attacked)

#2011
head(data11_pred)
data11_pred$attacked<-ifelse(data11_pred$n_eggs==0,0,1)

summary(lm(n_intact_fruits~attacked,data=data11_pred))
plotMeans(data11_pred$n_intact_fruits,data11_pred$attacked)

#Within each population
#2010
with(data10_pred,plotMeans(n_intact_fruits,attacked))

levels(data10_pred$LokalID)

par(mfrow=c(2,3))
with(subset(data10_pred,LokalID=="Ale001"),plotMeans(n_intact_fruits,attacked,main="Ale001"))
with(subset(data10_pred,LokalID=="Göt009a"),plotMeans(n_intact_fruits,attacked,main="Göt009a"))
with(subset(data10_pred,LokalID=="Göt009b"),plotMeans(n_intact_fruits,attacked,main="Göt009b"))
with(subset(data10_pred,LokalID=="Göt016"),plotMeans(n_intact_fruits,attacked,main="Göt016"))
with(subset(data10_pred,LokalID=="Her003"),plotMeans(n_intact_fruits,attacked,main="Her003"))
with(subset(data10_pred,LokalID=="Her004"),plotMeans(n_intact_fruits,attacked,main="Her004"))
with(subset(data10_pred,LokalID=="Her005"),plotMeans(n_intact_fruits,attacked,main="Her005"))
with(subset(data10_pred,LokalID=="Ler010"),plotMeans(n_intact_fruits,attacked,main="Ler010"))
with(subset(data10_pred,LokalID=="Par003"),plotMeans(n_intact_fruits,attacked,main="Par003"))
with(subset(data10_pred,LokalID=="Vår004"),plotMeans(n_intact_fruits,attacked,main="Vår004"))
with(subset(data10_pred,LokalID=="Vår009"),plotMeans(n_intact_fruits,attacked,main="Vår009"))

with(subset(data10_pred,LokalID=="Ale001"),summary(lm(n_intact_fruits~attacked)))
with(subset(data10_pred,LokalID=="Göt009a"),summary(lm(n_intact_fruits~attacked)))
with(subset(data10_pred,LokalID=="Göt009b"),summary(lm(n_intact_fruits~attacked)))
with(subset(data10_pred,LokalID=="Göt016"),summary(lm(n_intact_fruits~attacked)))
with(subset(data10_pred,LokalID=="Her003"),summary(lm(n_intact_fruits~attacked)))
with(subset(data10_pred,LokalID=="Her004"),summary(lm(n_intact_fruits~attacked)))
with(subset(data10_pred,LokalID=="Her005"),summary(lm(n_intact_fruits~attacked)))
with(subset(data10_pred,LokalID=="Ler010"),summary(lm(n_intact_fruits~attacked)))
with(subset(data10_pred,LokalID=="Par003"),summary(lm(n_intact_fruits~attacked)))
with(subset(data10_pred,LokalID=="Vår004"),summary(lm(n_intact_fruits~attacked)))
with(subset(data10_pred,LokalID=="Vår009"),summary(lm(n_intact_fruits~attacked)))

with(subset(data10_pred,LokalID=="Ale001"),summary(lm(n_intact_fruits~n_eggs)))
with(subset(data10_pred,LokalID=="Göt009a"),summary(lm(n_intact_fruits~n_eggs)))
with(subset(data10_pred,LokalID=="Göt009b"),summary(lm(n_intact_fruits~n_eggs)))
with(subset(data10_pred,LokalID=="Göt016"),summary(lm(n_intact_fruits~n_eggs)))
with(subset(data10_pred,LokalID=="Her003"),summary(lm(n_intact_fruits~n_eggs)))
with(subset(data10_pred,LokalID=="Her004"),summary(lm(n_intact_fruits~n_eggs)))
with(subset(data10_pred,LokalID=="Her005"),summary(lm(n_intact_fruits~n_eggs)))
with(subset(data10_pred,LokalID=="Ler010"),summary(lm(n_intact_fruits~n_eggs)))
with(subset(data10_pred,LokalID=="Par003"),summary(lm(n_intact_fruits~n_eggs)))
with(subset(data10_pred,LokalID=="Vår004"),summary(lm(n_intact_fruits~n_eggs)))
with(subset(data10_pred,LokalID=="Vår009"),summary(lm(n_intact_fruits~n_eggs)))

#2011
with(data11_pred,plotMeans(n_intact_fruits,attacked))

levels(data11_pred$LokalID)

par(mfrow=c(2,3))
with(subset(data11_pred,LokalID=="Ale001"),plotMeans(n_intact_fruits,attacked,main="Ale001"))
with(subset(data11_pred,LokalID=="Göt009a"),plotMeans(n_intact_fruits,attacked,main="Göt009a"))
with(subset(data11_pred,LokalID=="Göt009b"),plotMeans(n_intact_fruits,attacked,main="Göt009b"))
with(subset(data11_pred,LokalID=="Göt016"),plotMeans(n_intact_fruits,attacked,main="Göt016"))
with(subset(data11_pred,LokalID=="Her003"),plotMeans(n_intact_fruits,attacked,main="Her003"))
with(subset(data11_pred,LokalID=="Her004"),plotMeans(n_intact_fruits,attacked,main="Her004"))
with(subset(data11_pred,LokalID=="Her005"),plotMeans(n_intact_fruits,attacked,main="Her005"))
with(subset(data11_pred,LokalID=="Ler010"),plotMeans(n_intact_fruits,attacked,main="Ler010"))
with(subset(data11_pred,LokalID=="Par003"),plotMeans(n_intact_fruits,attacked,main="Par003"))
with(subset(data11_pred,LokalID=="Vår004"),plotMeans(n_intact_fruits,attacked,main="Vår004"))
with(subset(data11_pred,LokalID=="Vår009"),plotMeans(n_intact_fruits,attacked,main="Vår009"))

with(subset(data11_pred,LokalID=="Ale001"),summary(lm(n_intact_fruits~attacked)))
with(subset(data11_pred,LokalID=="Göt009a"),summary(lm(n_intact_fruits~attacked)))
with(subset(data11_pred,LokalID=="Göt009b"),summary(lm(n_intact_fruits~attacked)))
with(subset(data11_pred,LokalID=="Göt016"),summary(lm(n_intact_fruits~attacked)))
with(subset(data11_pred,LokalID=="Her003"),summary(lm(n_intact_fruits~attacked)))
with(subset(data11_pred,LokalID=="Her004"),summary(lm(n_intact_fruits~attacked)))
with(subset(data11_pred,LokalID=="Her005"),summary(lm(n_intact_fruits~attacked)))
with(subset(data11_pred,LokalID=="Ler010"),summary(lm(n_intact_fruits~attacked)))
with(subset(data11_pred,LokalID=="Par003"),summary(lm(n_intact_fruits~attacked)))
with(subset(data11_pred,LokalID=="Vår004"),summary(lm(n_intact_fruits~attacked)))
with(subset(data11_pred,LokalID=="Vår009"),summary(lm(n_intact_fruits~attacked)))

with(subset(data11_pred,LokalID=="Ale001"),summary(lm(n_intact_fruits~n_eggs)))
with(subset(data11_pred,LokalID=="Göt009a"),summary(lm(n_intact_fruits~n_eggs)))
with(subset(data11_pred,LokalID=="Göt009b"),summary(lm(n_intact_fruits~n_eggs)))
with(subset(data11_pred,LokalID=="Göt016"),summary(lm(n_intact_fruits~n_eggs)))
with(subset(data11_pred,LokalID=="Her003"),summary(lm(n_intact_fruits~n_eggs)))
with(subset(data11_pred,LokalID=="Her004"),summary(lm(n_intact_fruits~n_eggs)))
with(subset(data11_pred,LokalID=="Her005"),summary(lm(n_intact_fruits~n_eggs)))
with(subset(data11_pred,LokalID=="Ler010"),summary(lm(n_intact_fruits~n_eggs)))
with(subset(data11_pred,LokalID=="Par003"),summary(lm(n_intact_fruits~n_eggs)))
with(subset(data11_pred,LokalID=="Vår004"),summary(lm(n_intact_fruits~n_eggs)))
with(subset(data11_pred,LokalID=="Vår009"),summary(lm(n_intact_fruits~n_eggs)))

#Is probability of being attacked within populations with the predator associated with 
#flowering phenology? - Are early flowering plants more attacked than late flowering?
#All pops
#2010
logistic_10<-glm(attacked~most_adv,data=data10_pred,family=binomial)
summary(logistic_10)
with(data10_pred,plot(most_adv,attacked,xlab="Phenology",ylab="Attacked",main="ALL"))
curve(predict(logistic_10, data.frame(most_adv=x), type="resp"), add=TRUE) 

#2011
logistic_11<-glm(attacked~most_adv,data=data11_pred,family=binomial)
summary(logistic_11)
with(data11_pred,plot(most_adv,attacked,xlab="Phenology",ylab="Attacked",main="ALL"))
curve(predict(logistic_11, data.frame(most_adv=x), type="resp"), add=TRUE) 

#Within each population
#2010

levels(data10_pred$LokalID)

logistic_10_Ale001<-glm(attacked~most_adv,data=subset(data10_pred,LokalID=="Ale001"),family=binomial)
logistic_10_Göt009a<-glm(attacked~most_adv,data=subset(data10_pred,LokalID=="Göt009a"),family=binomial)
logistic_10_Göt009b<-glm(attacked~most_adv,data=subset(data10_pred,LokalID=="Göt009b"),family=binomial)
logistic_10_Göt016<-glm(attacked~most_adv,data=subset(data10_pred,LokalID=="Göt016"),family=binomial)
logistic_10_Her003<-glm(attacked~most_adv,data=subset(data10_pred,LokalID=="Her003"),family=binomial)
logistic_10_Her004<-glm(attacked~most_adv,data=subset(data10_pred,LokalID=="Her004"),family=binomial)
logistic_10_Her005<-glm(attacked~most_adv,data=subset(data10_pred,LokalID=="Her005"),family=binomial)
logistic_10_Ler010<-glm(attacked~most_adv,data=subset(data10_pred,LokalID=="Ler010"),family=binomial)
logistic_10_Par003<-glm(attacked~most_adv,data=subset(data10_pred,LokalID=="Par003"),family=binomial)
logistic_10_Vår004<-glm(attacked~most_adv,data=subset(data10_pred,LokalID=="Vår004"),family=binomial)
logistic_10_Vår009<-glm(attacked~most_adv,data=subset(data10_pred,LokalID=="Vår009"),family=binomial)

par(mfrow=c(2,3))
with(subset(data10_pred,LokalID=="Ale001"),plot(most_adv,attacked,xlab="Phenology",ylab="Attacked",main="Ale001"))
curve(predict(logistic_10_Ale001, data.frame(most_adv=x), type="resp"), add=TRUE) 
with(subset(data10_pred,LokalID=="Göt009a"),plot(most_adv,attacked,xlab="Phenology",ylab="Attacked",main="Göt009a"))
curve(predict(logistic_10_Göt009a, data.frame(most_adv=x), type="resp"), add=TRUE) 
with(subset(data10_pred,LokalID=="Göt009b"),plot(most_adv,attacked,xlab="Phenology",ylab="Attacked",main="Göt009b"))
curve(predict(logistic_10_Göt009b, data.frame(most_adv=x), type="resp"), add=TRUE) 
with(subset(data10_pred,LokalID=="Göt016"),plot(most_adv,attacked,xlab="Phenology",ylab="Attacked",main="Göt016"))
curve(predict(logistic_10_Göt016, data.frame(most_adv=x), type="resp"), add=TRUE) 
with(subset(data10_pred,LokalID=="Her003"),plot(most_adv,attacked,xlab="Phenology",ylab="Attacked",main="Her003"))
curve(predict(logistic_10_Her003, data.frame(most_adv=x), type="resp"), add=TRUE) 
with(subset(data10_pred,LokalID=="Her004"),plot(most_adv,attacked,xlab="Phenology",ylab="Attacked",main="Her004"))
curve(predict(logistic_10_Her004, data.frame(most_adv=x), type="resp"), add=TRUE) 
with(subset(data10_pred,LokalID=="Her005"),plot(most_adv,attacked,xlab="Phenology",ylab="Attacked",main="Her005"))
curve(predict(logistic_10_Her005, data.frame(most_adv=x), type="resp"), add=TRUE) 
with(subset(data10_pred,LokalID=="Ler010"),plot(most_adv,attacked,xlab="Phenology",ylab="Attacked",main="Ler010"))
curve(predict(logistic_10_Ler010, data.frame(most_adv=x), type="resp"), add=TRUE) 
with(subset(data10_pred,LokalID=="Par003"),plot(most_adv,attacked,xlab="Phenology",ylab="Attacked",main="Par003"))
curve(predict(logistic_10_Par003, data.frame(most_adv=x), type="resp"), add=TRUE) 
with(subset(data10_pred,LokalID=="Vår004"),plot(most_adv,attacked,xlab="Phenology",ylab="Attacked",main="Vår004"))
curve(predict(logistic_10_Vår004, data.frame(most_adv=x), type="resp"), add=TRUE) 
with(subset(data10_pred,LokalID=="Vår009"),plot(most_adv,attacked,xlab="Phenology",ylab="Attacked",main="Vår009"))
curve(predict(logistic_10_Vår009, data.frame(most_adv=x), type="resp"), add=TRUE) 

summary(logistic_10_Ale001)
summary(logistic_10_Göt009a)
summary(logistic_10_Göt009b)
summary(logistic_10_Göt016)
summary(logistic_10_Her003)
summary(logistic_10_Her004)
summary(logistic_10_Her005)
summary(logistic_10_Ler010)
summary(logistic_10_Par003)
summary(logistic_10_Vår004)
summary(logistic_10_Vår009)

#2011

levels(data11_pred$LokalID)

logistic_11_Ale001<-glm(attacked~most_adv,data=subset(data11_pred,LokalID=="Ale001"),family=binomial)
logistic_11_Göt009a<-glm(attacked~most_adv,data=subset(data11_pred,LokalID=="Göt009a"),family=binomial)
logistic_11_Göt009b<-glm(attacked~most_adv,data=subset(data11_pred,LokalID=="Göt009b"),family=binomial)
logistic_11_Göt016<-glm(attacked~most_adv,data=subset(data11_pred,LokalID=="Göt016"),family=binomial)
logistic_11_Her003<-glm(attacked~most_adv,data=subset(data11_pred,LokalID=="Her003"),family=binomial)
logistic_11_Her004<-glm(attacked~most_adv,data=subset(data11_pred,LokalID=="Her004"),family=binomial)
logistic_11_Her005<-glm(attacked~most_adv,data=subset(data11_pred,LokalID=="Her005"),family=binomial)
logistic_11_Ler010<-glm(attacked~most_adv,data=subset(data11_pred,LokalID=="Ler010"),family=binomial)
logistic_11_Par003<-glm(attacked~most_adv,data=subset(data11_pred,LokalID=="Par003"),family=binomial)
logistic_11_Vår004<-glm(attacked~most_adv,data=subset(data11_pred,LokalID=="Vår004"),family=binomial)
logistic_11_Vår009<-glm(attacked~most_adv,data=subset(data11_pred,LokalID=="Vår009"),family=binomial)

par(mfrow=c(2,3))
with(subset(data11_pred,LokalID=="Ale001"),plot(most_adv,attacked,xlab="Phenology",ylab="Attacked",main="Ale001"))
curve(predict(logistic_11_Ale001, data.frame(most_adv=x), type="resp"), add=TRUE) 
with(subset(data11_pred,LokalID=="Göt009a"),plot(most_adv,attacked,xlab="Phenology",ylab="Attacked",main="Göt009a"))
curve(predict(logistic_11_Göt009a, data.frame(most_adv=x), type="resp"), add=TRUE) 
with(subset(data11_pred,LokalID=="Göt009b"),plot(most_adv,attacked,xlab="Phenology",ylab="Attacked",main="Göt009b"))
curve(predict(logistic_11_Göt009b, data.frame(most_adv=x), type="resp"), add=TRUE) 
with(subset(data11_pred,LokalID=="Göt016"),plot(most_adv,attacked,xlab="Phenology",ylab="Attacked",main="Göt016"))
curve(predict(logistic_11_Göt016, data.frame(most_adv=x), type="resp"), add=TRUE) 
with(subset(data11_pred,LokalID=="Her003"),plot(most_adv,attacked,xlab="Phenology",ylab="Attacked",main="Her003"))
curve(predict(logistic_11_Her003, data.frame(most_adv=x), type="resp"), add=TRUE) 
with(subset(data11_pred,LokalID=="Her004"),plot(most_adv,attacked,xlab="Phenology",ylab="Attacked",main="Her004"))
curve(predict(logistic_11_Her004, data.frame(most_adv=x), type="resp"), add=TRUE) 
with(subset(data11_pred,LokalID=="Her005"),plot(most_adv,attacked,xlab="Phenology",ylab="Attacked",main="Her005"))
curve(predict(logistic_11_Her005, data.frame(most_adv=x), type="resp"), add=TRUE) 
with(subset(data11_pred,LokalID=="Ler010"),plot(most_adv,attacked,xlab="Phenology",ylab="Attacked",main="Ler010"))
curve(predict(logistic_11_Ler010, data.frame(most_adv=x), type="resp"), add=TRUE) 
with(subset(data11_pred,LokalID=="Par003"),plot(most_adv,attacked,xlab="Phenology",ylab="Attacked",main="Par003"))
curve(predict(logistic_11_Par003, data.frame(most_adv=x), type="resp"), add=TRUE) 
with(subset(data11_pred,LokalID=="Vår004"),plot(most_adv,attacked,xlab="Phenology",ylab="Attacked",main="Vår004"))
curve(predict(logistic_11_Vår004, data.frame(most_adv=x), type="resp"), add=TRUE) 
with(subset(data11_pred,LokalID=="Vår009"),plot(most_adv,attacked,xlab="Phenology",ylab="Attacked",main="Vår009"))
curve(predict(logistic_11_Vår009, data.frame(most_adv=x), type="resp"), add=TRUE) 

summary(logistic_11_Ale001)
summary(logistic_11_Göt009a)
summary(logistic_11_Göt009b)
summary(logistic_11_Göt016)
summary(logistic_11_Her003)
summary(logistic_11_Her004)
summary(logistic_11_Her005)
summary(logistic_11_Ler010)
summary(logistic_11_Par003)
summary(logistic_11_Vår004)
summary(logistic_11_Vår009)

############################################
with(data10_pred,summary(lm(n_intact_fruits~n_eggs)))
with(data10_pred,plot(n_eggs,n_intact_fruits))
with(data10_pred,abline(lm(n_intact_fruits~n_eggs)))

with(data11_pred,summary(lm(n_intact_fruits~n_eggs)))
with(data11_pred,plot(n_eggs,n_intact_fruits))
with(data11_pred,abline(lm(n_intact_fruits~n_eggs)))

data10_pred$most_adv<-as.numeric(data10_pred$most_adv)
with(data10_pred,summary(lm(n_eggs~most_adv)))
with(data10_pred,plot(most_adv,n_eggs))
with(data10_pred,abline(lm(n_eggs~most_adv)))
data10_pred$most_adv<-as.factor(data10_pred$most_adv)
with(data10_pred,plot(most_adv,n_eggs))
with(data10_pred,summary(lm(n_eggs~most_adv)))

data11_pred$most_adv<-as.numeric(data11_pred$most_adv)
with(data11_pred,summary(lm(n_eggs~most_adv)))
with(data11_pred,plot(most_adv,n_eggs))
with(data11_pred,abline(lm(n_eggs~most_adv)))
data11_pred$most_adv<-as.factor(data11_pred$most_adv)
with(data11_pred,plot(most_adv,n_eggs))
with(data11_pred,summary(lm(n_eggs~most_adv)))

