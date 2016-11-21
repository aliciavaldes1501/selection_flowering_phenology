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

data10sta<-data10[c(1:4,16,29:33,10:13,34,35)]
head(data10sta)
names(data10sta)<-c("LokalID","predator","Lokal","Individ","attack","n_shoots","h_shoot","n_fl","phen_index","most_adv",
                    "n_eggs","n_pred_all","n_aborted_all","perc_undeveloped","n_intact_fruits","n_seeds")
summary(data10sta,by="LokalID")

#Effects of traits on fitness - removing n_shoots, adding quadratic and interaction terms
library(car)

#Model with all populations, linear selection + interaction with population
modelfr_10<-lm(n_intact_fruits~most_adv+n_fl+h_shoot+
                 LokalID:most_adv+LokalID:n_fl+LokalID:h_shoot,data=data10sta)
Anova(modelfr_10,type="II")
summary(lm(n_intact_fruits~most_adv+n_fl+h_shoot,data=data10sta))   #Get estimate for h_shoot

#Model with all populations, linear selection + interaction between traits + interaction with population
modelfr_10_2<-lm(n_intact_fruits~most_adv+n_fl+h_shoot+
                 most_adv:n_fl+most_adv:h_shoot+n_fl:h_shoot+
                 LokalID:most_adv+LokalID:n_fl+LokalID:h_shoot,data=data10sta)
Anova(modelfr_10_2,type="II")   #interaction n_fl:h_shoot is significant

#Model with all populations, non-linear selection + interaction with population
modelfr_10_3<-lm(n_intact_fruits~most_adv+n_fl+h_shoot+
                   I(most_adv^2)+I(n_fl^2)+I(h_shoot^2)+
                   LokalID:most_adv+LokalID:n_fl+LokalID:h_shoot+
                   LokalID:I(most_adv^2)+LokalID:I(n_fl^2)+LokalID:I(h_shoot^2),
                 data=data10sta)
Anova(modelfr_10_3,type="II")

#Model with all populations, non-linear selection + interaction between traits + interaction with population -TOO COMPLICATED!
modelfr_10_4<-lm(n_intact_fruits~most_adv+n_fl+h_shoot+
                   I(most_adv^2)+I(n_fl^2)+I(h_shoot^2)+
                   most_adv:n_fl+most_adv:h_shoot+n_fl:h_shoot+
                   most_adv:I(n_fl^2)+most_adv:I(h_shoot^2)+n_fl:I(most_adv^2)+
                   n_fl:I(h_shoot^2)+h_shoot:I(most_adv^2)+h_shoot:I(n_fl^2)+
                   I(most_adv^2):I(n_fl^2)+I(most_adv^2):I(h_shoot^2)+I(n_fl^2):I(h_shoot^2)+                                   
                   LokalID:most_adv+LokalID:n_fl+LokalID:h_shoot+                                     
                   LokalID:I(most_adv^2)+LokalID:I(n_fl^2)+LokalID:I(h_shoot^2),
                   data=data10sta)
Anova(modelfr_10_4,type="II")



with(data10sta,plot(n_fl,n_intact_fruits))
abline(lm(n_intact_fruits~n_fl,data=data10sta))
summary(lm(n_intact_fruits~n_fl,data=data10sta))
summary(lm(n_intact_fruits~I(n_fl^2),data=data10sta))

ggplot(data10sta, aes(x = n_fl, y = n_intact_fruits)) + geom_point()+
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), size = 1)


#Models with predator term
#Linear selection + predator
data10sta$predator<-as.factor(data10sta$predator)
modelfr_10_p<-lm(n_intact_fruits~most_adv+n_fl+h_shoot+
                 predator:most_adv+predator:n_fl+predator:h_shoot,data=data10sta)
Anova(modelfr_10_p,type="II")
summary(lm(n_intact_fruits~most_adv+n_fl+h_shoot,data=data10sta))   #Get estimate for n_fl, h_shoot

#Linear selection + interaction between traits + interaction with predator
modelfr_10_2_p<-lm(n_intact_fruits~most_adv+n_fl+h_shoot+
                   most_adv:n_fl+most_adv:h_shoot+n_fl:h_shoot+
                   predator:most_adv+predator:n_fl+predator:h_shoot,data=data10sta)
Anova(modelfr_10_2_p,type="II")   #interaction n_fl:h_shoot AND most_adv:h_shoot are significant

#Non-linear selection + interaction with predator
modelfr_10_3_p<-lm(n_intact_fruits~most_adv+n_fl+h_shoot+
                   I(most_adv^2)+I(n_fl^2)+I(h_shoot^2)+
                     predator:most_adv+predator:n_fl+predator:h_shoot+
                     predator:I(most_adv^2)+predator:I(n_fl^2)+predator:I(h_shoot^2),
                 data=data10sta)
Anova(modelfr_10_3_p,type="II")

#Separately for pops with / without predator
data10sta_pred<-subset(data10sta,predator==1)
data10sta_npred<-subset(data10sta,predator==0)
data10sta_pred$LokalID <- factor(data10sta_pred$LokalID)
data10sta_npred$LokalID <- factor(data10sta_npred$LokalID)
levels(data10sta_pred$LokalID)
levels(data10sta_npred$LokalID)

#With predator, linear selection + interaction with population
modelfr_10_with<-lm(n_intact_fruits~most_adv+n_fl+h_shoot+
                 LokalID:most_adv+LokalID:n_fl+LokalID:h_shoot,data=data10sta_pred)
Anova(modelfr_10_with,type="II")
summary(lm(n_intact_fruits~most_adv+n_fl+h_shoot,data=data10sta_pred))   #Get estimate for h_shoot

#With predator, linear selection + interaction between traits + interaction with population
modelfr_10_2_with<-lm(n_intact_fruits~most_adv+n_fl+h_shoot+
                   most_adv:n_fl+most_adv:h_shoot+n_fl:h_shoot+
                   LokalID:most_adv+LokalID:n_fl+LokalID:h_shoot,data=data10sta_pred)
Anova(modelfr_10_2_with,type="II")   #interaction n_fl:h_shoot is significant

#With predator, non-linear selection + interaction with population
modelfr_10_3_with<-lm(n_intact_fruits~most_adv+n_fl+h_shoot+
                   I(most_adv^2)+I(n_fl^2)+I(h_shoot^2)+
                   LokalID:most_adv+LokalID:n_fl+LokalID:h_shoot+
                   LokalID:I(most_adv^2)+LokalID:I(n_fl^2)+LokalID:I(h_shoot^2),
                 data=data10sta_pred)
Anova(modelfr_10_3_with,type="II")
summary(lm(n_intact_fruits~most_adv+n_fl+h_shoot+
             I(most_adv^2)+I(n_fl^2)+I(h_shoot^2),
           data=data10sta_pred))   #Get estimate for I(most_adv^2)

#Without predator, linear selection + interaction with population
modelfr_10_without<-lm(n_intact_fruits~most_adv+n_fl+h_shoot+
                      LokalID:most_adv+LokalID:n_fl+LokalID:h_shoot,data=data10sta_npred)
Anova(modelfr_10_without,type="II")
summary(lm(n_intact_fruits~most_adv+n_fl+h_shoot,data=data10sta_npred))   #Get estimate for most_adv

#Without predator, linear selection + interaction between traits + interaction with population
modelfr_10_2_without<-lm(n_intact_fruits~most_adv+n_fl+h_shoot+
                        most_adv:n_fl+most_adv:h_shoot+n_fl:h_shoot+
                        LokalID:most_adv+LokalID:n_fl+LokalID:h_shoot,data=data10sta_npred)
Anova(modelfr_10_2_without,type="II")   #interaction most_adv:n_fl is significant

#Without predator, non-linear selection + interaction with population
modelfr_10_3_without<-lm(n_intact_fruits~most_adv+n_fl+h_shoot+
                        I(most_adv^2)+I(n_fl^2)+I(h_shoot^2)+
                        LokalID:most_adv+LokalID:n_fl+LokalID:h_shoot+
                        LokalID:I(most_adv^2)+LokalID:I(n_fl^2)+LokalID:I(h_shoot^2),
                      data=data10sta_npred)
Anova(modelfr_10_3_without,type="II")

#Linear selection gradients

library(lme4)
library(lattice)

data10staLokalID <- split(data10sta[1:2001,], data10sta[1:2001,]$LokalID)

lsel_fr_10 <- lapply(data10staLokalID, function (x) {
  lm(n_intact_fruits ~ most_adv+n_fl+h_shoot, 
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

summary(lm(most_adv~pred,data=lsel_grads_10))
plot(lsel_grads_10$pred,lsel_grads_10$most_adv,
     xlab="Predator",ylab="Selection gradient for phenology")
legend("topright", legend ="p <0.001",bty = "n")

library(Rcmdr)
summary(lm(h_shoot~pred,data=lsel_grads_10))

par(mfrow=c(1,1),bty="l",family="serif",col="black")

plotMeans(lsel_grads_10$most_adv,lsel_grads_10$pred,
          xlab="Predator",ylab="Selection gradient for phenology",main=NULL,
          xlim=c(0.5,2.5),ylim=c(-0.3,0.4))
legend("top", legend ="p < 0.001",bty = "n")

plotMeans(lsel_grads_10$h_shoot,lsel_grads_10$pred,
          xlab="Predator",ylab="Selection gradient for shoot height",main=NULL,
          xlim=c(0.5,2.5),ylim=c(-0.3,0.4))
legend("top", legend = "p = 0.850",bty = "n")

#Graphs with ggplot2

science_theme <- theme_bw(base_family = "serif")+theme(panel.grid.major = element_line(size = 0, color = "white"),
                                                       axis.line = element_line(size=.7, color = "black"), text = element_text(size=18),
                                                       axis.title.x = element_text(vjust=-0.50,size=18),axis.title.y = element_text(vjust=0.70,size=18),
                                                       axis.text.x=element_text(size=18),axis.text.y=element_text(size=18))+
  theme( plot.background = element_blank() ,panel.grid.major = element_blank() ,
         panel.grid.minor = element_blank() , panel.border = element_blank() ,
         panel.background = element_blank() ) +  theme(axis.line = element_line(color = 'black'))

df1<-lsel_grads_10
df1
df1s <- summarySE(df1, measurevar="most_adv", groupvars=c("pred"))
df1s1 <- summarySE(df1, measurevar="h_shoot", groupvars=c("pred"))

library(ggplot2)
ggplot(df1s, aes(x=pred, y=most_adv)) + 
  geom_errorbar(aes(ymin=most_adv-se, ymax=most_adv+se), width=.1,size=.7) +
  geom_line() +  geom_point(size=6)+science_theme+
  scale_y_continuous(limits = c(-0.3, 0.4))+
  labs(x = "Predator", y = "Selection gradient for phenology")

ggplot(df1s1, aes(x=pred, y=h_shoot)) + 
  geom_errorbar(aes(ymin=h_shoot-se, ymax=h_shoot+se), width=.1,size=.7) +
  geom_line() +  geom_point(size=6)+science_theme+
  scale_y_continuous(limits = c(-0.3, 0.4))+
    labs(x = "Predator", y = "Selection gradient for shoot height")

#Nonlinear selection gradients

qsel_fr_10 <- lapply(data10staLokalID, function (x) {
  lm(n_intact_fruits ~ most_adv+n_fl+h_shoot+
       I(most_adv^2)+I(n_fl^2)+I(h_shoot^2), 
     data = x)
})

qsel_fr_10

qselcoefs_fr_10 <- vector("list", 20) # create list

for (i in 1:20)  #this is 20 LokalID
{
  qselcoefs_fr_10[[i]]<-summary(qsel_fr_10[[i]])$coefficients
}

names(qselcoefs_fr_10)<-names(qsel_fr_10)
qselcoefs_fr_10
qselcoefs_fr_10<-do.call("rbind",lapply(qselcoefs_fr_10,FUN=data.frame))
qselcoefs_fr_10
#Add asterisks for significance
qselcoefs_fr_10$sig <- ifelse(qselcoefs_fr_10$Pr...t.. < 0.05,"*", "")
head(qselcoefs_fr_10)
write.table(qselcoefs_fr_10,file="qselcoefs_fr_10_most_adv.txt",sep="\t")

#Differences in nonlinear selection gradients between populations with/without predator
qsel_grads_10 <- read.table(
  "qsel_grads_most_adv.txt",
  header=TRUE, sep="\t", na.strings="NA", dec=",", strip.white=TRUE)
head(qsel_grads_10)
str(qsel_grads_10)
qsel_grads_10$pred<-as.factor(qsel_grads_10$pred)

summary(lm(most_adv2~pred,data=qsel_grads_10))
summary(lm(h_shoot2~pred,data=qsel_grads_10))
summary(lm(n_fl2~pred,data=qsel_grads_10))

#################################################################################

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
names(data11)[14:15]<-c("n_intact_fruits","n_seeds")
names(data11)[17:18]<-c("n_intact_fruits_mean","n_seeds_mean")

names(data11)[19:23]<-c("n_shoots_mean","h_shoot_mean","n_fl_mean","phen_index_mean","most_adv_mean")

data11<-merge(data11,sd_n_shoots, by="LokalID")
data11<-merge(data11,sd_h_shoot, by="LokalID")
data11<-merge(data11,sd_n_fl, by="LokalID")
data11<-merge(data11,sd_phen_index, by="LokalID")
data11<-merge(data11,sd_most_adv, by="LokalID")

names(data11)
names(data11)[5:9]<-c("n_shoots","h_shoot","n_fl","phen_index","most_adv")
names(data11)[24:28]<-c("n_shoots_sd","h_shoot_sd","n_fl_sd","phen_index_sd","most_adv_sd")

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

data11sta<-data11[c(1:4,16,29:33,10:13,34,35)]
head(data11sta)
names(data11sta)<-c("LokalID","predator","Lokal","Individ","attack","n_shoots","h_shoot","n_fl","phen_index","most_adv",
                    "n_eggs","n_pred_all","n_aborted_all","perc_undeveloped","n_intact_fruits","n_seeds")
summary(data11sta,by="LokalID")

#Effects of traits on fitness - removing n_shoots, adding quadratic and interaction terms
library(car)

#Model with all populations, linear selection + interaction with population
modelfr_11<-lm(n_intact_fruits~most_adv+n_fl+h_shoot+
                 LokalID:most_adv+LokalID:n_fl+LokalID:h_shoot,data=data11sta)
Anova(modelfr_11,type="II")

#Model with all populations, linear selection + interaction between traits + interaction with population
modelfr_11_2<-lm(n_intact_fruits~most_adv+n_fl+h_shoot+
                   most_adv:n_fl+most_adv:h_shoot+n_fl:h_shoot+
                   LokalID:most_adv+LokalID:n_fl+LokalID:h_shoot,data=data11sta)
Anova(modelfr_11_2,type="II")   #interaction n_fl:h_shoot is significant

#Model with all populations, non-linear selection + interaction with population
modelfr_11_3<-lm(n_intact_fruits~most_adv+n_fl+h_shoot+
                   I(most_adv^2)+I(n_fl^2)+I(h_shoot^2)+
                   LokalID:most_adv+LokalID:n_fl+LokalID:h_shoot+
                   LokalID:I(most_adv^2)+LokalID:I(n_fl^2)+LokalID:I(h_shoot^2),
                 data=data11sta)
Anova(modelfr_11_3,type="II")

#Model with all populations, non-linear selection + interaction between traits + interaction with population -TOO COMPLICATED!
modelfr_11_4<-lm(n_intact_fruits~most_adv+n_fl+h_shoot+
                   I(most_adv^2)+I(n_fl^2)+I(h_shoot^2)+
                   most_adv:n_fl+most_adv:h_shoot+n_fl:h_shoot+
                   most_adv:I(n_fl^2)+most_adv:I(h_shoot^2)+n_fl:I(most_adv^2)+
                   n_fl:I(h_shoot^2)+h_shoot:I(most_adv^2)+h_shoot:I(n_fl^2)+
                   I(most_adv^2):I(n_fl^2)+I(most_adv^2):I(h_shoot^2)+I(n_fl^2):I(h_shoot^2)+                                   
                   LokalID:most_adv+LokalID:n_fl+LokalID:h_shoot+                                     
                   LokalID:I(most_adv^2)+LokalID:I(n_fl^2)+LokalID:I(h_shoot^2),
                 data=data11sta)
Anova(modelfr_11_4,type="II")



with(data11sta,plot(n_fl,n_intact_fruits))
abline(lm(n_intact_fruits~n_fl,data=data11sta))
summary(lm(n_intact_fruits~n_fl,data=data11sta))
summary(lm(n_intact_fruits~I(n_fl^2),data=data11sta))

ggplot(data11sta, aes(x = n_fl, y = n_intact_fruits)) + geom_point()+
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), size = 1)


#Models with predator term
#Linear selection + predator
data11sta$predator<-as.factor(data11sta$predator)
modelfr_11_p<-lm(n_intact_fruits~most_adv+n_fl+h_shoot+
                   predator:most_adv+predator:n_fl+predator:h_shoot,data=data11sta)
Anova(modelfr_11_p,type="II")

#Linear selection + interaction between traits + interaction with predator
modelfr_11_2_p<-lm(n_intact_fruits~most_adv+n_fl+h_shoot+
                     most_adv:n_fl+most_adv:h_shoot+n_fl:h_shoot+
                     predator:most_adv+predator:n_fl+predator:h_shoot,data=data11sta)
Anova(modelfr_11_2_p,type="II")   

#Non-linear selection + interaction with predator
modelfr_11_3_p<-lm(n_intact_fruits~most_adv+n_fl+h_shoot+
                     I(most_adv^2)+I(n_fl^2)+I(h_shoot^2)+
                     predator:most_adv+predator:n_fl+predator:h_shoot+
                     predator:I(most_adv^2)+predator:I(n_fl^2)+predator:I(h_shoot^2),
                   data=data11sta)
Anova(modelfr_11_3_p,type="II")

#Separately for pops with / without predator
data11sta_pred<-subset(data11sta,predator==1)
data11sta_npred<-subset(data11sta,predator==0)
data11sta_pred$LokalID <- factor(data11sta_pred$LokalID)
data11sta_npred$LokalID <- factor(data11sta_npred$LokalID)
levels(data11sta_pred$LokalID)
levels(data11sta_npred$LokalID)

#With predator, linear selection + interaction with population
modelfr_11_with<-lm(n_intact_fruits~most_adv+n_fl+h_shoot+
                      LokalID:most_adv+LokalID:n_fl+LokalID:h_shoot,data=data11sta_pred)
Anova(modelfr_11_with,type="II")
summary(lm(n_intact_fruits~most_adv+n_fl+h_shoot,data=data11sta_pred))   #Get estimate for h_shoot

#With predator, linear selection + interaction between traits + interaction with population
modelfr_11_2_with<-lm(n_intact_fruits~most_adv+n_fl+h_shoot+
                        most_adv:n_fl+most_adv:h_shoot+n_fl:h_shoot+
                        LokalID:most_adv+LokalID:n_fl+LokalID:h_shoot,data=data11sta_pred)
Anova(modelfr_11_2_with,type="II")   #interactions n_fl:h_shoot AND most_adv:h_shoot are significant

#With predator, non-linear selection + interaction with population
modelfr_11_3_with<-lm(n_intact_fruits~most_adv+n_fl+h_shoot+
                        I(most_adv^2)+I(n_fl^2)+I(h_shoot^2)+
                        LokalID:most_adv+LokalID:n_fl+LokalID:h_shoot+
                        LokalID:I(most_adv^2)+LokalID:I(n_fl^2)+LokalID:I(h_shoot^2),
                      data=data11sta_pred)
Anova(modelfr_11_3_with,type="II")
summary(lm(n_intact_fruits~most_adv+n_fl+h_shoot+
             I(most_adv^2)+I(n_fl^2)+I(h_shoot^2),
           data=data11sta_pred))   

#Without predator, linear selection + interaction with population
modelfr_11_without<-lm(n_intact_fruits~most_adv+n_fl+h_shoot+
                         LokalID:most_adv+LokalID:n_fl+LokalID:h_shoot,data=data11sta_npred)
Anova(modelfr_11_without,type="II")
summary(lm(n_intact_fruits~most_adv+n_fl+h_shoot,data=data11sta_npred))   #Get estimate for most_adv and n_fl

#Without predator, linear selection + interaction between traits + interaction with population
modelfr_11_2_without<-lm(n_intact_fruits~most_adv+n_fl+h_shoot+
                           most_adv:n_fl+most_adv:h_shoot+n_fl:h_shoot+
                           LokalID:most_adv+LokalID:n_fl+LokalID:h_shoot,data=data11sta_npred)
Anova(modelfr_11_2_without,type="II")   #no significant interactions

#Without predator, non-linear selection + interaction with population
modelfr_11_3_without<-lm(n_intact_fruits~most_adv+n_fl+h_shoot+
                           I(most_adv^2)+I(n_fl^2)+I(h_shoot^2)+
                           LokalID:most_adv+LokalID:n_fl+LokalID:h_shoot+
                           LokalID:I(most_adv^2)+LokalID:I(n_fl^2)+LokalID:I(h_shoot^2),
                         data=data11sta_npred)
Anova(modelfr_11_3_without,type="II")

#Linear selection gradients

library(lme4)
library(lattice)

data11staLokalID <- split(data11sta, data11sta$LokalID)

lsel_fr_11 <- lapply(data11staLokalID, function (x) {
  lm(n_intact_fruits ~ most_adv+n_fl+h_shoot, 
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

#Differences in linear selection gradients between populations with/without predator
lsel_grads_11 <- read.table(
  "lsel_grads_11_most_adv.txt",
  header=TRUE, sep="\t", na.strings="NA", dec=",", strip.white=TRUE)
head(lsel_grads_11)
str(lsel_grads_11)
lsel_grads_11$pred<-as.factor(lsel_grads_11$pred)

par(mfrow=c(1,2))

summary(lm(most_adv~pred,data=lsel_grads_11))
plot(lsel_grads_11$pred,lsel_grads_11$most_adv,
     xlab="Predator",ylab="Selection gradient for phenology")
legend("topright", legend ="p <0.001",bty = "n")

library(Rcmdr)
summary(lm(h_shoot~pred,data=lsel_grads_11))

par(mfrow=c(1,1),bty="l",family="serif",col="black")

plotMeans(lsel_grads_11$most_adv,lsel_grads_11$pred,
          xlab="Predator",ylab="Selection gradient for phenology",main=NULL,
          xlim=c(0.5,2.5),ylim=c(-0.3,0.4))
legend("top", legend ="p < 0.001",bty = "n")

plotMeans(lsel_grads_11$h_shoot,lsel_grads_11$pred,
          xlab="Predator",ylab="Selection gradient for shoot height",main=NULL,
          xlim=c(0.5,2.5),ylim=c(-0.3,0.4))
legend("top", legend = "p = 0.046",bty = "n")

#Graphs with ggplot2

science_theme <- theme_bw(base_family = "serif")+theme(panel.grid.major = element_line(size = 0, color = "white"),
                                                       axis.line = element_line(size=.7, color = "black"), text = element_text(size=18),
                                                       axis.title.x = element_text(vjust=-0.50,size=18),axis.title.y = element_text(vjust=0.70,size=18),
                                                       axis.text.x=element_text(size=18),axis.text.y=element_text(size=18))+
  theme( plot.background = element_blank() ,panel.grid.major = element_blank() ,
         panel.grid.minor = element_blank() , panel.border = element_blank() ,
         panel.background = element_blank() ) +  theme(axis.line = element_line(color = 'black'))

df1<-lsel_grads_11
df1
df1s <- summarySE(df1, measurevar="most_adv", groupvars=c("pred"))
df1s1 <- summarySE(df1, measurevar="h_shoot", groupvars=c("pred"))

library(ggplot2)
ggplot(df1s, aes(x=pred, y=most_adv)) + 
  geom_errorbar(aes(ymin=most_adv-se, ymax=most_adv+se), width=.1,size=.7) +
  geom_line() +  geom_point(size=6)+science_theme+
  scale_y_continuous(limits = c(-0.3, 0.4))+
  labs(x = "Predator", y = "Selection gradient for phenology")

ggplot(df1s1, aes(x=pred, y=h_shoot)) + 
  geom_errorbar(aes(ymin=h_shoot-se, ymax=h_shoot+se), width=.1,size=.7) +
  geom_line() +  geom_point(size=6)+science_theme+
  scale_y_continuous(limits = c(-0.3, 0.4))+
  labs(x = "Predator", y = "Selection gradient for shoot height")

#Nonlinear selection gradients

qsel_fr_11 <- lapply(data11staLokalID, function (x) {
  lm(n_intact_fruits ~ most_adv+n_fl+h_shoot+
       I(most_adv^2)+I(n_fl^2)+I(h_shoot^2), 
     data = x)
})

qsel_fr_11

qselcoefs_fr_11 <- vector("list", 16) # create list

for (i in 1:16)  #this is 16 LokalID
{
  qselcoefs_fr_11[[i]]<-summary(qsel_fr_11[[i]])$coefficients
}

names(qselcoefs_fr_11)<-names(qsel_fr_11)
qselcoefs_fr_11
qselcoefs_fr_11<-do.call("rbind",lapply(qselcoefs_fr_11,FUN=data.frame))
qselcoefs_fr_11
#Add asterisks for significance
qselcoefs_fr_11$sig <- ifelse(qselcoefs_fr_11$Pr...t.. < 0.05,"*", "")
head(qselcoefs_fr_11)
write.table(qselcoefs_fr_11,file="qselcoefs_fr_11_most_adv.txt",sep="\t")

#Differences in nonlinear selection gradients between populations with/without predator
qsel_grads_11 <- read.table(
  "qsel_grads_11_most_adv.txt",
  header=TRUE, sep="\t", na.strings="NA", dec=",", strip.white=TRUE)
head(qsel_grads_11)
str(qsel_grads_11)
qsel_grads_11$pred<-as.factor(qsel_grads_11$pred)

summary(lm(most_adv2~pred,data=qsel_grads_11))
summary(lm(h_shoot2~pred,data=qsel_grads_11))
summary(lm(n_fl2~pred,data=qsel_grads_11))

##########################################################


#2010

#Effects of traits on interaction intensity
#Linear

int1_10<-lm(n_eggs~most_adv+n_fl+h_shoot+LokalID*most_adv+
              LokalID*n_fl+LokalID*h_shoot,data=subset(data10sta[1:2001,],predator==1&LokalID!="Göt016"))
Anova(int1_10,type="II")

#Random? - does not help a lot if we want to see differences between populations
summary(lmer(n_eggs~most_adv+n_fl+h_shoot+(1|LokalID)+
             (1|LokalID:most_adv)+(1|LokalID:n_fl)+(1|LokalID:h_shoot),
           data=subset(data10sta,predator==1)))
Anova(lmer(n_eggs~most_adv+n_fl+h_shoot+(1|LokalID)+
             (1|LokalID:most_adv)+(1|LokalID:n_fl)+(1|LokalID:h_shoot),
           data=subset(data10sta,predator==1)))

Anova(glm(attack~most_adv+n_fl+h_shoot+LokalID*most_adv+
           LokalID*n_fl+LokalID*h_shoot,data=subset(data10sta[1:2001,],predator==1&LokalID!="Göt016"),family="binomial"),type="II")
summary(glm(attack~most_adv+n_fl+h_shoot,data=subset(data10sta[1:2001,],predator==1&LokalID!="Göt016"),family="binomial"),type="II")

#Linear + interaction
Anova(lm(n_eggs~(most_adv+n_fl+h_shoot)*LokalID+
           (most_adv:n_fl+most_adv:h_shoot+n_fl:h_shoot)*LokalID,
           data=subset(data10sta,predator==1)),type="II")

Anova(lm(attack~(most_adv+n_fl+h_shoot)*LokalID+
           (most_adv:n_fl+most_adv:h_shoot+n_fl:h_shoot)*LokalID,
         data=subset(data10sta,predator==1)),type="II")

#Linear + quadratic
Anova(lm(n_eggs~most_adv+n_fl+h_shoot+
           LokalID*most_adv+LokalID*n_fl+LokalID*h_shoot+
           I(most_adv^2)+I(n_fl^2)+I(h_shoot^2)+
           LokalID:I(most_adv^2)+LokalID:I(n_fl^2)+LokalID:I(h_shoot^2),           
           data=subset(data10sta,predator==1)),type="II")

Anova(lm(attack~most_adv+n_fl+h_shoot+
           LokalID*most_adv+LokalID*n_fl+LokalID*h_shoot+
           I(most_adv^2)+I(n_fl^2)+I(h_shoot^2)+
           LokalID:I(most_adv^2)+LokalID:I(n_fl^2)+LokalID:I(h_shoot^2),           
         data=subset(data10sta,predator==1)),type="II")


#2011

#Effects of traits on interaction intensity
#Linear

int1_11<-lm(n_eggs~most_adv+n_fl+h_shoot+LokalID*most_adv+
              LokalID*n_fl+LokalID*h_shoot,data=subset(data11sta,predator==1))
Anova(int1_11,type="II")

Anova(glm(attack~most_adv+n_fl+h_shoot+LokalID*most_adv+
            LokalID*n_fl+LokalID*h_shoot,data=subset(data11sta,predator==1),family="binomial"),type="II")


#Linear + interaction
Anova(lm(n_eggs~(most_adv+n_fl+h_shoot)*LokalID+
           (most_adv:n_fl+most_adv:h_shoot+n_fl:h_shoot)*LokalID,
         data=subset(data11sta,predator==1)),type="II")

Anova(lm(attack~(most_adv+n_fl+h_shoot)*LokalID+
           (most_adv:n_fl+most_adv:h_shoot+n_fl:h_shoot)*LokalID,
         data=subset(data11sta,predator==1)),type="II")


summary(lm(attack~(most_adv+n_fl+h_shoot)^2,
         data=subset(data11sta,predator==1)),type="II")

#Linear + quadratic
Anova(lm(n_eggs~most_adv+n_fl+h_shoot+
           LokalID*most_adv+LokalID*n_fl+LokalID*h_shoot+
           I(most_adv^2)+I(n_fl^2)+I(h_shoot^2)+
           LokalID:I(most_adv^2)+LokalID:I(n_fl^2)+LokalID:I(h_shoot^2),           
         data=subset(data11sta,predator==1)),type="II")

Anova(lm(attack~most_adv+n_fl+h_shoot+
           LokalID*most_adv+LokalID*n_fl+LokalID*h_shoot+
           I(most_adv^2)+I(n_fl^2)+I(h_shoot^2)+
           LokalID:I(most_adv^2)+LokalID:I(n_fl^2)+LokalID:I(h_shoot^2),           
         data=subset(data11sta,predator==1)),type="II")

summary(lm(attack~most_adv+n_fl+h_shoot+
           I(most_adv^2)+I(n_fl^2)+I(h_shoot^2),           
         data=subset(data11sta,predator==1)))   
#Get estimate for I(most_adv^2) and I(n_fl^2)

#2010

#Effects of traits on interaction intensity: models for each population

data10sta_pred<-subset(data10sta[1:2001,],predator==1)
levels(data10sta_pred$LokalID)
data10sta_pred$LokalID <- factor(data10sta_pred$LokalID)
levels(data10sta_pred$LokalID)

data10sta_predLokalID <- split(data10sta_pred, data10sta_pred$LokalID)

int_1_10_pops <- lapply(data10sta_predLokalID, function (x) {
  lm(n_eggs ~ most_adv+n_fl+h_shoot, 
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

library(ggplot2)

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#CC6666", "#9999CC", "#66CC99")

ggplot(data10_pred, aes(x=most_adv, y=attack,color=LokalID)) +
  geom_smooth(method=lm, se=FALSE, size=0.7,fullrange=T)+ 
  scale_x_continuous(breaks=c(0,1,2,3,4,5,6))+
  labs(x = "Most advanced bud", y = "Number of eggs")+
  science_theme+scale_colour_manual(values=cbbPalette)

ggplot(data10_pred, aes(x=n_fl, y=n_eggs,color=LokalID)) +
  geom_smooth(method=lm, se=FALSE, size=0.7,fullrange=T)+ 
  labs(x = "Number of flowers", y = "Number of eggs")+
  science_theme+scale_colour_manual(values=cbbPalette)

#Significance?


phen10 <- lapply(data10sta_predLokalID, function (x) {
  lm(scale(n_eggs) ~ most_adv, 
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

#Effects of traits on interaction intensity

int1_11<-lm(n_eggs~most_adv+n_fl+h_shoot+LokalID*most_adv+
              LokalID*n_fl+LokalID*n_shoots+LokalID*h_shoot,data=subset(data11sta,predator==1))
Anova(int1_11,type="II")

#Effects of traits on interaction intensity: models for each population

data11sta_pred<-subset(data11sta,predator==1)
levels(data11sta_pred$LokalID)
data11sta_pred$LokalID <- factor(data11sta_pred$LokalID)
levels(data11sta_pred$LokalID)

data11sta_predLokalID <- split(data11sta_pred, data11sta_pred$LokalID)

int_1_11_pops <- lapply(data11sta_predLokalID, function (x) {
  lm(n_eggs ~ most_adv+n_fl+h_shoot, 
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

ggplot(data11_pred, aes(x=most_adv, y=n_eggs,color=LokalID)) +
  geom_smooth(method=lm, se=FALSE, size=1,fullrange=T)+ 
  scale_x_continuous(breaks=c(0,1,2,3,4,5,6))+
  labs(x = "Most advanced bud", y = "Number of eggs")+
  science_theme+scale_colour_manual(values=cbbPalette)

#Significance?
#2011
phen11 <- lapply(data11sta_predLokalID, function (x) {
  lm(scale(n_eggs) ~ most_adv, 
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

#2010
phen10bis <- lapply(data10sta_predLokalID, function (x) {
  glm(attack ~ most_adv, 
     family= binomial,data = x)
})

phen10biscoefs<-vector("list", 11) # create list

for (i in 1:11)  #this is 11 LokalID
{
  phen10biscoefs[[i]]<-summary(phen10bis[[i]])$coefficients
}

phen10biscoefs

names(phen10biscoefs)<-names(phen10bis)
phen10biscoefs
phen10biscoefs<-do.call("rbind",lapply(phen10biscoefs,FUN=data.frame))
phen10biscoefs
#Add asterisks for significance
phen10biscoefs$sig <- ifelse(phen10biscoefs$Pr...z.. < 0.05,"*", "")
phen10biscoefs

#2011
phen11bis <- lapply(data11sta_predLokalID, function (x) {
  glm(attack ~ most_adv, 
      family= binomial,data = x)
})

phen11biscoefs<-vector("list", 11) # create list

for (i in 1:11)  #this is 11 LokalID
{
  phen11biscoefs[[i]]<-summary(phen11bis[[i]])$coefficients
}

phen11biscoefs

names(phen11biscoefs)<-names(phen11bis)
phen11biscoefs
phen11biscoefs<-do.call("rbind",lapply(phen11biscoefs,FUN=data.frame))
phen11biscoefs
#Add asterisks for significance
phen11biscoefs$sig <- ifelse(phen11biscoefs$Pr...z.. < 0.05,"*", "")
phen11biscoefs


phen10biscoefs
phen11biscoefs #Copy to JMP!

########################################################################

#Predator preference coefficients (phen together with other traits)

data <- read.table(
  "data_JMP.txt",
  header=TRUE, sep="\t", na.strings="NA", dec=",", strip.white=TRUE)
data$year<-as.factor(data$year)

data_pred_10<-subset(data,predator==1&year==2010)
data_pred_10$LokalID <- factor(data_pred_10$LokalID)
data_pred_10LokalID <- split(data_pred_10, data_pred_10$LokalID)

data_pred_11<-subset(data,predator==1&year==2011)
data_pred_11$LokalID <- factor(data_pred_11$LokalID)
data_pred_11LokalID <- split(data_pred_11, data_pred_11$LokalID)

#2010
#Standardized
phen10 <- lapply(data_pred_10LokalID, function (x) {
  lm(n_eggs_sta ~ most_adv_sta+n_fl_sta+h_shoot_sta, 
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
#Standardized
phen11 <- lapply(data_pred_11LokalID, function (x) {
  lm(n_eggs_sta ~ most_adv_sta+n_fl_sta+h_shoot_sta, 
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


######################################################################


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
ants_intensity$propattack_prop<-ants_intensity$propattack/100

summary(lm(meaneggs~logants,data=subset(ants_intensity,year==2010)))
summary(lm(meaneggs~logants,data=subset(ants_intensity,year==2011)))
summary(lm(propattack~logants,data=subset(ants_intensity,year==2010)))
summary(lm(propattack~logants,data=subset(ants_intensity,year==2011)))               


par(mfrow=c(1,1),col="black",pch=20,family="serif",bty="l",cex=1.5,cex.axis=0.5,cex.lab=0.6,mar=c(4, 4, 0.5, 0.5) + 0.1)
plot(ants_mean_max$logants,ants_mean_max$pred,xlab="",ylab=expression(italic(M.alcon)~presence),
     bty="l",pch=20,family="serif",xlim=c(-4, 4))
curve(predict(logistic, data.frame(logants=x), type="resp"), add=TRUE) 

at1<-seq(from = -2, to = 2.5, by = 0.5)
with(ants_intensity,plot(logants,meaneggs,col = c("black","grey")[as.factor(year)],
                         xlab="",ylab="Mean number of eggs",xaxt="n",xlim=c(-1.6, 2.6), ylim=c(0, 14)))
axis(side = 1, at = at1)

with(ants_intensity,plot(logants,propattack_prop,col = c("black","grey")[as.factor(year)],
                         xlab=expression(italic(Myrmica)~"abundance (log)"),ylab="Proportion of plants attacked",
                         xaxt="n",xlim=c(-1.6, 2.6), ylim=c(0, 0.7)))
axis(side = 1, at = at1)
###################################################################################



