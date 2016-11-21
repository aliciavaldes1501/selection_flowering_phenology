#Differences in linear selection gradients between populations with/without predator
lsel_grads_both <- read.table(
  "lsel_grads_both.txt",
  header=TRUE, sep="\t", na.strings="NA", dec=",", strip.white=TRUE)
head(lsel_grads_both)
str(lsel_grads_both)
lsel_grads_both$pred<-as.factor(lsel_grads_both$pred)

par(mfrow=c(1,1))

boxplot(phenology~pred*year, data=subset(lsel_grads_both,phen_measure="phen_index"),
        xlab="Predator",ylab="Selection gradient for phen_index")

qplot(pred, phenology, fill=factor(year), data=subset(lsel_grads_both,phen_measure="phen_index"), geom="boxplot", position="dodge")+theme_bw()

bwplot(phenology ~ pred | year, data=subset(lsel_grads_both,phen_measure="phen_index"))

df<-subset(lsel_grads_both,phen_measure=="most_adv")
df
df$year<-as.factor(df$year)
str(df)
df1 <- summarySE(df, measurevar="phenology", groupvars=c("pred","year"))
df2 <- summarySE(df, measurevar="h_shoot", groupvars=c("pred","year"))

ggplot(df1, aes(x=pred, y=phenology,colour=year)) + 
  geom_errorbar(aes(ymin=phenology-se, ymax=phenology+se), width=.1) +
  geom_line() +  geom_point()+science_theme

#Ants with mean value for both years

ants_mean_max <- read.table(
  "ants_mean_max.txt",
  header=TRUE, sep="\t", na.strings="NA", dec=",", strip.white=TRUE)

ants_mean_max
str(ants_mean_max)
ants_mean_max$pred<-as.factor(ants_mean_max$pred)

#Differences in ant abundance between populations with/without predator

par(mfrow=c(1,2))

summary(lm(log(ants_max+1)~pred,data=ants_mean_max))
plot(ants_mean_max$pred,ants_mean_max$ants_max)
plot(ants_mean_max$pred,log(ants_mean_max$ants_max+1),xlab="Predation",
     ylab="Abundance of ants (log)",main="Outlier included")
legend("topright", legend = "p =  0.1211",bty = "n")
library(Rcmdr)
plotMeans(log(ants_mean_max$ants_max+1),ants_mean_max$pred,xlab="Predator",ylab="Abundance of ants (log)")
legend("topright", legend = "p =  0.1211",bty = "n")

#Removing outlier
ants_mean_sub<-subset(ants_mean_max,pop!="Mar001")

summary(lm(log(ants_max+1)~pred,data=ants_mean_sub))
plot(ants_mean_sub$pred,ants_mean_sub$ants_max)
plot(ants_mean_sub$pred,log(ants_mean_sub$ants_max+1),xlab="Predation",
     ylab="Abundance of ants (log)",main="Outlier removed")
legend("topright", legend = "p = 0.00359",bty = "n")
plotMeans(log(ants_mean_sub$ants_max+1),ants_mean_sub$pred,xlab="Predator",ylab="Abundance of ants (log)")
legend("topright", legend = "p = 0.00359",bty = "n")

#The other way round: logistic regression
ants_mean_max$logants<-log(ants_mean_max$ants_max+0.025)
logistic<-glm(pred~logants,data=ants_mean_max,family=binomial)
summary(logistic)
par(mfrow=c(1,1),family="serif")
plot(ants_mean_max$logants,ants_mean_max$pred,xlab="Myrmica abundance (log)",ylab="M. alcon presence",bty="l",pch=20,family="serif")
curve(predict(logistic, data.frame(logants=x), type="resp"), add=TRUE) 
legend("topleft", legend = "p = 0.045",bty = "n")   #<---------------------

#Removing outlier

ants_mean_sub$logants<-log(ants_mean_sub$ants_max+1)
ants_mean_sub$logants11<-log(ants_mean_sub$ants_11+1)

par(mfrow=c(1,2),family="serif",bty="l",pch=20)

logistic<-glm(pred~logants,data=ants_mean_sub,family=binomial)
summary(logistic)
plot(ants_mean_sub$logants,ants_mean_sub$pred,xlab="Myrmica abundance (log)",ylab="M. alcon presence",
     main="Max of both years")
curve(predict(logistic, data.frame(logants=x), type="response"), add=TRUE) 
legend("topright", legend = "p = 0.0272",bty = "n")     #<-- KEEP!

logistic<-glm(pred~logants11,data=ants_mean_sub,family=binomial)
summary(logistic)
plot(ants_mean_sub$logants11,ants_mean_sub$pred,xlab="Myrmica abundance (log)",ylab="M. alcon presence",
     main="2011")
curve(predict(logistic, data.frame(logants11=x), type="response"), add=TRUE) 
legend("topright", legend = "p = 0.0219",bty = "n")     #<-- KEEP!


logistic<-glm(pred~logants,data=ants_mean_max,family=binomial)
summary(logistic)
plot(ants_mean_max$logants,ants_mean_max$pred,xlab="Myrmica abundance (log)",ylab="M. alcon presence",
     main="Max of both years")
curve(predict(logistic, data.frame(logants=x), type="response"), add=TRUE) 
legend("topright", legend = "p = 0.134",bty = "n")     #<-- KEEP!

ants_mean_max$logants11<-log(ants_mean_max$ants_11+0.025)
logistic<-glm(pred~logants11,data=ants_mean_max,family=binomial)
summary(logistic)
plot(ants_mean_max$logants11,ants_mean_max$pred,xlab="Myrmica abundance (log)",ylab="M. alcon presence",
     main="2011")
curve(predict(logistic, data.frame(logants11=x), type="response"), add=TRUE) 
legend("topright", legend = "p = 0.152",bty = "n")     #<-- KEEP!

ants_mean_max$antsarea<-ants_mean_max$ants_max*ants_mean_max$area
ants_mean_max$logantsarea<-log(ants_mean_max$antsarea+1)
logistic<-glm(pred~logants,data=ants_mean_max,family=binomial)
logistic<-glm(pred~logants+log(area),data=ants_mean_max,family=binomial)
logistic<-glm(pred~logants+log(area)+logants:log(area),data=ants_mean_max,family=binomial)
logistic<-glm(pred~logants+logants:log(area),data=ants_mean_max,family=binomial)
logistic<-glm(pred~logantsarea,data=ants_mean_max,family=binomial)

#TRY using the most common interval for 2011 or something like that
#Or dividing abundance in intervals again...

summary(logistic)
plot(ants_mean_max$logantsarea,ants_mean_max$pred,xlab="Myrmica abundance * area (log)",ylab="M. alcon presence",
     main="2011")
curve(predict(logistic, data.frame(logantsarea=x), type="response"), add=TRUE) 
legend("topright", legend = "p = 0.0295",bty = "n")     #<-- KEEP!


#Effects of ants on interaction intensity

names(ants_mean_max)
ants_mean_max_merge<-subset(ants_mean_max[c(1,4,6,8)])
names(ants_mean_max_merge)<-c("LokalID","pred","ants","logants")
data10sta_ants<-merge(data10sta,ants_mean_max_merge,by="LokalID")
head(data10sta_ants)

int1_10_ants<-glm(n_eggs~ants,data=subset(data10sta_ants,predator==1),family=poisson)
Anova(int1_10_ants,type="II")
summary(int1_10_ants)

par(mfrow=c(1,2))

plot(subset(data10sta_ants,predator==1)$ants,subset(data10sta_ants,predator==1)$n_eggs,
     xlab="Abundance of ants",ylab="Number of eggs")
abline(glm(n_eggs~ants,data=subset(data10sta_ants,predator==1),family=poisson))
legend("topright", legend = "p < 0.001",bty = "n")

ants_pred<-subset(ants,pred==1)

data10_pred_means_bis<-merge(data10_pred_means,ants_pred,by="LokalID")

plot(data10_pred_means_bis$log(ants+1),data10_pred_means_bis$n_eggs,
     xlab="Abundance of ants",ylab="Mean number of eggs")
summary(lm(data10_pred_means_bis$n_eggs~data10_pred_means_bis$ants))
abline(lm(data10_pred_means_bis$n_eggs~data10_pred_means_bis$ants))
legend("topright", legend = "p = 0.7081",bty = "n")

par(mfrow=c(1,2))

plot(log(subset(data10sta_ants,predator==1)$ants+1),subset(data10sta_ants,predator==1)$n_eggs,
     xlab="Abundance of ants (log)",ylab="Number of eggs")
abline(glm(n_eggs~log(ants+1),data=subset(data10sta_ants,predator==1),family=poisson))
legend("topright", legend = "p < 0.001",bty = "n")

ants_pred<-subset(ants,pred==1)

data10_pred_means_bis<-merge(data10_pred_means,ants_pred,by="LokalID")

plot(log(data10_pred_means_bis$ants+1),data10_pred_means_bis$n_eggs,
     xlab="Abundance of ants (log)",ylab="Mean number of eggs")
summary(lm(data10_pred_means_bis$n_eggs~log(data10_pred_means_bis$ants+1)))
abline(lm(data10_pred_means_bis$n_eggs~log(data10_pred_means_bis$ants+1)))
legend("topright", legend = "p = 0.618",bty = "n")

#Ants with max value for both years

ants_max <- read.table(
  "ants_mean_max.txt",
  header=TRUE, sep="\t", na.strings="NA", dec=",", strip.white=TRUE)

ants_max
str(ants_max)

#The other way round: logistic regression
ants_max$logants<-log(ants_max$ants_max+1)
logistic<-glm(pred~logants,data=ants_max,family=binomial)
summary(logistic)
par(mfrow=c(1,2))
plot(ants_max$logants,ants_max$pred_11,xlab="Ant abundance (log)",ylab="Predator",
     main="Using max of ant abundances")
curve(predict(logistic, data.frame(logants=x), type="resp"), add=TRUE) 
legend("topright", legend = "p = 0.0453",bty = "n")

#Ants and area 

ants_mean_max <- read.table(
  "ants_mean_max.txt",
  header=TRUE, sep="\t", na.strings="NA", dec=",", strip.white=TRUE)

ants_mean_max
str(ants_mean_max)

ants_mean_max<-subset(ants_mean_max,pop!="Mar001")
#Logistic regression
ants_mean_max$logants<-log(ants_mean_max$ants_max+1)
ants_mean_max$logarea<-log(ants_mean_max$area)

logistic<-glm(pred~logarea,data=ants_mean_max,family=binomial)
summary(logistic)
par(mfrow=c(1,2))
plot(ants_mean_max$logarea,ants_mean_max$pred,xlab="Patch area (log)",
     ylab="Predator")
curve(predict(logistic, data.frame(logarea=x), type="resp"), add=TRUE) 
legend("topright", legend = "p = 0.0211",bty = "n")

logistic<-glm(pred~logarea+logants,data=ants_mean_max,family=binomial)
summary(logistic)
par(mfrow=c(1,2))
plot(ants_mean_max$logarea,ants_mean_max$pred,xlab="Patch area (log)",
     ylab="Predator")
curve(predict(logistic, data.frame(logarea=x), type="resp"), add=TRUE) 
legend("topright", legend = "p = 0.0211",bty = "n")

ants_mean_max$logants<-log(ants_mean_max$ants_max+1)
logistic<-glm(pred~logants,data=ants_mean_max,family=binomial)
summary(logistic)
plot(ants_mean_max$logants,ants_mean_max$pred,xlab="Ant abundance (log)",
     ylab="Butterfly presence")
curve(predict(logistic, data.frame(logants=x), type="resp"), add=TRUE) 
legend("topleft", legend = "p = 0.0272",bty = "n")


logistic<-glm(pred~sqrt(ants_max),data=ants_mean_max,family=binomial)
summary(logistic)



#Differences between years
data1_both <- read.table(
  "data1_both.txt",
  header=TRUE, sep="\t", na.strings="NA", dec=",", strip.white=TRUE)
head(data1_both)
data1_both$year<-as.factor(data1_both$year)
str(data1_both)

#Differences in fitness
summary(lm(n_intact_fruits~year,data=data1_both))
plot(data1_both$year,data1_both$n_intact_fruits)
plotMeans(data1_both$n_intact_fruits,data1_both$year,xlab="Year",
          ylab="n_intact_fruits")

summary(lm(n_intact_fruits~year*LokalID,data=data1_both))
Anova(lm(n_intact_fruits~year*LokalID,data=data1_both))

summary(lm(n_seeds~year,data=data1_both))
plot(data1_both$year,data1_both$n_seeds)
plotMeans(data1_both$n_seeds,data1_both$year,xlab="Year",
          ylab="n_seeds")

#Differences in traits
summary(lm(n_shoots~year,data=data1_both))
plot(data1_both$year,data1_both$n_shoots)
plotMeans(data1_both$n_shoots,data1_both$year,xlab="Year",
          ylab="n_shoots")

summary(lm(h_shoot~year,data=data1_both))
plot(data1_both$year,data1_both$h_shoot)
plotMeans(data1_both$h_shoot,data1_both$year,xlab="Year",
          ylab="h_shoot")

summary(lm(n_fl~year,data=data1_both)) #NS
plot(data1_both$year,data1_both$n_fl)
plotMeans(data1_both$n_fl,data1_both$year,xlab="Year",
          ylab="n_fl")

summary(lm(phen_index~year,data=data1_both))
plot(data1_both$year,data1_both$phen_index)
plotMeans(data1_both$phen_index,data1_both$year,xlab="Year",
          ylab="phen_index")

summary(lm(phen_index~year*LokalID,data=data1_both))
Anova(lm(phen_index~year*LokalID,data=data1_both))

summary(lm(most_adv~year,data=data1_both))
plot(data1_both$year,data1_both$most_adv)
plotMeans(data1_both$most_adv,data1_both$year,xlab="Year",
          ylab="most_adv")

#Differences in interaction intensity
summary(lm(n_eggs~year,data=data1_both))
plot(data1_both$year,data1_both$n_eggs)
plotMeans(data1_both$n_eggs,data1_both$year,xlab="Year",
          ylab="n_eggs")

summary(lm(n_pred_all~year,data=data1_both)) #NS
plot(data1_both$year,data1_both$n_pred_all)
plotMeans(data1_both$n_pred_all,data1_both$year,xlab="Year",
          ylab="n_pred_all")

summary(lm(n_aborted_all~year,data=data1_both))
plot(data1_both$year,data1_both$n_aborted_all)
plotMeans(data1_both$n_aborted_all,data1_both$year,xlab="Year",
          ylab="n_aborted_all")






