ants <- read.table(
  "ants_11.txt",
  header=TRUE, sep="\t", na.strings="NA", dec=",", strip.white=TRUE)

ants
str(ants)

#Differences in ant abundance between populations with/without predator

par(mfrow=c(1,2))

summary(lm(log(ants+1)~pred,data=ants))
plot(ants$pred,ants$ants)
plot(ants$pred,log(ants$ants+1),xlab="Predation",ylab="Abundance of ants (log)")
legend("topright", legend = "p =  0.032323",bty = "n")
library(Rcmdr)
plotMeans(log(ants$ants+1),ants$pred,xlab="Predator",ylab="Abundance of ants (log)")
legend("topright", legend = "p =  0.032323",bty = "n")

#Removing outlier
ants_sub<-subset(ants, ants<300)

summary(lm(log(ants+1)~pred,data=ants_sub))
plot(ants_sub$pred,ants_sub$ants)
plot(ants_sub$pred,log(ants_sub$ants+1),xlab="Predation",ylab="Abundance of ants (log)")
legend("topright", legend = "p = 0.003007",bty = "n")
plotMeans(log(ants_sub$ants+1),ants_sub$pred,xlab="Predator",ylab="Abundance of ants (log)")
legend("topright", legend = "p = 0.003007",bty = "n")


#The other way round: logistic regression
ants$logants<-log(ants$ants+1)
logistic<-glm(pred~logants,data=ants,family=binomial)
summary(logistic)
par(mfrow=c(1,2))
plot(ants$logants,ants$pred,xlab="Ant abundance (log)",ylab="Predator",
     main="Outlier included")
curve(predict(logistic, data.frame(logants=x), type="resp"), add=TRUE) 
legend("topright", legend = "p = 0.054",bty = "n")

#Removing outlier

ants_sub$pred<-as.numeric(ants_sub$pred)
ants_sub$logants<-log(ants_sub$ants+1)

logistic<-glm(pred~logants,data=ants_sub,family=binomial)
summary(logistic)
plot(ants_sub$logants,ants_sub$pred,xlab="Ant abundance (log)",ylab="Predator",
     main="Outlier removed")
curve(predict(logistic, data.frame(logants=x), type="response"), add=TRUE) 
legend("topright", legend = "p = 0.0223",bty = "n")


#Effects of ants on interaction intensity

names(ants)<-c("LokalID","ants","pred","logants")
data11sta_ants<-merge(data11sta,ants,by="LokalID")
head(data11sta_ants)

int1_11_ants<-glm(n_eggs~ants,data=subset(data11sta_ants,predator==1),family=poisson)
Anova(int1_11_ants,type="II")
summary(int1_11_ants)

par(mfrow=c(1,2))

plot(subset(data11sta_ants,predator==1)$ants,subset(data11sta_ants,predator==1)$n_eggs,
     xlab="Abundance of ants",ylab="Number of eggs")
abline(glm(n_eggs~ants,data=subset(data11sta_ants,predator==1),family=poisson))
legend("topright", legend = "p < 0.001",bty = "n")

ants_pred<-subset(ants,pred==1)

data11_pred_means_bis<-merge(data11_pred_means,ants_pred,by="LokalID")

plot(data11_pred_means_bis$ants,data11_pred_means_bis$n_eggs,
     xlab="Abundance of ants",ylab="Mean number of eggs")
summary(lm(data11_pred_means_bis$n_eggs~data11_pred_means_bis$ants))
abline(lm(data11_pred_means_bis$n_eggs~data11_pred_means_bis$ants))
legend("topright", legend = "p = 0.756",bty = "n")

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



