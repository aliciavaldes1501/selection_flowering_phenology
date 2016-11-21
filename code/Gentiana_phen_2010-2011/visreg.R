model1<-lm(n_intact_fruits~(most_adv+n_fl+h_shoot)*predator,data=data10sta)
model2<-lm(n_intact_fruits~(most_adv+n_fl+h_shoot+most_adv:n_fl+most_adv:h_shoot+n_fl:h_shoot)*predator,data=data10sta)
model3<-lm(n_intact_fruits~(most_adv+n_fl+h_shoot+I(most_adv^2)+I(n_fl^2)+I(h_shoot^2))*predator,data=data10sta)

library(visreg)
visreg(model1,xvar="most_adv",by="predator",overlay=T, partial=FALSE)
visreg(model1,xvar="most_adv",by="predator", partial=FALSE,band=FALSE)
visreg(model1,xvar="most_adv",by="predator",band=FALSE)

visreg2d(model2, "most_adv","h_shoot", plot.type = "image")
visreg2d(model2, "n_fl","h_shoot", plot.type = "image")

visreg2d(model2, "most_adv","n_fl", cond = list(predator = 1))
visreg2d(model2, "most_adv","n_fl", cond = list(predator = 0))

visreg(model3,xvar="most_adv",by="predator",overlay=TRUE, partial=F)
visreg(model3,xvar="n_fl",by="predator",overlay=TRUE, partial=F)




model1<-lm(n_intact_fruits~(most_adv+n_fl+h_shoot)*predator,data=data11sta)
model2<-lm(n_intact_fruits~(most_adv+n_fl+h_shoot+most_adv:n_fl+most_adv:h_shoot+n_fl:h_shoot)*predator,data=data11sta)
model3<-lm(n_intact_fruits~(most_adv+n_fl+h_shoot+I(most_adv^2)+I(n_fl^2)+I(h_shoot^2))*predator,data=data11sta)

visreg2d(model2, "most_adv","h_shoot", cond = list(predator = 1))
visreg2d(model2, "most_adv","h_shoot", cond = list(predator = 0))

visreg2d(model2, "n_fl","h_shoot", cond = list(predator = 1))
visreg2d(model2, "n_fl","h_shoot", cond = list(predator = 0))

visreg(model3,xvar="most_adv",by="predator",overlay=TRUE, partial=F)
visreg(model3,xvar="n_fl",by="predator",overlay=TRUE, partial=F)


int1_10<-lm(n_eggs~most_adv+n_fl+h_shoot+LokalID*most_adv+
              LokalID*n_fl+LokalID*h_shoot,data=subset(data10sta[1:2001,],predator==1& LokalID!="Göt016"))
Anova(int1_10,Type="II")

par(mfrow=c(2,2),family="serif",bty="L")
visreg(int1_10,xvar="most_adv",by="LokalID",overlay=TRUE, partial=F,band=F,
       xlab="Phenology (early flowering)",ylab="Number of eggs")

int2_10<-glm(attack~most_adv+n_fl+h_shoot+LokalID*most_adv+
           LokalID*n_fl+LokalID*h_shoot,
           data=subset(data10sta[1:2001,],predator==1 & LokalID!="Göt016"),family="binomial")

summary(glm(attack~most_adv+n_fl+h_shoot,
            data=subset(data10sta,predator==1 & LokalID!="Göt016"),family="binomial"))

Anova(int2_10,Type="II")

visreg(int2_10,xvar="most_adv",by="LokalID",scale = "response",overlay=TRUE, partial=F,band=FALSE,
       xlab="Phenology (early flowering)",ylab="Probability of attack")

int1_11<-lm(n_eggs~most_adv+n_fl+h_shoot+LokalID*most_adv+
              LokalID*n_fl+LokalID*h_shoot,data=subset(data11sta,predator==1))

Anova(int1_11,Type="II")

visreg(int1_11,xvar="most_adv",by="LokalID",overlay=TRUE, partial=F,band=FALSE,
       xlab="Phenology (early flowering)",ylab="Number of eggs")

int2_11<-glm(attack~most_adv+n_fl+h_shoot+LokalID*most_adv+
              LokalID*n_fl+LokalID*h_shoot,
             data=subset(data11sta,predator==1),family="binomial")

summary(glm(attack~most_adv+n_fl+h_shoot,
    data=subset(data11sta,predator==1),family="binomial"))

Anova(int2_11,Type="II")

visreg(int2_11,xvar="most_adv",by="LokalID",scale = "response",overlay=TRUE, partial=F,band=FALSE,
       xlab="Phenology (early flowering)",ylab="Probability of attack")



par(mfrow=c(2,2),family="serif",bty="L",
    oma = c(2,2,0,0) + 0.1,
    mar = c(4,4,1,1) + 0.1)

visreg(int2_10,xvar="most_adv",by="LokalID",scale = "response",overlay=TRUE, partial=F,band=FALSE,
       xlab="",ylab="Probability of attack",legend=F)
visreg(int2_11,xvar="most_adv",by="LokalID",scale = "response",overlay=TRUE, partial=F,band=FALSE,
       xlab="",ylab="",legend=F)
visreg(int1_10,xvar="most_adv",by="LokalID",overlay=TRUE, partial=F,band=F,
       xlab="Phenology (early flowering)",ylab="Number of eggs",legend=F)
visreg(int1_11,xvar="most_adv",by="LokalID",overlay=TRUE, partial=F,band=FALSE,
       xlab="Phenology (early flowering)",ylab="",legend=F)


visreg(int2_10,xvar="n_fl",by="LokalID",scale = "response",overlay=TRUE, partial=F,band=FALSE,
       xlab="",ylab="Probability of attack",legend=F)
visreg(int2_11,xvar="n_fl",by="LokalID",scale = "response",overlay=TRUE, partial=F,band=FALSE,
       xlab="",ylab="",legend=F)
visreg(int1_10,xvar="n_fl",by="LokalID",overlay=TRUE, partial=F,band=F,
       xlab="Number of flowers",ylab="Number of eggs",legend=F)
visreg(int1_11,xvar="n_fl",by="LokalID",overlay=TRUE, partial=F,band=FALSE,
       xlab="Number of flowers",ylab="",legend=F)


visreg(int2_10,xvar="h_shoot",by="LokalID",scale = "response",overlay=TRUE, partial=F,band=FALSE,
       xlab="",ylab="Probability of attack",legend=F)
visreg(int2_11,xvar="h_shoot",by="LokalID",scale = "response",overlay=TRUE, partial=F,band=FALSE,
       xlab="",ylab="",legend=F)
visreg(int1_10,xvar="h_shoot",by="LokalID",overlay=TRUE, partial=F,band=F,
       xlab="Shoot height",ylab="Number of eggs",legend=F)
visreg(int1_11,xvar="h_shoot",by="LokalID",overlay=TRUE, partial=F,band=FALSE,
       xlab="Shoot height",ylab="",legend=F)




