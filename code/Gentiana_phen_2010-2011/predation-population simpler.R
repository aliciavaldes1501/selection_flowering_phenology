#Effects of traits on fitness
library(car)

#2010

#Linear selection

model1<-lm(n_intact_fruits~(most_adv+n_fl+h_shoot)*predator,data=data10sta)
model1p<-lm(n_intact_fruits~most_adv+n_fl+h_shoot+most_adv:LokalID+n_fl:LokalID+h_shoot:LokalID,data=subset(data10sta,predator==1))
model1np<-lm(n_intact_fruits~most_adv+n_fl+h_shoot+most_adv:LokalID+n_fl:LokalID+h_shoot:LokalID,data=subset(data10sta,predator==0))
Anova(model1,type="II")
Anova(model1p,type="II")
Anova(model1np,type="II")

summary(lm(n_intact_fruits~most_adv+n_fl+h_shoot,data=data10sta))
summary(lm(n_intact_fruits~most_adv+n_fl+h_shoot,data=subset(data10sta,predator==1)))
summary(lm(n_intact_fruits~most_adv+n_fl+h_shoot,data=subset(data10sta,predator==0)))


#Linear selection + interaction between traits

model2<-lm(n_intact_fruits~(most_adv+n_fl+h_shoot+most_adv:n_fl+most_adv:h_shoot+n_fl:h_shoot)*predator,data=data10sta)
Anova(model2,type="II")
summary(lm(n_intact_fruits~most_adv+n_fl+h_shoot+most_adv:n_fl+most_adv:h_shoot+n_fl:h_shoot,data=data10sta))

model2p<-lm(n_intact_fruits~most_adv+n_fl+h_shoot+most_adv:n_fl+most_adv:h_shoot+n_fl:h_shoot+
            most_adv:LokalID+n_fl:LokalID+h_shoot:LokalID+most_adv:n_fl:LokalID+most_adv:h_shoot:LokalID+n_fl:h_shoot:LokalID,
            data=subset(data10sta,predator==1))
model2np<-lm(n_intact_fruits~most_adv+n_fl+h_shoot+most_adv:n_fl+most_adv:h_shoot+n_fl:h_shoot+
            most_adv:LokalID+n_fl:LokalID+h_shoot:LokalID+most_adv:n_fl:LokalID+most_adv:h_shoot:LokalID+n_fl:h_shoot:LokalID,
            data=subset(data10sta,predator==0))

Anova(model2p,type="II")
Anova(model2np,type="II")

#Linear effects + quadratic effects
most_adv_2<-I(data10sta$most_adv^2)
n_fl_2<-I(data10sta$n_fl^2)
h_shoot_2<-I(data10sta$h_shoot^2)
most_adv_2p<-I(subset(data10sta,predator==1)$most_adv^2)
n_fl_2p<-I(subset(data10sta,predator==1)$n_fl^2)
h_shoot_2p<-I(subset(data10sta,predator==1)$h_shoot^2)
most_adv_2np<-I(subset(data10sta,predator==0)$most_adv^2)
n_fl_2np<-I(subset(data10sta,predator==0)$n_fl^2)
h_shoot_2np<-I(subset(data10sta,predator==0)$h_shoot^2)

model3<-lm(n_intact_fruits~(most_adv+n_fl+h_shoot+most_adv_2+n_fl_2+h_shoot_2)*predator,data=data10sta)
Anova(model3)
summary(lm(n_intact_fruits~most_adv+n_fl+h_shoot+most_adv_2+n_fl_2+h_shoot_2,data=data10sta))

model3p<-lm(n_intact_fruits~most_adv+n_fl+h_shoot+
            most_adv_2p+n_fl_2p+h_shoot_2p+
            most_adv:LokalID+n_fl:LokalID+h_shoot:LokalID+
            most_adv_2p:LokalID+n_fl_2p:LokalID+h_shoot_2p:LokalID,
            data=subset(data10sta,predator==1))
Anova(model3p)
summary(lm(n_intact_fruits~most_adv+n_fl+h_shoot+
             most_adv_2p+n_fl_2p+h_shoot_2p,
           data=subset(data10sta,predator==1)))

model3np<-lm(n_intact_fruits~most_adv+n_fl+h_shoot+
              most_adv_2np+n_fl_2np+h_shoot_2np+
              most_adv:LokalID+n_fl:LokalID+h_shoot:LokalID+
              most_adv_2np:LokalID+n_fl_2np:LokalID+h_shoot_2np:LokalID,
            data=subset(data10sta,predator==0))
Anova(model3np)



#2011

#Linear selection

model1<-lm(n_intact_fruits~(most_adv+n_fl+h_shoot)*predator,data=data11sta)
model1p<-lm(n_intact_fruits~most_adv+n_fl+h_shoot+most_adv:LokalID+n_fl:LokalID+h_shoot:LokalID,data=subset(data11sta,predator==1))
model1np<-lm(n_intact_fruits~most_adv+n_fl+h_shoot+most_adv:LokalID+n_fl:LokalID+h_shoot:LokalID,data=subset(data11sta,predator==0))
Anova(model1,type="II")
Anova(model1p,type="II")
Anova(model1np,type="II")
summary(lm(n_intact_fruits~most_adv+n_fl+h_shoot,data=subset(data11sta,predator==1)))
summary(lm(n_intact_fruits~most_adv+n_fl+h_shoot,data=subset(data11sta,predator==0)))

#Linear selection + interaction between traits

model2<-lm(n_intact_fruits~(most_adv+n_fl+h_shoot+most_adv:n_fl+most_adv:h_shoot+n_fl:h_shoot)*predator,data=data11sta)
Anova(model2,type="II")

model2p<-lm(n_intact_fruits~most_adv+n_fl+h_shoot+most_adv:n_fl+most_adv:h_shoot+n_fl:h_shoot+
              most_adv:LokalID+n_fl:LokalID+h_shoot:LokalID+most_adv:n_fl:LokalID+most_adv:h_shoot:LokalID+n_fl:h_shoot:LokalID,
            data=subset(data11sta,predator==1))
model2np<-lm(n_intact_fruits~most_adv+n_fl+h_shoot+most_adv:n_fl+most_adv:h_shoot+n_fl:h_shoot+
               most_adv:LokalID+n_fl:LokalID+h_shoot:LokalID+most_adv:n_fl:LokalID+most_adv:h_shoot:LokalID+n_fl:h_shoot:LokalID,
             data=subset(data11sta,predator==0))

Anova(model2p,type="II")
Anova(model2np,type="II")
summary(lm(n_intact_fruits~most_adv+n_fl+h_shoot+most_adv:n_fl+most_adv:h_shoot+n_fl:h_shoot,
                    data=subset(data11sta,predator==1)))
#Linear effects + quadratic effects
most_adv_2<-I(data11sta$most_adv^2)
n_fl_2<-I(data11sta$n_fl^2)
h_shoot_2<-I(data11sta$h_shoot^2)
most_adv_2p<-I(subset(data11sta,predator==1)$most_adv^2)
n_fl_2p<-I(subset(data11sta,predator==1)$n_fl^2)
h_shoot_2p<-I(subset(data11sta,predator==1)$h_shoot^2)
most_adv_2np<-I(subset(data11sta,predator==0)$most_adv^2)
n_fl_2np<-I(subset(data11sta,predator==0)$n_fl^2)
h_shoot_2np<-I(subset(data11sta,predator==0)$h_shoot^2)

model3<-lm(n_intact_fruits~(most_adv+n_fl+h_shoot+most_adv_2+n_fl_2+h_shoot_2)*predator,data=data11sta)
Anova(model3)
summary(model3)

model3p<-lm(n_intact_fruits~most_adv+n_fl+h_shoot+
              most_adv_2p+n_fl_2p+h_shoot_2p+
              most_adv:LokalID+n_fl:LokalID+h_shoot:LokalID+
              most_adv_2p:LokalID+n_fl_2p:LokalID+h_shoot_2p:LokalID,
            data=subset(data11sta,predator==1))
Anova(model3p)

model3np<-lm(n_intact_fruits~most_adv+n_fl+h_shoot+
               most_adv_2np+n_fl_2np+h_shoot_2np+
               most_adv:LokalID+n_fl:LokalID+h_shoot:LokalID+
               most_adv_2np:LokalID+n_fl_2np:LokalID+h_shoot_2np:LokalID,
             data=subset(data11sta,predator==0))
Anova(model3np)








