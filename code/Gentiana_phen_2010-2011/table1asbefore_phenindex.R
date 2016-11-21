#Effects of traits on fitness
library(car)

#2010

#Linear selection

model1<-lm(n_intact_fruits~phen_index+n_fl+h_shoot+
             LokalID:phen_index+LokalID:n_fl+LokalID:h_shoot,data=data10sta[1:2001,])

Anova(model1,type="II")

summary(lm(n_intact_fruits~phen_index+n_fl+h_shoot,data=data10sta))

data10sta$predator<-as.factor(data10sta$predator)
model1bis<-lmer(n_intact_fruits~(phen_index+n_fl)*predator+h_shoot+(0+phen_index+n_fl|LokalID),
                data=data10sta[1:2001,])

summary(model1bis)
Anova(model1bis) #Wald chisquare

qqmath(ranef(model1bis))


#Linear selection + interaction between traits

model2<-lm(n_intact_fruits~(phen_index+n_fl+h_shoot+phen_index:n_fl+phen_index:h_shoot+n_fl:h_shoot)*LokalID,
           data=data10sta[1:2001,])
model2<-update(model2, . ~ . -LokalID) 
Anova(model2,type="II")

#Linear effects + quadratic effects
phen_index_2<-I(data10sta[1:2001,]$phen_index^2)
n_fl_2<-I(data10sta[1:2001,]$n_fl^2)
h_shoot_2<-I(data10sta[1:2001,]$h_shoot^2)

model3<-lm(n_intact_fruits~(phen_index+n_fl+h_shoot+phen_index_2+n_fl_2+h_shoot_2)*LokalID,data=data10sta[1:2001,])
model3<-update(model3, . ~ . -LokalID) 
Anova(model3)
summary(lm(n_intact_fruits~phen_index+n_fl+h_shoot+phen_index_2+n_fl_2+h_shoot_2,data=data10sta))

#Linear+interaction+quadratic
model4<-lm(n_intact_fruits~(phen_index+n_fl+h_shoot+phen_index_2+n_fl_2+h_shoot_2+
                              phen_index:n_fl+phen_index:h_shoot+n_fl:h_shoot)*LokalID,
           data=data10sta[1:2001,])
model4<-update(model4, . ~ . -LokalID)
Anova(model4,type="II")

#2011

#Linear selection

model1<-lm(n_intact_fruits~phen_index+n_fl+h_shoot+
             LokalID:phen_index+LokalID:n_fl+LokalID:h_shoot,data=data11sta)
Anova(model1,type="II")

model1bis<-lmer(n_intact_fruits~(phen_index+n_fl+h_shoot)*predator+(0+phen_index+n_fl+h_shoot|LokalID),
                data=data11sta)

summary(model1bis)
Anova(model1bis) #Wald chisquare

qqmath(ranef(model1bis))

#Linear selection + interaction between traits

model2<-lm(n_intact_fruits~(phen_index+n_fl+h_shoot+phen_index:n_fl+phen_index:h_shoot+n_fl:h_shoot)*LokalID,data=data11sta)
model2<-update(model2, . ~ . -LokalID) 
Anova(model2,type="II")

#Linear effects + quadratic effects
phen_index_2<-I(data11sta$phen_index^2)
n_fl_2<-I(data11sta$n_fl^2)
h_shoot_2<-I(data11sta$h_shoot^2)

model3<-lm(n_intact_fruits~(phen_index+n_fl+h_shoot+phen_index_2+n_fl_2+h_shoot_2)*LokalID,data=data11sta)
model3<-update(model3, . ~ . -LokalID) 
Anova(model3)

#Linear+interaction+quadratic
model4<-lm(n_intact_fruits~(phen_index+n_fl+h_shoot+phen_index_2+n_fl_2+h_shoot_2+
                              phen_index:n_fl+phen_index:h_shoot+n_fl:h_shoot)*LokalID,
           data=data11sta)
model4<-update(model4, . ~ . -LokalID)
Anova(model4,type="II")



