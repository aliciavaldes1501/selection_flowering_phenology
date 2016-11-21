#Changed type II to type III sums of squares

#Effects of traits on fitness
library(car)
#2010

#Linear selection (Table 1A)

model1<-lm(n_intact_fruits~most_adv+n_fl+h_shoot+
             LokalID:most_adv+LokalID:n_fl+LokalID:h_shoot,data=data10sta[1:2001,])

Anova(model1,type="II")
Anova(model1,type="III")

summary(lm(n_intact_fruits~most_adv+n_fl+h_shoot,data=data10sta))

#Table 2
data10sta$predator<-as.factor(data10sta$predator)
model1bis<-lmer(n_intact_fruits~(most_adv+n_fl+h_shoot)*predator+(0+most_adv+n_fl+h_shoot|LokalID),
                data=data10sta[1:2001,])

summary(model1bis)
Anova(model1bis) #Wald chisquare
Anova(model1bis, type="III")

#Linear+interaction+quadratic (Table 1B)
most_adv_2<-I(data10sta[1:2001,]$most_adv^2)
n_fl_2<-I(data10sta[1:2001,]$n_fl^2)
h_shoot_2<-I(data10sta[1:2001,]$h_shoot^2)

model4<-lm(n_intact_fruits~(most_adv+n_fl+h_shoot+most_adv_2+n_fl_2+h_shoot_2+
                              most_adv:n_fl+most_adv:h_shoot+n_fl:h_shoot)*LokalID,
           data=data10sta[1:2001,])
model4<-update(model4, . ~ . -LokalID)
Anova(model4,type="II")
Anova(model4,type="III")

#2011

#Linear selection (Table 1A)

model1<-lm(n_intact_fruits~most_adv+n_fl+h_shoot+
             LokalID:most_adv+LokalID:n_fl+LokalID:h_shoot,data=data11sta)

Anova(model1,type="II")
Anova(model1,type="III")

summary(lm(n_intact_fruits~most_adv+n_fl+h_shoot,data=data11sta))

#Table 2
data11sta$predator<-as.factor(data11sta$predator)
model1bis<-lmer(n_intact_fruits~(most_adv+n_fl+h_shoot)*predator+(0+most_adv+n_fl+h_shoot|LokalID),
                data=data11sta)

summary(model1bis)
Anova(model1bis) #Wald chisquare
Anova(model1bis, type="III")

#Linear+interaction+quadratic (Table 1B)
most_adv_2<-I(data11sta$most_adv^2)
n_fl_2<-I(data11sta$n_fl^2)
h_shoot_2<-I(data11sta$h_shoot^2)

model4<-lm(n_intact_fruits~(most_adv+n_fl+h_shoot+most_adv_2+n_fl_2+h_shoot_2+
                              most_adv:n_fl+most_adv:h_shoot+n_fl:h_shoot)*LokalID,
           data=data11sta)
model4<-update(model4, . ~ . -LokalID)
Anova(model4,type="II")
Anova(model4,type="III")

#Appendix S2

#2010

#Linear selection

model1<-lm(n_intact_fruits~phen_index+n_fl+h_shoot+
             LokalID:phen_index+LokalID:n_fl+LokalID:h_shoot,data=data10sta[1:2001,])

Anova(model1,type="II")
Anova(model1,type="III")

summary(lm(n_intact_fruits~phen_index+n_fl+h_shoot,data=data10sta))

#Linear+interaction+quadratic (Table 1B)
phen_index_2<-I(data10sta[1:2001,]$phen_index^2)
n_fl_2<-I(data10sta[1:2001,]$n_fl^2)
h_shoot_2<-I(data10sta[1:2001,]$h_shoot^2)

model4<-lm(n_intact_fruits~(phen_index+n_fl+h_shoot+phen_index_2+n_fl_2+h_shoot_2+
                              phen_index:n_fl+phen_index:h_shoot+n_fl:h_shoot)*LokalID,
           data=data10sta[1:2001,])
model4<-update(model4, . ~ . -LokalID)
Anova(model4,type="II")
Anova(model4,type="III")

#2011

#Linear selection

model1<-lm(n_intact_fruits~phen_index+n_fl+h_shoot+
             LokalID:phen_index+LokalID:n_fl+LokalID:h_shoot,data=data11sta)

Anova(model1,type="II")
Anova(model1,type="III")

summary(lm(n_intact_fruits~phen_index+n_fl+h_shoot,data=data11sta))

#Linear+interaction+quadratic
phen_index_2<-I(data11sta$phen_index^2)
n_fl_2<-I(data11sta$n_fl^2)
h_shoot_2<-I(data11sta$h_shoot^2)

model4<-lm(n_intact_fruits~(phen_index+n_fl+h_shoot+phen_index_2+n_fl_2+h_shoot_2+
                              phen_index:n_fl+phen_index:h_shoot+n_fl:h_shoot)*LokalID,
           data=data11sta)
model4<-update(model4, . ~ . -LokalID)
Anova(model4,type="II")
Anova(model4,type="III")



