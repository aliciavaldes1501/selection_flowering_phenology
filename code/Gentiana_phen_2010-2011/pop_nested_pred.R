data <- read.table(
  "data_JMP.txt",
  header=TRUE, sep="\t", na.strings="NA", dec=",", strip.white=TRUE)
data$year<-as.factor(data$year)
data$predator<-as.factor(data$predator)
head(data)
str(data)

#Effects of traits on fitness - removing n_shoots, adding quadratic and interaction terms
library(car)

#2010

#Linear selection
#Population=fixed
modelfr_10<-lm(n_intact_fruits_rel~most_adv_sta+n_fl_sta+h_shoot_sta+
                 predator*most_adv_sta+predator*n_fl_sta+predator*h_shoot_sta+
                 (LokalID%in%predator):most_adv_sta+(LokalID%in%predator):n_fl_sta+(LokalID%in%predator):h_shoot_sta,
                 data=subset(data,year==2010))
Anova(modelfr_10,type="II")

summary(lm(n_intact_fruits_rel~most_adv_sta+n_fl_sta+h_shoot_sta+
             predator*most_adv_sta+predator*n_fl_sta+predator*h_shoot_sta,
           data=subset(data,year==2010)))

#Differences between "/" and "%in%"? --> I think "%in%" is the right one to use for nesting

#Population=random
library(lmerTest)
modelfr_10<-lmer(n_intact_fruits_rel ~ most_adv_sta+n_fl_sta+h_shoot_sta+
      predator*most_adv_sta+predator*n_fl_sta+predator*h_shoot_sta + 
      (1|predator/LokalID:most_adv_sta)+(1|predator/LokalID:n_fl_sta)+(1|predator/LokalID:h_shoot_sta),
      data = subset(data,year==2010),REML=F)
summary(modelfr_10)
Anova(modelfr_10,type="II")
#But no significance tests for random factors!

#Linear selection + interaction between traits
#Population=fixed
modelfr_10_2<-lm(n_intact_fruits_rel~most_adv_sta+n_fl_sta+h_shoot_sta+
                   most_adv:n_fl+most_adv:h_shoot+n_fl:h_shoot+
                 predator*most_adv_sta+predator*n_fl_sta+predator*h_shoot_sta+
                 (LokalID%in%predator):most_adv_sta+(LokalID%in%predator):n_fl_sta+(LokalID%in%predator):h_shoot_sta,
               data=subset(data,year==2010))
Anova(modelfr_10_2,type="II")
#None of the interactions is significant

#Population=random
modelfr_10_2<-lmer(n_intact_fruits_rel ~ most_adv_sta+n_fl_sta+h_shoot_sta+
             most_adv:n_fl+most_adv:h_shoot+n_fl:h_shoot+
             predator*most_adv_sta+predator*n_fl_sta+predator*h_shoot_sta + 
            (1|predator/LokalID:most_adv_sta)+(1|predator/LokalID:n_fl_sta)+(1|predator/LokalID:h_shoot_sta),
            data = subset(data,year==2010),REML=F)
summary(modelfr_10_2)
Anova(modelfr_10_2,type="II")

#Linear effects + quadratic effects
#Population=fixed
most_adv_sta2<-I(subset(data,year==2010)$most_adv_sta^2)
n_fl_sta2<-I(subset(data,year==2010)$n_fl_sta^2)
h_shoot_sta2<-I(subset(data,year==2010)$h_shoot_sta^2)

modelfr_10_3<-lm(n_intact_fruits_rel~most_adv_sta+n_fl_sta+h_shoot_sta+
              most_adv_sta2+n_fl_sta2+h_shoot_sta2+
              predator*most_adv_sta+predator*n_fl_sta+predator*h_shoot_sta+
              predator*most_adv_sta2+predator*n_fl_sta2+predator*h_shoot_sta2+
              (LokalID%in%predator):most_adv_sta+(LokalID%in%predator):n_fl_sta+(LokalID%in%predator):h_shoot_sta+
              (LokalID%in%predator):most_adv_sta2+(LokalID%in%predator):n_fl_sta2+(LokalID%in%predator):h_shoot_sta2,
              data=subset(data,year==2010))
  
Anova(modelfr_10_3,type="II")

#Population=random
modelfr_10_3<-lmer(n_intact_fruits_rel ~ most_adv_sta+n_fl_sta+h_shoot_sta+
              most_adv_sta2+n_fl_sta2+h_shoot_sta2+
              predator*most_adv_sta+predator*n_fl_sta+predator*h_shoot_sta + 
              predator*most_adv_sta2+predator*n_fl_sta2+predator*h_shoot_sta2+
              (1|predator/LokalID:most_adv_sta)+(1|predator/LokalID:n_fl_sta)+(1|predator/LokalID:h_shoot_sta)+
              (1|predator/LokalID:most_adv_sta2)+(1|predator/LokalID:n_fl_sta2)+(1|predator/LokalID:h_shoot_sta2),
              data = subset(data,year==2010),REML=F,lmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))
summary(modelfr_10_3)
Anova(modelfr_10_3,type="II")


#2011

#Linear selection
#Population=fixed
modelfr_11<-lm(n_intact_fruits_rel~most_adv_sta+n_fl_sta+h_shoot_sta+
                 predator*most_adv_sta+predator*n_fl_sta+predator*h_shoot_sta+
                 (LokalID%in%predator):most_adv_sta+(LokalID%in%predator):n_fl_sta+(LokalID%in%predator):h_shoot_sta,
               data=subset(data,year==2011))
Anova(modelfr_11,type="II")

summary(lm(n_intact_fruits_rel~most_adv_sta+n_fl_sta+h_shoot_sta+
             predator*most_adv_sta+predator*n_fl_sta+predator*h_shoot_sta,
           data=subset(data,year==2011)))

#Differences between "/" and "%in%"? --> I think "%in%" is the right one to use for nesting

#Population=random
library(lmerTest)
modelfr_11<-lmer(n_intact_fruits_rel ~ most_adv_sta+n_fl_sta+h_shoot_sta+
                   predator*most_adv_sta+predator*n_fl_sta+predator*h_shoot_sta + 
                   (1|predator/LokalID:most_adv_sta)+(1|predator/LokalID:n_fl_sta)+(1|predator/LokalID:h_shoot_sta),
                 data = subset(data,year==2011),REML=F)
summary(modelfr_11)
Anova(modelfr_11,type="II")
#But no significance tests for random factors!

#Linear selection + interaction between traits
#Population=fixed
modelfr_11_2<-lm(n_intact_fruits_rel~most_adv_sta+n_fl_sta+h_shoot_sta+
                   most_adv:n_fl+most_adv:h_shoot+n_fl:h_shoot+
                   predator*most_adv_sta+predator*n_fl_sta+predator*h_shoot_sta+
                   (LokalID%in%predator):most_adv_sta+(LokalID%in%predator):n_fl_sta+(LokalID%in%predator):h_shoot_sta,
                 data=subset(data,year==2011))
Anova(modelfr_11_2,type="II")
#None of the interactions is significant

#Population=random
modelfr_11_2<-lmer(n_intact_fruits_rel ~ most_adv_sta+n_fl_sta+h_shoot_sta+
                     most_adv:n_fl+most_adv:h_shoot+n_fl:h_shoot+
                     predator*most_adv_sta+predator*n_fl_sta+predator*h_shoot_sta + 
                     (1|predator/LokalID:most_adv_sta)+(1|predator/LokalID:n_fl_sta)+(1|predator/LokalID:h_shoot_sta),
                   data = subset(data,year==2011),REML=F)
summary(modelfr_11_2)
Anova(modelfr_11_2,type="II")

#Linear effects + quadratic effects
#Population=fixed
most_adv_sta2<-I(subset(data,year==2011)$most_adv_sta^2)
n_fl_sta2<-I(subset(data,year==2011)$n_fl_sta^2)
h_shoot_sta2<-I(subset(data,year==2011)$h_shoot_sta^2)

modelfr_11_3<-lm(n_intact_fruits_rel~most_adv_sta+n_fl_sta+h_shoot_sta+
                   most_adv_sta2+n_fl_sta2+h_shoot_sta2+
                   predator*most_adv_sta+predator*n_fl_sta+predator*h_shoot_sta+
                   predator*most_adv_sta2+predator*n_fl_sta2+predator*h_shoot_sta2+
                   (LokalID%in%predator):most_adv_sta+(LokalID%in%predator):n_fl_sta+(LokalID%in%predator):h_shoot_sta+
                   (LokalID%in%predator):most_adv_sta2+(LokalID%in%predator):n_fl_sta2+(LokalID%in%predator):h_shoot_sta2,
                 data=subset(data,year==2011))

Anova(modelfr_11_3,type="II")

#Population=random
modelfr_11_3<-lmer(n_intact_fruits_rel ~ most_adv_sta+n_fl_sta+h_shoot_sta+
                     most_adv_sta2+n_fl_sta2+h_shoot_sta2+
                     predator*most_adv_sta+predator*n_fl_sta+predator*h_shoot_sta + 
                     predator*most_adv_sta2+predator*n_fl_sta2+predator*h_shoot_sta2+
                     (1|predator/LokalID:most_adv_sta)+(1|predator/LokalID:n_fl_sta)+(1|predator/LokalID:h_shoot_sta)+
                     (1|predator/LokalID:most_adv_sta2)+(1|predator/LokalID:n_fl_sta2)+(1|predator/LokalID:h_shoot_sta2),
                   data = subset(data,year==2011),REML=F,lmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))
summary(modelfr_11_3)
Anova(modelfr_11_3,type="II")











