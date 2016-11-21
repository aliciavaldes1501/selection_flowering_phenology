#Read data
data <- read.table(
  "data_JMP.txt",
  header=TRUE, sep="\t", na.strings="NA", dec=",", strip.white=TRUE)
head(data)
summary(data)
str(data)
data$year<-as.factor(data$year)

aggregate(data[,c("h_shoot_sta","most_adv_sta",
"n_eggs_sta","n_fl_sta","n_intact_fruits_rel","n_shoots_sta",
"phen_index_sta"), drop=FALSE], by=list(LokalID=data$LokalID,year=data$year), FUN=mean)

aggregate(data[,c("h_shoot_sta","most_adv_sta",
"n_eggs_sta","n_fl_sta","n_intact_fruits_rel","n_shoots_sta",
"phen_index_sta"), drop=FALSE], by=list(LokalID=data$LokalID,year=data$year), FUN=sd)


#Effects of traits on fitness
library(car)

#2010

#Model with all populations, linear selection + interaction with population
Anova(lm(n_intact_fruits_rel~most_adv_sta+n_fl_sta+h_shoot_sta+
               LokalID:most_adv_sta+LokalID:n_fl_sta+LokalID:h_shoot_sta,
             data=subset(data,year==2010)),type="II")

summary(lm(n_intact_fruits_rel~most_adv_sta+n_fl_sta+h_shoot_sta,
           data=subset(data,year==2010))) #Get estimate for h_shoot

#Model with all populations, linear selection + interaction between traits + interaction with population
Anova(lm(n_intact_fruits_rel~most_adv_sta+n_fl_sta+h_shoot_sta+
         most_adv_sta:n_fl_sta+most_adv_sta:h_shoot_sta+n_fl_sta:h_shoot_sta+
         LokalID:most_adv_sta+LokalID:n_fl_sta+LokalID:h_shoot_sta,
         data=subset(data,year==2010)),type="II")   
         #interaction n_fl:h_shoot is significant

#Model with all populations, non-linear selection + interaction with population
Anova(lm(n_intact_fruits_rel~most_adv_sta+n_fl_sta+h_shoot_sta+
                   I(most_adv_sta^2)+I(n_fl_sta^2)+I(h_shoot_sta^2)+
                   LokalID:most_adv_sta+LokalID:n_fl_sta+LokalID:h_shoot_sta+
                   LokalID:I(most_adv_sta^2)+LokalID:I(n_fl_sta^2)+LokalID:I(h_shoot_sta^2),
                 data=subset(data,year==2010)),type="II")





