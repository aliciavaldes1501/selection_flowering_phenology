#Read data
datapoll <- read.table(
  "data_poll.txt",
  header=TRUE, sep="\t", na.strings="NA", dec=",", strip.white=TRUE)
head(datapoll)
summary(datapoll)
str(datapoll)


#Standardize  reproductive traits

mean_n_intact_fruits<-aggregate(n_intact_fruits~Lokal, datapoll, mean )

datapoll<-merge(datapoll,mean_n_intact_fruits, by="Lokal")

head(datapoll)

mean_n_shoots<-aggregate(n_shoots~Lokal, datapoll, mean )
mean_h_shoot<-aggregate(h_shoot~Lokal, datapoll, mean )
mean_n_fl<-aggregate(n_fl~Lokal, datapoll, mean )
mean_phen_index<-aggregate(phen_index~Lokal, datapoll, mean )
mean_most_adv<-aggregate(most_adv~Lokal, datapoll, mean )

sd_n_shoots<-aggregate(n_shoots~Lokal, datapoll, sd )
sd_h_shoot<-aggregate(h_shoot~Lokal, datapoll, sd )
sd_n_fl<-aggregate(n_fl~Lokal, datapoll, sd )
sd_phen_index<-aggregate(phen_index~Lokal, datapoll, sd )
sd_most_adv<-aggregate(most_adv~Lokal, datapoll, sd )

datapoll<-merge(datapoll,mean_n_shoots, by="Lokal")
datapoll<-merge(datapoll,mean_h_shoot, by="Lokal")
datapoll<-merge(datapoll,mean_n_fl, by="Lokal")
datapoll<-merge(datapoll,mean_phen_index, by="Lokal")
datapoll<-merge(datapoll,mean_most_adv, by="Lokal")

head(datapoll)
names(datapoll)

names(datapoll)[3:7]<-c("n_fl","phen_index","most_adv","n_shoots","h_shoot")
names(datapoll)[9:10]<-c("n_intact_fruits","n_intact_fruits_mean")
names(datapoll)[11:15]<-c("n_shoots_mean","h_shoot_mean","n_fl_mean","phen_index_mean","most_adv_mean")

datapoll<-merge(datapoll,sd_n_shoots, by="Lokal")
datapoll<-merge(datapoll,sd_h_shoot, by="Lokal")
datapoll<-merge(datapoll,sd_n_fl, by="Lokal")
datapoll<-merge(datapoll,sd_phen_index, by="Lokal")
datapoll<-merge(datapoll,sd_most_adv, by="Lokal")

names(datapoll)
names(datapoll)[3:7]<-c("n_fl","phen_index","most_adv","n_shoots","h_shoot")
names(datapoll)[16:20]<-c("n_shoots_sd","h_shoot_sd","n_fl_sd","phen_index_sd","most_adv_sd")

names(datapoll)

datapoll$n_shoots_sta<-(datapoll$n_shoots-datapoll$n_shoots_mean)/datapoll$n_shoots_sd
datapoll$h_shoot_sta<-(datapoll$h_shoot-datapoll$h_shoot_mean)/datapoll$h_shoot_sd
datapoll$n_fl_sta<-(datapoll$n_fl-datapoll$n_fl_mean)/datapoll$n_fl_sd
datapoll$phen_index_sta<-(datapoll$phen_index-datapoll$phen_index_mean)/datapoll$phen_index_sd
datapoll$most_adv_sta<-(datapoll$most_adv-datapoll$most_adv_mean)/datapoll$most_adv_sd

summary(datapoll,by="Lokal")
aggregate(n_shoots_sta~Lokal, datapoll, sd )
aggregate(h_shoot_sta~Lokal, datapoll, sd )
aggregate(n_fl_sta~Lokal, datapoll, sd )
aggregate(phen_index_sta~Lokal, datapoll, sd )
aggregate(most_adv_sta~Lokal, datapoll, sd )

datapoll$n_intact_fruits_sta<-datapoll$n_intact_fruits/datapoll$n_intact_fruits_mean


summary(datapoll,by="Lokal")
aggregate(n_intact_fruits_sta~Lokal, datapoll, sd )

names(datapoll)

datapollsta<-datapoll[c(1:2,8,21:26)]
head(datapollsta)
names(datapollsta)<-c("Lokal","Individ","n_eggs","n_shoots","h_shoot","n_fl","phen_index","most_adv",
                      "n_intact_fruits")
summary(datapollsta,by="Lokal")

#Effects of traits on fitness

cor(datapollsta[7:8],use="pairwise.complete.obs") #phen_index and most_adv highly correlated

modelfr1_10<-lm(n_intact_fruits~phen_index+n_fl+n_shoots+h_shoot+Lokal:phen_index+
                  Lokal:n_fl+Lokal:n_shoots+Lokal:h_shoot,data=datapollsta)
Anova(modelfr1_10,type="II")
summary(lm(n_intact_fruits~phen_index+n_fl+n_shoots+h_shoot,data=datapollsta))

#Separate models for pops with/without pollination

datapollsta_npoll<-subset(datapollsta,Lokal=="Tånga hed")
datapollsta_poll<-subset(datapollsta,Lokal=="Tångahed Pollinering")

#With pollination
modelfr1_poll_10<-lm(n_intact_fruits~phen_index+n_fl+n_shoots+h_shoot,data=datapollsta_poll)
Anova(modelfr1_poll_10,type="II")
summary(modelfr1_poll_10)

#Without pollination
modelfr1_npoll_10<-lm(n_intact_fruits~phen_index+n_fl+n_shoots+h_shoot,data=datapollsta_npoll)
Anova(modelfr1_npoll_10,type="II")
summary(modelfr1_npoll_10)



summary(lm(n_intact_fruits~Lokal,data=datapollsta))









