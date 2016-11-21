#SEM with standardized reproductive traits

#2010

data10 <- read.table(
  "data1_2010.txt",
  header=TRUE, sep="\t", na.strings="NA", dec=",", strip.white=TRUE)
head(data10)
str(data10)

#Standardize  reproductive traits

mean_h_shoot<-aggregate(h_shoot~LokalID, data10, mean )
mean_n_fl<-aggregate(n_fl~LokalID, data10, mean )
mean_most_adv<-aggregate(most_adv~LokalID, data10, mean )

sd_h_shoot<-aggregate(h_shoot~LokalID, data10, sd )
sd_n_fl<-aggregate(n_fl~LokalID, data10, sd )
sd_most_adv<-aggregate(most_adv~LokalID, data10, sd )

data10<-merge(data10,mean_h_shoot, by="LokalID")
data10<-merge(data10,mean_n_fl, by="LokalID")
data10<-merge(data10,mean_most_adv, by="LokalID")

head(data10)
names(data10)

names(data10)[6:7]<-c("h_shoot","n_fl")
names(data10)[9]<-c("most_adv")
names(data10)[17:19]<-c("h_shoot_mean","n_fl_mean","most_adv_mean")

data10<-merge(data10,sd_h_shoot, by="LokalID")
data10<-merge(data10,sd_n_fl, by="LokalID")
data10<-merge(data10,sd_most_adv, by="LokalID")

names(data10)
names(data10)[6:7]<-c("h_shoot","n_fl")
names(data10)[9]<-c("most_adv")
names(data10)[20:22]<-c("h_shoot_sd","n_fl_sd","most_adv_sd")

names(data10)

data10$h_shoot_sta<-(data10$h_shoot-data10$h_shoot_mean)/data10$h_shoot_sd
data10$n_fl_sta<-(data10$n_fl-data10$n_fl_mean)/data10$n_fl_sd
data10$most_adv_sta<-(data10$most_adv-data10$most_adv_mean)/data10$most_adv_sd

summary(data10,by="LokalID")
aggregate(h_shoot_sta~LokalID, data10, sd )
aggregate(n_fl_sta~LokalID, data10, sd )
aggregate(most_adv_sta~LokalID, data10, sd )

# Model 2010, eggs

data10_pred<-subset(data10,predator==1&LokalID!="Göt016")
data10_pred$LokalID<-droplevels(data10_pred$LokalID)
data10_pred<-data10_pred[complete.cases(data10_pred[6:10]),]
#Construct new variable: observation-level random effect
data10_pred$id<-1:1000

library(lmerTest)

modeleg10<-glmer.nb(n_eggs~most_adv_sta+n_fl_sta+h_shoot_sta+(1|LokalID),data=data10_pred)
summary(modeleg10)
r.squaredGLMM(modeleg10)

modeleg10p<-glmer(n_eggs~most_adv_sta+n_fl_sta+h_shoot_sta+(1|LokalID)+(1|id),data=data10_pred,family="poisson")
summary(modeleg10p)
r.squaredGLMM(modeleg10p)
overdisp.glmer(modeleg10p) #OK!

modelfr10<-glmer(n_intact_fruits~most_adv_sta+n_fl_sta+h_shoot_sta+n_eggs+(1|LokalID),data=data10_pred,family="poisson")
summary(modelfr10)
r.squaredGLMM(modelfr10)
overdisp.glmer(modelfr10) #OK!

library(piecewiseSEM)
model10<-list(
  glmer(n_eggs~most_adv+n_fl+h_shoot+(1|LokalID)+(1|id),data=data10_pred,family="poisson"),
  glmer(n_intact_fruits~n_fl+h_shoot+n_eggs+(1|LokalID),data=data10_pred,family="poisson")
)

sem.fit(model10,data=data10_pred,conditional=T,
        corr.errors=c("most_adv_sta~~n_fl_sta","most_adv_sta~~h_shoot_sta","n_fl_sta~~h_shoot_sta"))
sem.model.fits(model10)
sem.coefs(model10,data=data10_pred,
          corr.errors=c("most_adv_sta~~n_fl_sta","most_adv_sta~~h_shoot_sta","n_fl_sta~~h_shoot_sta"))

#lmer for standardized coefs
model10l<-list(
  lmer(scale(n_eggs)~scale(most_adv)+scale(n_fl)+scale(h_shoot)+(1|LokalID),data=data10_pred),
  lmer(scale(n_intact_fruits)~scale(n_fl)+scale(h_shoot)+scale(n_eggs)+(1|LokalID),data=data10_pred)
)
sem.fit(model10l,data=data10_pred,conditional=T,
        corr.errors=c("most_adv~~n_fl","most_adv~~h_shoot","n_fl~~h_shoot"))
sem.model.fits(model10l)
sem.coefs(model10l,data=data10_pred,
          corr.errors=c("most_adv~~n_fl","most_adv~~h_shoot","n_fl~~h_shoot"))

