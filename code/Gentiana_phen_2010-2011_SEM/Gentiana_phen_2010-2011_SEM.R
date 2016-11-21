#2010 - eggs
##################################################################################
data10 <- read.table(
  "data1_2010.txt",
  header=TRUE, sep="\t", na.strings="NA", dec=",", strip.white=TRUE)
head(data10)
str(data10)

################################################ Correction phenology
dates_correction <- read.table(
  "dates_correction.txt",
  header=TRUE, sep="\t", na.strings="NA", dec=",", strip.white=TRUE)
dates_correction
data10<-merge(data10,subset(dates_correction,year=="2010"))
head(data10)
str(data10)
data10$phen_corr<-data10$most_adv+data10$apply_to_phen_1ago
with(data10,plot(most_adv,phen_corr))
with(data10,hist(most_adv))
with(data10,hist(phen_corr))
######################################################################

data10_pred<-subset(data10,predator==1&LokalID!="Göt016")
data10_pred$LokalID<-droplevels(data10_pred$LokalID)
data10_pred<-data10_pred[complete.cases(data10_pred[6:10]),]
#Construct new variable: observation-level random effect
data10_pred$id<-1:1000

library(lmerTest)

modeleg10<-glmer.nb(n_eggs~phen_corr+n_fl+h_shoot+(1|LokalID),data=data10_pred)
summary(modeleg10)
r.squaredGLMM(modeleg10)

modeleg10p<-glmer(n_eggs~phen_corr+n_fl+h_shoot+(1|LokalID)+(1|id),data=data10_pred,family="poisson")
summary(modeleg10p)
r.squaredGLMM(modeleg10p)
overdisp.glmer(modeleg10p) #OK!

modelfr10<-glmer(n_intact_fruits~phen_corr+n_fl+h_shoot+n_eggs+(1|LokalID),data=data10_pred,family="poisson")
summary(modelfr10)
r.squaredGLMM(modelfr10)
overdisp.glmer(modelfr10) #OK!

library(piecewiseSEM)
model10<-list(
  glmer(n_eggs~phen_corr+n_fl+h_shoot+(1|LokalID)+(1|id),data=data10_pred,family="poisson"),
  glmer(n_intact_fruits~n_fl+h_shoot+n_eggs+(1|LokalID),data=data10_pred,family="poisson")
)

sem.fit(model10,data=data10_pred,conditional=T,
        corr.errors=c("phen_corr~~n_fl","phen_corr~~h_shoot","n_fl~~h_shoot"))
sem.model.fits(model10)
sem.coefs(model10,data=data10_pred,
          corr.errors=c("phen_corr~~n_fl","phen_corr~~h_shoot","n_fl~~h_shoot"))

#lmer for standardized coefs
model10l<-list(
  lmer(scale(n_eggs)~scale(most_adv)+scale(n_fl)+scale(h_shoot)+(1|LokalID),data=data10_pred),
  lmer(scale(n_intact_fruits)~scale(n_fl)+scale(h_shoot)+scale(n_eggs)+(1|LokalID),data=data10_pred)
  )
model10l<-list(
  lm(scale(n_eggs)~scale(most_adv)+scale(n_fl)+scale(h_shoot),data=data10_pred),
  lm(scale(n_intact_fruits)~scale(n_fl)+scale(h_shoot)+scale(n_eggs),data=data10_pred)
)
model10l<-list(
  lm(n_eggs~most_adv+n_fl+h_shoot,data=data10_pred),
  lm(n_intact_fruits~n_fl+h_shoot+n_eggs,data=data10_pred)
)
sem.fit(model10l,data=data10_pred,conditional=T,
        corr.errors=c("most_adv~~n_fl","most_adv~~h_shoot","n_fl~~h_shoot"))
sem.model.fits(model10l)
sem.coefs(model10l,data=data10_pred,
          corr.errors=c("most_adv~~n_fl","most_adv~~h_shoot","n_fl~~h_shoot"))

#2010 - attack
##################################################################################

modelat10<-glmer(attack~phen_corr+n_fl+h_shoot+(1|LokalID),data=data10_pred,family="binomial")
summary(modelat10)
r.squaredGLMM(modelat10)
overdisp.glmer(modelat10) #OK!

modelfr10a<-glmer(n_intact_fruits~phen_corr+n_fl+h_shoot+attack+(1|LokalID),data=data10_pred,family="poisson")
summary(modelfr10a)
r.squaredGLMM(modelfr10a)
overdisp.glmer(modelfr10a) #OK!

model10a<-list(
  glmer(attack~phen_corr+n_fl+h_shoot+(1|LokalID),data=data10_pred,family="binomial"),
  glmer(n_intact_fruits~n_fl+h_shoot+attack+(1|LokalID),data=data10_pred,family="poisson")
)

sem.fit(model10a,data=data10_pred,conditional=T,
        corr.errors=c("most_adv~~n_fl","most_adv~~h_shoot","n_fl~~h_shoot"))
sem.model.fits(model10a)
sem.coefs(model10a,data=data10_pred,
          corr.errors=c("most_adv~~n_fl","most_adv~~h_shoot","n_fl~~h_shoot"))

#lmer for standardized coefs
model10la<-list(
  lmer(scale(attack)~scale(phen_corr)+scale(n_fl)+scale(h_shoot)+(1|LokalID),data=data10_pred),
  lmer(scale(n_intact_fruits)~scale(n_fl)+scale(h_shoot)+scale(attack)+(1|LokalID),data=data10_pred)
)
sem.fit(model10la,data=data10_pred,conditional=T,
        corr.errors=c("phen_corr~~n_fl","phen_corr~~h_shoot","n_fl~~h_shoot"))
sem.model.fits(model10la)
sem.coefs(model10la,data=data10_pred,
          corr.errors=c("phen_corr~~n_fl","phen_corr~~h_shoot","n_fl~~h_shoot"))

#2011 - eggs
##################################################################################
data11 <- read.table(
  "data1_2011.txt",
  header=TRUE, sep="\t", na.strings="NA", dec=",", strip.white=TRUE)
head(data11)
str(data11)

################################################ Correction phenology
data11<-merge(data11,subset(dates_correction,year=="2011"))
head(data11)
str(data11)
data11$phen_corr<-data11$most_adv+data11$apply_to_phen_1ago
with(data11,plot(most_adv,phen_corr))
with(data11,hist(most_adv))
with(data11,hist(phen_corr))
######################################################################

data11_pred<-subset(data11,Predator==1)
data11_pred$LokalID<-droplevels(data11_pred$LokalID)
data11_pred<-data11_pred[complete.cases(data11_pred[6:10]),]
#Construct new variable: observation-level random effect
data11_pred$id<-1:1099

modeleg11<-glmer.nb(n_eggs~phen_corr+n_fl+h_shoot+(1|LokalID),data=data11_pred)
summary(modeleg11)
r.squaredGLMM(modeleg11)

modeleg11p<-glmer(n_eggs~phen_corr+n_fl+h_shoot+(1|LokalID)+(1|id),data=data11_pred,family="poisson")
summary(modeleg11p)
r.squaredGLMM(modeleg11p)
overdisp.glmer(modeleg11p) #OK!

modelfr11<-glmer(n_intact_fruits~phen_corr+n_fl+h_shoot+n_eggs+(1|LokalID),data=data11_pred,family="poisson")
summary(modelfr11)
r.squaredGLMM(modelfr11)
overdisp.glmer(modelfr11) #OK!

model11<-list(
  glmer(n_eggs~phen_corr+n_fl+h_shoot+(1|LokalID)+(1|id),data=data11_pred,family="poisson"),
  glmer(n_intact_fruits~phen_corr+n_fl+n_eggs+(1|LokalID),data=data11_pred,family="poisson")
)

sem.fit(model11,data=data11_pred,conditional=T,
        corr.errors=c("phen_corr~~n_fl","phen_corr~~h_shoot","n_fl~~h_shoot"))
sem.model.fits(model11)
sem.coefs(model11,data=data11_pred,
          corr.errors=c("phen_corr~~n_fl","phen_corr~~h_shoot","n_fl~~h_shoot"))

#lmer for standardized coefs
model11l<-list(
  lmer(scale(n_eggs)~scale(phen_corr)+scale(n_fl)+scale(h_shoot)+(1|LokalID),data=data11_pred),
  lmer(scale(n_intact_fruits)~scale(phen_corr)+scale(n_fl)+scale(n_eggs)+(1|LokalID),data=data11_pred)
)
sem.fit(model11l,data=data11_pred,conditional=T,
        corr.errors=c("phen_corr~~n_fl","phen_corr~~h_shoot","n_fl~~h_shoot"))
sem.model.fits(model11l)
sem.coefs(model11l,data=data11_pred,
          corr.errors=c("phen_corr~~n_fl","phen_corr~~h_shoot","n_fl~~h_shoot"))

#2011 - attack
##################################################################################

modelat11<-glmer(attack~phen_corr+n_fl+h_shoot+(1|LokalID),data=data11_pred,family="binomial")
summary(modelat11)
r.squaredGLMM(modelat11)
overdisp.glmer(modelat11) #OK!

modelfr11a<-glmer(n_intact_fruits~phen_corr+n_fl+h_shoot+attack+(1|LokalID),data=data11_pred,family="poisson")
summary(modelfr11a)
r.squaredGLMM(modelfr11a)
overdisp.glmer(modelfr11a) #OK!

model11a<-list(
  glmer(attack~phen_corr+n_fl+(1|LokalID),data=data11_pred,family="binomial"),
  glmer(n_intact_fruits~phen_corr+n_fl+h_shoot+attack+(1|LokalID),data=data11_pred,family="poisson")
)

sem.fit(model11a,data=data11_pred,conditional=T,
        corr.errors=c("phen_corr~~n_fl","phen_corr~~h_shoot","n_fl~~h_shoot"))
sem.model.fits(model11a)
sem.coefs(model11a,data=data11_pred,
          corr.errors=c("phen_corr~~n_fl","phen_corr~~h_shoot","n_fl~~h_shoot"))

#lmer for standardized coefs
model11la<-list(
  lmer(scale(attack)~scale(phen_corr)+scale(n_fl)+(1|LokalID),data=data11_pred),
  lmer(scale(n_intact_fruits)~scale(phen_corr)+scale(n_fl)+scale(h_shoot)+scale(attack)+(1|LokalID),data=data11_pred)
)
sem.fit(model11la,data=data11_pred,conditional=T,
        corr.errors=c("phen_corr~~n_fl","phen_corr~~h_shoot","n_fl~~h_shoot"))
sem.model.fits(model11la)
sem.coefs(model11la,data=data11_pred,
          corr.errors=c("phen_corr~~n_fl","phen_corr~~h_shoot","n_fl~~h_shoot"))











