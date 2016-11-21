head(data10_pred)
str(data10_pred)
data10_pred$LokalID<-droplevels(data10_pred$LokalID)
attach(data10_pred)
hist(n_eggs)
hist(n_intact_fruits)
library(lmerTest)

model_eg10<-glmer.nb(n_eggs~scale(most_adv)+scale(n_fl)+scale(h_shoot)+
                    (1|LokalID),data=data10_pred)
summary(model_eg10)
overdisp.glmer(model_eg10)

model_eg10_p<-glmer(n_eggs~scale(most_adv)+scale(n_fl)+scale(h_shoot)+
                    (1|LokalID),family="poisson")
summary(model_eg10_p)
overdisp.glmer(model_eg10_p)

AIC(model_eg10,model_eg10_p) #nb wins
plot(model_eg10)


model_fr10_p<-glmer(n_intact_fruits~scale(most_adv)+scale(n_fl)+scale(h_shoot)+
                      scale(n_eggs)+(1|LokalID),family="poisson")
summary(model_fr10_p)
overdisp.glmer(model_fr10_p)

model_fr10<-glmer.nb(n_intact_fruits~scale(most_adv)+scale(n_fl)+scale(h_shoot)+
                       scale(n_eggs)+(1|LokalID),data=data10_pred)
summary(model_fr10)
overdisp.glmer(model_fr10)

AIC(model_fr10,model_fr10_p) #poisson wins


data10_pred<-data10_pred[complete.cases(data10_pred[6:14]),]


model10<-list(
  glmer.nb(n_eggs~most_adv+n_fl+h_shoot+(1|LokalID),na.action="na.fail",data=data10_pred),
  glmer(n_intact_fruits~n_fl+h_shoot+n_eggs+(1|LokalID),na.action="na.fail",data=data10_pred,family="poisson")
)



sem.fit(model10,data=data10_pred,conditional=T,corr.errors=c("most_adv~~n_fl","most_adv~~h_shoot","n_fl~~h_shoot"))
sem.model.fits(model10)
sem.coefs(model10, data10_pred,
        corr.errors=c("most_adv~~n_fl","most_adv~~h_shoot","n_fl~~h_shoot"))

