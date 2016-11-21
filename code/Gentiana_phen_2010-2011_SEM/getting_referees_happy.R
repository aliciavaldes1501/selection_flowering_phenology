Aggr10<- aggregate(cbind(n_intact_fruits, predator) ~ LokalID, data=data10, FUN=mean)
with(Aggr10,summary(lm(n_intact_fruits~predator)))
with(subset(Aggr10,predator==0),mean(n_intact_fruits))
with(subset(Aggr10,predator==1),mean(n_intact_fruits))
100-((0.8287399*100)/0.97) # % reduction in pops with predator 2010

Aggr11<- aggregate(cbind(n_intact_fruits, Predator) ~ LokalID, data=data11, FUN=mean)
with(Aggr11,summary(lm(n_intact_fruits~Predator)))
with(subset(Aggr11,Predator==0),mean(n_intact_fruits))
with(subset(Aggr11,Predator==1),mean(n_intact_fruits))
100-((0.6090909*100)/0.76) # % reduction in pops with predator 2011

with(data10,plot(as.factor(attack),n_intact_fruits))
with(data10,summary(lm(n_intact_fruits~attack)))
with(data10,summary(lmer(n_intact_fruits~attack+(1|LokalID))))

with(subset(data10,attack==0),mean(n_intact_fruits))
with(subset(data10,attack==1),mean(n_intact_fruits))
100-((0.7554348*100)/0.9228414) # % reduction in plants with predator 2010

with(data11,plot(as.factor(attack),n_intact_fruits))
with(data11,summary(lm(n_intact_fruits~attack)))
with(subset(data11,attack==0),mean(n_intact_fruits))
with(subset(data11,attack==1),mean(n_intact_fruits))
100-((0.589899*100)/0.6859729) # % reduction in plants with predator 2011

with(subset(data10,LokalID=="Ale001"),summary(lm(n_intact_fruits~attack+n_fl)))
with(subset(data10,LokalID=="Göt009a"),summary(lm(n_intact_fruits~attack+n_fl)))
with(subset(data10,LokalID=="Göt009b"),summary(lm(n_intact_fruits~attack+n_fl)))
with(subset(data10,LokalID=="Her003"),summary(lm(n_intact_fruits~attack+n_fl)))
with(subset(data10,LokalID=="Her004"),summary(lm(n_intact_fruits~attack+n_fl)))
with(subset(data10,LokalID=="Her005"),summary(lm(n_intact_fruits~attack+n_fl)))
with(subset(data10,LokalID=="Ler010"),summary(lm(n_intact_fruits~attack+n_fl)))
with(subset(data10,LokalID=="Par003"),summary(lm(n_intact_fruits~attack+n_fl)))
with(subset(data10,LokalID=="Vår004"),summary(lm(n_intact_fruits~attack+n_fl)))
with(subset(data10,LokalID=="Vår009"),summary(lm(n_intact_fruits~attack+n_fl)))


###################################################################################
data10$prod_fruit<-as.factor(with(data10,ifelse(n_intact_fruits>0,"1","0")))
data11$prod_fruit<-as.factor(with(data11,ifelse(n_intact_fruits>0,"1","0")))

Aggr10<- aggregate(as.numeric(prod_fruit) ~ LokalID, data=data10, FUN=sum)
with(Aggr10,summary(lm(n_intact_fruits~predator)))



x <- read.table("data.txt",
  header=TRUE, sep="\t", na.strings="NA", dec=",", strip.white=TRUE)
head(x)
tail(x)
names(x)
str(x)

#Relation among n intact fr and prop pred (fr + fl + bd)
#R2 gives prop of variation in fitness explained by predation
#Within-pops
xpred<-subset(x,predator==1)
xpred$LokalID<-droplevels(xpred$LokalID)
attach(xpred)
summary(lm(n_intact_fruits~prop_pred_c))
summary(glm(n_intact_fruits~prop_pred_c,family="poisson"))
r.squaredLR(glm(n_intact_fruits~prop_pred_c,family="poisson"))

summary(glm(n_intact_fruits~prop_pred_c,family="poisson",data=subset(xpred,year=="2010" & LokalID == "Ale001")))
summary(glm(n_intact_fruits~prop_pred_c,family="poisson",data=subset(xpred,year=="2010" & LokalID == "Göt009a")))
summary(glm(n_intact_fruits~prop_pred_c,family="poisson",data=subset(xpred,year=="2010" & LokalID == "Göt009b")))
summary(glm(n_intact_fruits~prop_pred_c,family="poisson",data=subset(xpred,year=="2010" & LokalID == "Her003")))
summary(glm(n_intact_fruits~prop_pred_c,family="poisson",data=subset(xpred,year=="2010" & LokalID == "Her004")))
summary(glm(n_intact_fruits~prop_pred_c,family="poisson",data=subset(xpred,year=="2010" & LokalID == "Her005")))
summary(glm(n_intact_fruits~prop_pred_c,family="poisson",data=subset(xpred,year=="2010" & LokalID == "Ler010")))
summary(glm(n_intact_fruits~prop_pred_c,family="poisson",data=subset(xpred,year=="2010" & LokalID == "Par003")))
summary(glm(n_intact_fruits~prop_pred_c,family="poisson",data=subset(xpred,year=="2010" & LokalID == "Vår004")))
summary(glm(n_intact_fruits~prop_pred_c,family="poisson",data=subset(xpred,year=="2010" & LokalID == "Vår009")))

summary(glm(n_intact_fruits~prop_pred_c,family="poisson",data=subset(xpred,year=="2011" & LokalID == "Ale001")))
summary(glm(n_intact_fruits~prop_pred_c,family="poisson",data=subset(xpred,year=="2011" & LokalID == "Göt009a")))
summary(glm(n_intact_fruits~prop_pred_c,family="poisson",data=subset(xpred,year=="2011" & LokalID == "Göt009b")))
summary(glm(n_intact_fruits~prop_pred_c,family="poisson",data=subset(xpred,year=="2011" & LokalID == "Göt016")))
summary(glm(n_intact_fruits~prop_pred_c,family="poisson",data=subset(xpred,year=="2011" & LokalID == "Her003")))
summary(glm(n_intact_fruits~prop_pred_c,family="poisson",data=subset(xpred,year=="2011" & LokalID == "Her004")))
summary(glm(n_intact_fruits~prop_pred_c,family="poisson",data=subset(xpred,year=="2011" & LokalID == "Her005")))
summary(glm(n_intact_fruits~prop_pred_c,family="poisson",data=subset(xpred,year=="2011" & LokalID == "Ler010")))
summary(glm(n_intact_fruits~prop_pred_c,family="poisson",data=subset(xpred,year=="2011" & LokalID == "Par003")))
summary(glm(n_intact_fruits~prop_pred_c,family="poisson",data=subset(xpred,year=="2011" & LokalID == "Vår004")))
summary(glm(n_intact_fruits~prop_pred_c,family="poisson",data=subset(xpred,year=="2011" & LokalID == "Vår009")))

c(r.squaredLR(glm(n_intact_fruits~prop_pred_c,family="poisson",data=subset(xpred,year=="2010" & LokalID == "Ale001")))[1],
r.squaredLR(glm(n_intact_fruits~prop_pred_c,family="poisson",data=subset(xpred,year=="2010" & LokalID == "Göt009a")))[1],
r.squaredLR(glm(n_intact_fruits~prop_pred_c,family="poisson",data=subset(xpred,year=="2010" & LokalID == "Göt009b")))[1],
r.squaredLR(glm(n_intact_fruits~prop_pred_c,family="poisson",data=subset(xpred,year=="2010" & LokalID == "Her003")))[1],
r.squaredLR(glm(n_intact_fruits~prop_pred_c,family="poisson",data=subset(xpred,year=="2010" & LokalID == "Her004")))[1],
r.squaredLR(glm(n_intact_fruits~prop_pred_c,family="poisson",data=subset(xpred,year=="2010" & LokalID == "Her005")))[1],
r.squaredLR(glm(n_intact_fruits~prop_pred_c,family="poisson",data=subset(xpred,year=="2010" & LokalID == "Ler010")))[1],
r.squaredLR(glm(n_intact_fruits~prop_pred_c,family="poisson",data=subset(xpred,year=="2010" & LokalID == "Par003")))[1],
r.squaredLR(glm(n_intact_fruits~prop_pred_c,family="poisson",data=subset(xpred,year=="2010" & LokalID == "Vår004")))[1],
r.squaredLR(glm(n_intact_fruits~prop_pred_c,family="poisson",data=subset(xpred,year=="2010" & LokalID == "Vår009")))[1],

r.squaredLR(glm(n_intact_fruits~prop_pred_c,family="poisson",data=subset(xpred,year=="2011" & LokalID == "Ale001")))[1],
r.squaredLR(glm(n_intact_fruits~prop_pred_c,family="poisson",data=subset(xpred,year=="2011" & LokalID == "Göt009a")))[1],
r.squaredLR(glm(n_intact_fruits~prop_pred_c,family="poisson",data=subset(xpred,year=="2011" & LokalID == "Göt009b")))[1],
r.squaredLR(glm(n_intact_fruits~prop_pred_c,family="poisson",data=subset(xpred,year=="2011" & LokalID == "Göt016")))[1],
r.squaredLR(glm(n_intact_fruits~prop_pred_c,family="poisson",data=subset(xpred,year=="2011" & LokalID == "Her003")))[1],
r.squaredLR(glm(n_intact_fruits~prop_pred_c,family="poisson",data=subset(xpred,year=="2011" & LokalID == "Her004")))[1],
r.squaredLR(glm(n_intact_fruits~prop_pred_c,family="poisson",data=subset(xpred,year=="2011" & LokalID == "Her005")))[1],
r.squaredLR(glm(n_intact_fruits~prop_pred_c,family="poisson",data=subset(xpred,year=="2011" & LokalID == "Ler010")))[1],
r.squaredLR(glm(n_intact_fruits~prop_pred_c,family="poisson",data=subset(xpred,year=="2011" & LokalID == "Par003")))[1],
r.squaredLR(glm(n_intact_fruits~prop_pred_c,family="poisson",data=subset(xpred,year=="2011" & LokalID == "Vår004")))[1],
r.squaredLR(glm(n_intact_fruits~prop_pred_c,family="poisson",data=subset(xpred,year=="2011" & LokalID == "Vår009")))[1])
  
#Among-pops
xpredmean<-aggregate(cbind(n_intact_fruits,prop_pred_c), by = list(LokalID, year), data=xpred, FUN=mean)
xpredmean<-xpredmean[-c(4), ]   
xpredmean

summary(lm(n_intact_fruits~prop_pred_c,data=xpredmean))



