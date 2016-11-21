dates_correction <- read.table(
  "dates_correction.txt",
  header=TRUE, sep="\t", na.strings="NA", dec=",", strip.white=TRUE)
dates_correction

data10<-merge(data10,subset(dates_correction,year=="2010"))
head(data10)
str(data10)

data11<-merge(data11,subset(dates_correction,year=="2011"))
head(data11)
str(data11)
########################################################################

#Correct phenology measure
data10$phen_corr<-data10$most_adv+data10$apply_to_phen_1ago
with(data10,plot(most_adv,phen_corr))
with(data10,hist(most_adv))
with(data10,hist(phen_corr))
data10$phen_corr

with(subset(data10[1:2001,],predator==1&LokalID!="Göt016"),plot(LokalID,most_adv))
with(subset(data10[1:2001,],predator==1&LokalID!="Göt016"),plot(LokalID,phen_corr))

plotmeans(most_adv~LokalID,data=data10,subset=LokalID!="Göt016",connect=F,n.label=F,ylim=c(0,5),barcol="black")
plotmeans(phen_corr~LokalID,data=data10,subset=LokalID!="Göt016",connect=F,n.label=F,add=T,col="red",barcol="red")

data11$phen_corr<-data11$most_adv+data11$apply_to_phen_1ago
with(data11,plot(most_adv,phen_corr))
with(data11,hist(most_adv))
with(data11,hist(phen_corr))
data11$phen_corr

with(subset(data11,Predator==1&LokalID!="Göt016"),plot(LokalID,most_adv))
with(subset(data11,Predator==1&LokalID!="Göt016"),plot(LokalID,phen_corr))

plotmeans(most_adv~LokalID,data=data11,connect=F,n.label=F,ylim=c(0,5),barcol="black")
plotmeans(phen_corr~LokalID,data=data11,connect=F,n.label=F,add=T,col="red",barcol="red")

#Repeat analyses Table 3, with unstd variables + corrected phenology

int1_10a<-lm(n_eggs~most_adv+n_fl+h_shoot+LokalID*most_adv+
              LokalID*n_fl+LokalID*h_shoot,data=subset(data10[1:2001,],predator==1&LokalID!="Göt016"))
Anova(int1_10a,type="II")
int1_10b<-lm(n_eggs~phen_corr+n_fl+h_shoot+LokalID*phen_corr+
              LokalID*n_fl+LokalID*h_shoot,data=subset(data10[1:2001,],predator==1&LokalID!="Göt016"))
Anova(int1_10b,type="II") #Only slight change in population effect

Anova(glm(attack~most_adv+n_fl+h_shoot+LokalID*most_adv+
            LokalID*n_fl+LokalID*h_shoot,data=subset(data10[1:2001,],predator==1&LokalID!="Göt016"),family="binomial"),type="II")
summary(glm(attack~most_adv+n_fl+h_shoot+LokalID*most_adv+
            LokalID*n_fl+LokalID*h_shoot,data=subset(data10[1:2001,],predator==1&LokalID!="Göt016"),family="binomial"),type="II")


int1_11a<-lm(n_eggs~most_adv+n_fl+h_shoot+LokalID*most_adv+
              LokalID*n_fl+LokalID*h_shoot,data=subset(data11,Predator==1))
Anova(int1_11a,type="II")
int1_11b<-lm(n_eggs~phen_corr+n_fl+h_shoot+LokalID*phen_corr+
               LokalID*n_fl+LokalID*h_shoot,data=subset(data11,Predator==1))
Anova(int1_11b,type="II")

Anova(glm(attack~most_adv+n_fl+h_shoot+LokalID*most_adv+
            LokalID*n_fl+LokalID*h_shoot,data=subset(data11,Predator==1),family="binomial"),type="II")
summary(glm(attack~most_adv+n_fl+h_shoot+LokalID*most_adv+
            LokalID*n_fl+LokalID*h_shoot,data=subset(data11,Predator==1),family="binomial"),type="II")


