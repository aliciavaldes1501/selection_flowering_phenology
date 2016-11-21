diffphen <- read.table(
  "diffphen.txt",
  header=TRUE, sep="\t", na.strings="NA", dec=",", strip.white=TRUE)
head(diffphen)
tail(diffphen)
summary(diffphen)
str(diffphen)

diffphen$predator<-as.factor(diffphen$predator)
diffphen$most_adv<-as.integer(diffphen$most_adv)

attach(diffphen)

with(subset(diffphen,year==2010),summary(lm(most_adv~predator+Individ%in%LokalID)))
with(subset(diffphen,year==2010),Anova(lm(most_adv~predator+Individ%in%LokalID),type="II"))
with(subset(diffphen,year==2010),summary(lmer(most_adv~predator+Individ%in%LokalID+(1|Datum))))
with(subset(diffphen,year==2010),Anova(lmer(most_adv~predator+Individ%in%LokalID+(1|Datum)),type="II"))

with(subset(diffphen,year==2011),summary(lm(most_adv~predator+Individ%in%LokalID)))
with(subset(diffphen,year==2011),Anova(lm(most_adv~predator+Individ%in%LokalID),type="II"))
with(subset(diffphen,year==2011),summary(lmer(most_adv~predator+Individ%in%LokalID+(1|Datum))))
with(subset(diffphen,year==2011),Anova(lmer(most_adv~predator+Individ%in%LokalID+(1|Datum)),type="II"))


