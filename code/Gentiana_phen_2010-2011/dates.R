#Read data
dates <- read.table(
  "dates.txt",
  header=TRUE, sep="\t", na.strings="NA", dec=",", strip.white=TRUE)
head(dates)
summary(dates)
str(dates)
dates$year<-as.factor(dates$year)

attach(dates)

plot(LokalID,most_adv)
plot(LokalID,phen_corr)

library(Rcmdr)

plotMeans(most_adv,LokalID)
plotMeans(phen_corr,LokalID)

summary(aov(phen_corr~LokalID))
TukeyHSD(aov(phen_corr~LokalID))

detach(dates)


dfc <- summarySE(dates, measurevar="phen_corr", groupvars=c("year","LokalID"))
library(ggplot2)

## set the levels in order we want
dfc <- within(dfc,phen_corr <- factor(phen_corr, levels=names(sort(table(phen_corr), decreasing=TRUE))))

ggplot(dfc, aes(x=LokalID, y=phen_corr, colour=year)) + 
  geom_errorbar(aes(ymin=phen_corr-se, ymax=phen_corr+se), width=.1) +
  geom_line() +
  geom_point()

pd <- position_dodge(.1) # move them .05 to the left and right

cbPalette <- c("#999999","#000000")

ggplot(dfc, aes(x=LokalID, y=phen_corr, colour=year, group=year)) + 
  geom_errorbar(aes(ymin=phen_corr-se, ymax=phen_corr+se), width=.5, position=pd) +
  geom_line(position=pd,width=150) +
  geom_point(position=pd, size=7, shape=20) + 
  xlab("LokalID") +
  ylab("Phenology (most_adv divided by Julian day)") +
  scale_colour_manual(values=cbPalette)+
  theme_bw() +
  theme(legend.justification=c(1,0), legend.position=c(1,0)) # Position legend in bottom right





