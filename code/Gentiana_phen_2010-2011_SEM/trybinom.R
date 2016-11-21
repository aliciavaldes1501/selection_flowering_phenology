trybinom <- read.table(
  "trybinom.txt",
  header=TRUE, sep="\t", na.strings="NA", dec=",", strip.white=TRUE)
trybinom
summary(glm(cbind(attacked, not_attacked)~mean_phen,family="binomial",data=trybinom))

plot()summary(glm(cbind(attacked, not_attacked)~mean_phen,family="binomial",data=subset(trybinom,year==2010)))
summary(glm(cbind(attacked, not_attacked)~mean_phen,family="binomial",data=subset(trybinom,year==2011)))

summary(lm(log(mean_n_eggs)~mean_phen,data=trybinom)) ##
plot(lm(log(mean_n_eggs)~mean_phen,data=trybinom))
trybinom$sum_n_eggs<-(trybinom$mean_n_eggs)*100
summary(glm(sum_n_eggs~mean_phen,family="poisson",data=trybinom)) #Signif, but highly overdisp
summary(glm(sum_n_eggs~mean_phen,family="quasipoisson",data=trybinom)) #Not signif
