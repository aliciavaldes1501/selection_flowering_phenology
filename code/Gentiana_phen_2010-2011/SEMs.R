#SEMs
library(lavaan)     #Load the lavaan package 
library(semPlot)

#For all populations with predator
install.packages("lavaan", repos = "http://www.da.ugent.be", type = "source") 

#2010
head(data10_pred)

sem_all_2010<- '
n_intact_fruits ~ most_adv + attacked+h_shoot+n_fl
attacked ~ most_adv+h_shoot
'

fit_sem_all_2010 <- sem(sem_all_2010, data = data10_pred,std.lv=FALSE,std.ov=TRUE,estimator="MLM") #Fit SEM to data
summary(fit_sem_all_2010, standardized = TRUE,fit.measures=TRUE,modindices = FALSE,rsquare=TRUE) #Get summary
semPaths(fit_sem_all_2010,what="stand",layout="spring",intercepts=FALSE) #Get graph with standardized estimates

#2011
head(data11_pred)

sem_all_2011<- '
n_intact_fruits ~ attacked+h_shoot+n_fl
attacked ~ most_adv+h_shoot+n_fl
'

fit_sem_all_2011 <- sem(sem_all_2011, data = data11_pred,std.lv=FALSE,std.ov=TRUE,estimator="MLM") #Fit SEM to data
summary(fit_sem_all_2011, standardized = TRUE,fit.measures=TRUE,modindices = FALSE,rsquare=TRUE) #Get summary
semPaths(fit_sem_all_2011,what="stand",layout="spring",intercepts=FALSE) #Get graph with standardized estimates

#Models from Amos specification search

#All populations
sem_all_2010<- '
n_intact_fruits ~ most_adv + n_eggs+h_shoot+n_fl
n_eggs ~ most_adv+h_shoot
most_adv~~h_shoot
most_adv~~n_fl
h_shoot~~n_fl
'

fit_sem_all_2010 <- sem(sem_all_2010, data = data10_pred,fixed.x=FALSE,std.lv=FALSE,std.ov=TRUE,estimator="MLM") #Fit SEM to data
summary(fit_sem_all_2010, standardized = TRUE,fit.measures=TRUE,modindices = FALSE,rsquare=TRUE) #Get summary
semPaths(fit_sem_all_2010,what="stand",layout="spring",intercepts=FALSE) #Get graph with standardized estimates

sem_all_2011<- '
n_intact_fruits ~ n_eggs+h_shoot+n_fl
n_eggs ~ most_adv+h_shoot+n_fl
most_adv~~h_shoot
most_adv~~n_fl
h_shoot~~n_fl
'

fit_sem_all_2011 <- sem(sem_all_2011, data = data11_pred,fixed.x=FALSE,std.lv=FALSE,std.ov=TRUE,estimator="MLM") #Fit SEM to data
summary(fit_sem_all_2011, standardized = TRUE,fit.measures=TRUE,modindices = FALSE,rsquare=TRUE) #Get summary
semPaths(fit_sem_all_2011,what="stand",layout="spring",intercepts=FALSE) #Get graph with standardized estimates

#Ale001
sem_Ale001_2010<- '
n_intact_fruits ~ n_eggs+n_fl
n_eggs ~ most_adv+n_fl
most_adv~~h_shoot
most_adv~~n_fl
h_shoot~~n_fl
'

fit_sem_Ale001_2010 <- sem(sem_Ale001_2010, data = subset(data10_pred,LokalID=="Ale001"),fixed.x=FALSE,
                           std.lv=FALSE,std.ov=TRUE,estimator="MLM") #Fit SEM to data
summary(fit_sem_Ale001_2010, standardized = TRUE,fit.measures=TRUE,modindices = FALSE,rsquare=TRUE) #Get summary
semPaths(fit_sem_Ale001_2010,what="stand",layout="spring",intercepts=FALSE) #Get graph with standardized estimates

sem_Ale001_2011<- '
n_intact_fruits ~ n_eggs+n_fl+h_shoot
n_eggs ~ most_adv+n_fl
most_adv~~h_shoot
most_adv~~n_fl
h_shoot~~n_fl
'

fit_sem_Ale001_2011 <- sem(sem_Ale001_2011, data = subset(data11_pred,LokalID=="Ale001"),fixed.x=FALSE,
                           std.lv=FALSE,std.ov=TRUE,estimator="MLM") #Fit SEM to data
summary(fit_sem_Ale001_2011, standardized = TRUE,fit.measures=TRUE,modindices = FALSE,rsquare=TRUE) #Get summary
semPaths(fit_sem_Ale001_2011,what="stand",layout="spring",intercepts=FALSE) #Get graph with standardized estimates

#Göt009a
sem_Göt009a_2010<- '
n_intact_fruits ~ n_eggs+n_fl+most_adv
n_eggs ~ h_shoot+n_fl
most_adv~~h_shoot
most_adv~~n_fl
h_shoot~~n_fl
'

fit_sem_Göt009a_2010 <- sem(sem_Göt009a_2010, data = subset(data10_pred,LokalID=="Göt009a"),fixed.x=FALSE,
                           std.lv=FALSE,std.ov=TRUE,estimator="MLM") #Fit SEM to data
summary(fit_sem_Göt009a_2010, standardized = TRUE,fit.measures=TRUE,modindices = FALSE,rsquare=TRUE) #Get summary
semPaths(fit_sem_Göt009a_2010,what="stand",layout="spring",intercepts=FALSE) #Get graph with standardized estimates

sem_Göt009a_2011<- '
n_intact_fruits ~ n_eggs+n_fl+h_shoot
n_eggs ~ most_adv+n_fl
most_adv~~h_shoot
most_adv~~n_fl
h_shoot~~n_fl
'

fit_sem_Göt009a_2011 <- sem(sem_Göt009a_2011, data = subset(data11_pred,LokalID=="Göt009a"),fixed.x=FALSE,
                           std.lv=FALSE,std.ov=TRUE,estimator="MLM") #Fit SEM to data
summary(fit_sem_Göt009a_2011, standardized = TRUE,fit.measures=TRUE,modindices = FALSE,rsquare=TRUE) #Get summary
semPaths(fit_sem_Göt009a_2011,what="stand",layout="spring",intercepts=FALSE) #Get graph with standardized estimates

#Göt009b
sem_Göt009b_2010<- '
n_intact_fruits ~ n_eggs+n_fl
n_eggs ~ h_shoot+most_adv
most_adv~~h_shoot
most_adv~~n_fl
h_shoot~~n_fl
'

fit_sem_Göt009b_2010 <- sem(sem_Göt009b_2010, data = subset(data10_pred,LokalID=="Göt009b"),fixed.x=FALSE,
                            std.lv=FALSE,std.ov=TRUE,estimator="MLM") #Fit SEM to data
summary(fit_sem_Göt009b_2010, standardized = TRUE,fit.measures=TRUE,modindices = FALSE,rsquare=TRUE) #Get summary
semPaths(fit_sem_Göt009b_2010,what="stand",layout="spring",intercepts=FALSE) #Get graph with standardized estimates

sem_Göt009b_2011<- '
n_intact_fruits ~ n_fl+h_shoot
n_eggs ~ n_fl
most_adv~~h_shoot
most_adv~~n_fl
h_shoot~~n_fl
'

fit_sem_Göt009b_2011 <- sem(sem_Göt009b_2011, data = subset(data11_pred,LokalID=="Göt009b"),fixed.x=FALSE,
                            std.lv=FALSE,std.ov=TRUE,estimator="MLM") #Fit SEM to data
summary(fit_sem_Göt009b_2011, standardized = TRUE,fit.measures=TRUE,modindices = FALSE,rsquare=TRUE) #Get summary
semPaths(fit_sem_Göt009b_2011,what="stand",layout="spring",intercepts=FALSE) #Get graph with standardized estimates

#Göt016
sem_Göt016_2010<- '
n_intact_fruits ~ n_fl
n_eggs ~ h_shoot
most_adv~~n_fl
h_shoot~~n_fl
'

fit_sem_Göt016_2010 <- sem(sem_Göt016_2010, data = subset(data10_pred,LokalID=="Göt016"),fixed.x=FALSE,
                            std.lv=FALSE,std.ov=TRUE,estimator="MLM") #Fit SEM to data
summary(fit_sem_Göt016_2010, standardized = TRUE,fit.measures=TRUE,modindices = FALSE,rsquare=TRUE) #Get summary
semPaths(fit_sem_Göt016_2010,what="stand",layout="spring",intercepts=FALSE) #Get graph with standardized estimates

sem_Göt016_2011<- '
n_intact_fruits ~ n_fl
n_eggs ~ n_fl+most_adv
most_adv~~h_shoot
most_adv~~n_fl
h_shoot~~n_fl
'

fit_sem_Göt016_2011 <- sem(sem_Göt016_2011, data = subset(data11_pred,LokalID=="Göt016"),fixed.x=FALSE,
                            std.lv=FALSE,std.ov=TRUE,estimator="MLM") #Fit SEM to data
summary(fit_sem_Göt016_2011, standardized = TRUE,fit.measures=TRUE,modindices = FALSE,rsquare=TRUE) #Get summary
semPaths(fit_sem_Göt016_2011,what="stand",layout="spring",intercepts=FALSE) #Get graph with standardized estimates

#Her003
sem_Her003_2010<- '
n_intact_fruits ~ n_eggs+n_fl
n_eggs ~ n_fl
most_adv~~h_shoot
most_adv~~n_fl
h_shoot~~n_fl
'

fit_sem_Her003_2010 <- sem(sem_Her003_2010, data = subset(data10_pred,LokalID=="Her003"),fixed.x=FALSE,
                            std.lv=FALSE,std.ov=TRUE,estimator="MLM") #Fit SEM to data
summary(fit_sem_Her003_2010, standardized = TRUE,fit.measures=TRUE,modindices = FALSE,rsquare=TRUE) #Get summary
semPaths(fit_sem_Her003_2010,what="stand",layout="spring",intercepts=FALSE) #Get graph with standardized estimates

sem_Her003_2011<- '
n_intact_fruits ~ n_eggs+n_fl
n_eggs ~ n_fl+most_adv
most_adv~~h_shoot
most_adv~~n_fl
h_shoot~~n_fl
'

fit_sem_Her003_2011 <- sem(sem_Her003_2011, data = subset(data11_pred,LokalID=="Her003"),fixed.x=FALSE,
                            std.lv=FALSE,std.ov=TRUE,estimator="MLM") #Fit SEM to data
summary(fit_sem_Her003_2011, standardized = TRUE,fit.measures=TRUE,modindices = FALSE,rsquare=TRUE) #Get summary
semPaths(fit_sem_Her003_2011,what="stand",layout="spring",intercepts=FALSE) #Get graph with standardized estimates

#Her004
sem_Her004_2010<- '
n_intact_fruits ~ n_eggs+n_fl
n_eggs ~ n_fl+most_adv
most_adv~~h_shoot
most_adv~~n_fl
h_shoot~~n_fl
'

fit_sem_Her004_2010 <- sem(sem_Her004_2010, data = subset(data10_pred,LokalID=="Her004"),fixed.x=FALSE,
                           std.lv=FALSE,std.ov=TRUE,estimator="MLM") #Fit SEM to data
summary(fit_sem_Her004_2010, standardized = TRUE,fit.measures=TRUE,modindices = FALSE,rsquare=TRUE) #Get summary
semPaths(fit_sem_Her004_2010,what="stand",layout="spring",intercepts=FALSE) #Get graph with standardized estimates

sem_Her004_2011<- '
n_intact_fruits ~ n_eggs+n_fl
n_eggs ~ n_fl+most_adv
most_adv~~h_shoot
most_adv~~n_fl
h_shoot~~n_fl
'

fit_sem_Her004_2011 <- sem(sem_Her004_2011, data = subset(data11_pred,LokalID=="Her004"),fixed.x=FALSE,
                           std.lv=FALSE,std.ov=TRUE,estimator="MLM") #Fit SEM to data
summary(fit_sem_Her004_2011, standardized = TRUE,fit.measures=TRUE,modindices = FALSE,rsquare=TRUE) #Get summary
semPaths(fit_sem_Her004_2011,what="stand",layout="spring",intercepts=FALSE) #Get graph with standardized estimates

#Par003
sem_Par003_2010<- '
n_intact_fruits ~ most_adv+n_fl
n_eggs ~ n_fl
most_adv~~h_shoot
most_adv~~n_fl
h_shoot~~n_fl
'

fit_sem_Par003_2010 <- sem(sem_Par003_2010, data = subset(data10_pred,LokalID=="Par003"),fixed.x=FALSE,
                           std.lv=FALSE,std.ov=TRUE,estimator="MLM") #Fit SEM to data
summary(fit_sem_Par003_2010, standardized = TRUE,fit.measures=TRUE,modindices = FALSE,rsquare=TRUE) #Get summary
semPaths(fit_sem_Par003_2010,what="stand",layout="spring",intercepts=FALSE) #Get graph with standardized estimates

sem_Par003_2011<- '
n_intact_fruits ~ n_eggs+n_fl+h_shoot
n_eggs ~ n_fl
most_adv~~h_shoot
most_adv~~n_fl
h_shoot~~n_fl
'

fit_sem_Par003_2011 <- sem(sem_Par003_2011, data = subset(data11_pred,LokalID=="Par003"),fixed.x=FALSE,
                           std.lv=FALSE,std.ov=TRUE,estimator="MLM") #Fit SEM to data
summary(fit_sem_Par003_2011, standardized = TRUE,fit.measures=TRUE,modindices = FALSE,rsquare=TRUE) #Get summary
semPaths(fit_sem_Par003_2011,what="stand",layout="spring",intercepts=FALSE) #Get graph with standardized estimates

#Ler010
sem_Ler010_2010<- '
n_intact_fruits ~ n_eggs+n_fl
n_eggs ~ n_fl+most_adv
most_adv~~h_shoot
most_adv~~n_fl
h_shoot~~n_fl
'

fit_sem_Ler010_2010 <- sem(sem_Ler010_2010, data = subset(data10_pred,LokalID=="Ler010"),fixed.x=FALSE,
                           std.lv=FALSE,std.ov=TRUE,estimator="MLM") #Fit SEM to data
summary(fit_sem_Ler010_2010, standardized = TRUE,fit.measures=TRUE,modindices = FALSE,rsquare=TRUE) #Get summary
semPaths(fit_sem_Ler010_2010,what="stand",layout="spring",intercepts=FALSE) #Get graph with standardized estimates

sem_Ler010_2011<- '
n_intact_fruits ~ n_eggs+n_fl
n_eggs ~ n_fl+most_adv
most_adv~~n_fl
h_shoot~~n_fl
'

fit_sem_Ler010_2011 <- sem(sem_Ler010_2011, data = subset(data11_pred,LokalID=="Ler010"),fixed.x=FALSE,
                           std.lv=FALSE,std.ov=TRUE,estimator="MLM") #Fit SEM to data
summary(fit_sem_Ler010_2011, standardized = TRUE,fit.measures=TRUE,modindices = FALSE,rsquare=TRUE) #Get summary
semPaths(fit_sem_Ler010_2011,what="stand",layout="spring",intercepts=FALSE) #Get graph with standardized estimates

#Par003
sem_Par003_2010<- '
n_intact_fruits ~ most_adv+h_shoot
n_eggs ~ n_fl+most_adv
most_adv~~h_shoot
most_adv~~n_fl
h_shoot~~n_fl
'

fit_sem_Par003_2010 <- sem(sem_Par003_2010, data = subset(data10_pred,LokalID=="Par003"),fixed.x=FALSE,
                           std.lv=FALSE,std.ov=TRUE,estimator="MLM") #Fit SEM to data
summary(fit_sem_Par003_2010, standardized = TRUE,fit.measures=TRUE,modindices = FALSE,rsquare=TRUE) #Get summary
semPaths(fit_sem_Par003_2010,what="stand",layout="spring",intercepts=FALSE) #Get graph with standardized estimates

sem_Par003_2011<- '
n_intact_fruits ~ n_eggs+most_adv+h_shoot
n_eggs ~ n_fl+most_adv
most_adv~~h_shoot
most_adv~~n_fl
h_shoot~~n_fl
'

fit_sem_Par003_2011 <- sem(sem_Par003_2011, data = subset(data11_pred,LokalID=="Par003"),fixed.x=FALSE,
                           std.lv=FALSE,std.ov=TRUE,estimator="MLM") #Fit SEM to data
summary(fit_sem_Par003_2011, standardized = TRUE,fit.measures=TRUE,modindices = FALSE,rsquare=TRUE) #Get summary
semPaths(fit_sem_Par003_2011,what="stand",layout="spring",intercepts=FALSE) #Get graph with standardized estimates

#Vår004
sem_Vår004_2010<- '
n_intact_fruits ~ n_eggs+h_shoot
n_eggs ~ n_fl+most_adv
most_adv~~n_fl
h_shoot~~n_fl
'

fit_sem_Vår004_2010 <- sem(sem_Vår004_2010, data = subset(data10_pred,LokalID=="Vår004"),fixed.x=FALSE,
                           std.lv=FALSE,std.ov=TRUE,estimator="MLM") #Fit SEM to data
summary(fit_sem_Vår004_2010, standardized = TRUE,fit.measures=TRUE,modindices = FALSE,rsquare=TRUE) #Get summary
semPaths(fit_sem_Vår004_2010,what="stand",layout="spring",intercepts=FALSE) #Get graph with standardized estimates

sem_Vår004_2011<- '
n_intact_fruits ~ n_fl
n_eggs ~ n_fl+most_adv
most_adv~~h_shoot
most_adv~~n_fl
h_shoot~~n_fl
'

fit_sem_Vår004_2011 <- sem(sem_Vår004_2011, data = subset(data11_pred,LokalID=="Vår004"),fixed.x=FALSE,
                           std.lv=FALSE,std.ov=TRUE,estimator="MLM") #Fit SEM to data
summary(fit_sem_Vår004_2011, standardized = TRUE,fit.measures=TRUE,modindices = FALSE,rsquare=TRUE) #Get summary
semPaths(fit_sem_Vår004_2011,what="stand",layout="spring",intercepts=FALSE) #Get graph with standardized estimates

#Vår009
sem_Vår009_2010<- '
n_intact_fruits ~ n_eggs+n_fl
n_eggs ~ h_shoot
most_adv~~h_shoot
most_adv~~n_fl
h_shoot~~n_fl
'

fit_sem_Vår009_2010 <- sem(sem_Vår009_2010, data = subset(data10_pred,LokalID=="Vår009"),fixed.x=FALSE,
                           std.lv=FALSE,std.ov=TRUE,estimator="MLM") #Fit SEM to data
summary(fit_sem_Vår009_2010, standardized = TRUE,fit.measures=TRUE,modindices = FALSE,rsquare=TRUE) #Get summary
semPaths(fit_sem_Vår009_2010,what="stand",layout="spring",intercepts=FALSE) #Get graph with standardized estimates

sem_Vår009_2011<- '
n_intact_fruits ~ n_fl+n_eggs
n_eggs ~ n_fl+h_shoot
most_adv~~h_shoot
most_adv~~n_fl
h_shoot~~n_fl
'

fit_sem_Vår009_2011 <- sem(sem_Vår009_2011, data = subset(data11_pred,LokalID=="Vår009"),fixed.x=FALSE,
                           std.lv=FALSE,std.ov=TRUE,estimator="MLM") #Fit SEM to data
summary(fit_sem_Vår009_2011, standardized = TRUE,fit.measures=TRUE,modindices = FALSE,rsquare=TRUE) #Get summary
semPaths(fit_sem_Vår009_2011,what="stand",layout="spring",intercepts=FALSE) #Get graph with standardized estimates








