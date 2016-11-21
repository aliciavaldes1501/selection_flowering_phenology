#Models from Amos specification search

#All populations
sem_all_2010<- '
n_intact_fruits ~ attack+h_shoot+n_fl
attack ~ most_adv+h_shoot+n_fl
most_adv~~h_shoot
most_adv~~n_fl
h_shoot~~n_fl
'

fit_sem_all_2010 <- sem(sem_all_2010, data = data10_pred,fixed.x=FALSE,std.lv=FALSE,std.ov=TRUE,estimator="MLM") #Fit SEM to data
summary(fit_sem_all_2010, standardized = TRUE,fit.measures=TRUE,modindices = FALSE,rsquare=TRUE) #Get summary
semPaths(fit_sem_all_2010,what="stand",layout="spring",intercepts=FALSE) #Get graph with standardized estimates


meas<-measurementInvariance(sem_all_2010, data = data10_pred,group="LokalID",
                      fixed.x=FALSE,std.lv=FALSE,std.ov=TRUE,estimator="MLM",
                      strict=T,quiet=T)



sem_all_2011<- '
n_intact_fruits ~ attacked+h_shoot+n_fl
attacked ~ most_adv+h_shoot+n_fl
most_adv~~h_shoot
most_adv~~n_fl
h_shoot~~n_fl
'

fit_sem_all_2011 <- sem(sem_all_2011, data = data11_pred,fixed.x=FALSE,std.lv=FALSE,std.ov=TRUE,estimator="MLM") #Fit SEM to data
summary(fit_sem_all_2011, standardized = TRUE,fit.measures=TRUE,modindices = FALSE,rsquare=TRUE) #Get summary
semPaths(fit_sem_all_2011,what="stand",layout="spring",intercepts=FALSE) #Get graph with standardized estimates
