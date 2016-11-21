#SEMs for pops without predator
#To see how the direct effect of phenology looks!
#2010
data10_L<-subset(data10,LokalID=="Ale010")
data10_M<-subset(data10,LokalID=="Bor012")
data10_N<-subset(data10,LokalID=="Mar001")
data10_O<-subset(data10,LokalID=="Sve001")
data10_P<-subset(data10,LokalID=="Sve005")
data10_Q<-subset(data10,LokalID=="Sve011")
data10_R<-subset(data10,LokalID=="Sve013")
data10_S<-subset(data10,LokalID=="Tra001")
data10_T<-subset(data10,LokalID=="Tra002")

data10_sc<-data10
data10_sc$h_shoot<-scale(data10_sc$h_shoot)
data10_sc$n_fl<-scale(data10_sc$n_fl)
data10_sc$phen_corr<-scale(data10_sc$phen_corr)
data10_sc$n_eggs<-scale(data10_sc$n_eggs)
data10_sc$n_intact_fruits<-scale(data10_sc$n_intact_fruits)
data10_sc$attack<-scale(data10_sc$attack)

data10_L_sc<-subset(data10_sc,LokalID=="Ale010")
data10_M_sc<-subset(data10_sc,LokalID=="Bor012")
data10_N_sc<-subset(data10_sc,LokalID=="Mar001")
data10_O_sc<-subset(data10_sc,LokalID=="Sve001")
data10_P_sc<-subset(data10_sc,LokalID=="Sve005")
data10_Q_sc<-subset(data10_sc,LokalID=="Sve011")
data10_R_sc<-subset(data10_sc,LokalID=="Sve013")
data10_S_sc<-subset(data10_sc,LokalID=="Tra001")
data10_T_sc<-subset(data10_sc,LokalID=="Tra002")


#Saturated models, non-normal
model10_L<-list(
  glm(n_intact_fruits~phen_corr+n_fl+h_shoot,data=data10_L,family="poisson")
)
model10_M<-list(
  glm(n_intact_fruits~phen_corr+n_fl+h_shoot,data=data10_M,family="poisson")
)
model10_N<-list(
  glm(n_intact_fruits~phen_corr+n_fl+h_shoot,data=data10_N,family="poisson")
)
model10_O<-list(
  glm(n_intact_fruits~phen_corr+n_fl+h_shoot,data=data10_O,family="poisson")
)
model10_P<-list(
  glm(n_intact_fruits~phen_corr+n_fl+h_shoot,data=data10_P,family="poisson")
)
model10_Q<-list(
  glm(n_intact_fruits~phen_corr+n_fl+h_shoot,data=data10_Q,family="poisson")
)
model10_R<-list(
  glm(n_intact_fruits~phen_corr+n_fl+h_shoot,data=data10_R,family="poisson")
)
model10_S<-list(
  glm(n_intact_fruits~phen_corr+n_fl+h_shoot,data=data10_S,family="poisson")
)
model10_T<-list(
  glm(n_intact_fruits~phen_corr+n_fl+h_shoot,data=data10_T,family="poisson")
)


sem.coefs(model10_L,data=data10_L,corr.errors=c("phen_corr~~n_fl","phen_corr~~h_shoot","n_fl~~h_shoot"))
sem.coefs(model10_M,data=data10_M,corr.errors=c("phen_corr~~n_fl","phen_corr~~h_shoot","n_fl~~h_shoot"))
sem.coefs(model10_N,data=data10_N,corr.errors=c("phen_corr~~n_fl","phen_corr~~h_shoot","n_fl~~h_shoot"))
sem.coefs(model10_O,data=data10_O,corr.errors=c("phen_corr~~n_fl","phen_corr~~h_shoot","n_fl~~h_shoot"))
sem.coefs(model10_P,data=data10_P,corr.errors=c("phen_corr~~n_fl","phen_corr~~h_shoot","n_fl~~h_shoot"))
sem.coefs(model10_Q,data=data10_Q,corr.errors=c("phen_corr~~n_fl","phen_corr~~h_shoot","n_fl~~h_shoot"))
sem.coefs(model10_R,data=data10_R,corr.errors=c("phen_corr~~n_fl","phen_corr~~h_shoot","n_fl~~h_shoot"))
sem.coefs(model10_S,data=data10_S,corr.errors=c("phen_corr~~n_fl","phen_corr~~h_shoot","n_fl~~h_shoot"))
sem.coefs(model10_T,data=data10_T,corr.errors=c("phen_corr~~n_fl","phen_corr~~h_shoot","n_fl~~h_shoot"))

#Effect of phenology on fruits always positive

#2011
data11_L<-subset(data11,LokalID=="Ale010")
data11_M<-subset(data11,LokalID=="Bor012")

data11_R<-subset(data11,LokalID=="Sve013")
data11_S<-subset(data11,LokalID=="Tra001")
data11_T<-subset(data11,LokalID=="Tra002")

data11_sc<-data11
data11_sc$h_shoot<-scale(data11_sc$h_shoot)
data11_sc$n_fl<-scale(data11_sc$n_fl)
data11_sc$phen_corr<-scale(data11_sc$phen_corr)
data11_sc$n_eggs<-scale(data11_sc$n_eggs)
data11_sc$n_intact_fruits<-scale(data11_sc$n_intact_fruits)
data11_sc$attack<-scale(data11_sc$attack)

data11_L_sc<-subset(data11_sc,LokalID=="Ale010")
data11_M_sc<-subset(data11_sc,LokalID=="Bor012")

data11_R_sc<-subset(data11_sc,LokalID=="Sve013")
data11_S_sc<-subset(data11_sc,LokalID=="Tra001")
data11_T_sc<-subset(data11_sc,LokalID=="Tra002")


#Saturated models, non-normal
model11_L<-list(
  glm(n_intact_fruits~phen_corr+n_fl+h_shoot,data=data11_L,family="poisson")
)
model11_M<-list(
  glm(n_intact_fruits~phen_corr+n_fl+h_shoot,data=data11_M,family="poisson")
)

model11_R<-list(
  glm(n_intact_fruits~phen_corr+n_fl+h_shoot,data=data11_R,family="poisson")
)
model11_S<-list(
  glm(n_intact_fruits~phen_corr+n_fl+h_shoot,data=data11_S,family="poisson")
)
model11_T<-list(
  glm(n_intact_fruits~phen_corr+n_fl+h_shoot,data=data11_T,family="poisson")
)


sem.coefs(model11_L,data=data11_L,corr.errors=c("phen_corr~~n_fl","phen_corr~~h_shoot","n_fl~~h_shoot"))
sem.coefs(model11_M,data=data11_M,corr.errors=c("phen_corr~~n_fl","phen_corr~~h_shoot","n_fl~~h_shoot"))

sem.coefs(model11_R,data=data11_R,corr.errors=c("phen_corr~~n_fl","phen_corr~~h_shoot","n_fl~~h_shoot"))
sem.coefs(model11_S,data=data11_S,corr.errors=c("phen_corr~~n_fl","phen_corr~~h_shoot","n_fl~~h_shoot"))
sem.coefs(model11_T,data=data11_T,corr.errors=c("phen_corr~~n_fl","phen_corr~~h_shoot","n_fl~~h_shoot"))

#Effect of phenology on fruits always positive


