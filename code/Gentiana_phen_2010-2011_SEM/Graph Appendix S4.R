library(ggplot2)

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#F0E442", "#0072B2", 
                "#D55E00", "#CC79A7","#CC6666", "#9999CC", "#66CC99")

ggplot(data10_pred, aes(x=most_adv, y=n_eggs,color=LokalID)) +
  geom_smooth(method=lm, se=FALSE, size=0.7,fullrange=T)+ 
  scale_x_continuous(breaks=c(0,1,2,3,4,5,6))+
  labs(x = "Phenology (early flowering)", y = "Number of eggs")+
  theme_bw(base_family = "serif")+scale_colour_manual(values=cbbPalette)

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", 
                "#D55E00", "#CC79A7","#CC6666", "#9999CC", "#66CC99")

ggplot(data11_pred, aes(x=most_adv, y=n_eggs,color=LokalID)) +
  geom_smooth(method=lm, se=FALSE, size=0.7,fullrange=T)+ 
  scale_x_continuous(breaks=c(0,1,2,3,4,5,6))+
    labs(x = "Phenology (early flowering)", y = "Number of eggs")+
  theme_bw(base_family = "serif")+scale_colour_manual(values=cbbPalette)

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#F0E442", "#0072B2", 
                "#D55E00", "#CC79A7","#CC6666", "#9999CC", "#66CC99")

ggplot(data10_pred, aes(x=phen_corr, y=n_eggs,color=LokalID)) +
  geom_smooth(method=lm, se=FALSE, size=0.7,fullrange=T)+ 
  scale_x_continuous(breaks=c(0,1,2,3,4,5,6,7,8))+
  labs(x = "Phenology (early flowering)", y = "Number of eggs")+
  theme_bw(base_family = "serif")+scale_colour_manual(values=cbbPalette)

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", 
                "#D55E00", "#CC79A7","#CC6666", "#9999CC", "#66CC99")

ggplot(data11_pred, aes(x=phen_corr, y=n_eggs,color=LokalID)) +
  geom_smooth(method=lm, se=FALSE, size=0.7,fullrange=T)+ 
  scale_x_continuous(breaks=c(0,1,2,3,4,5,6,7,8))+
  labs(x = "Phenology (early flowering)", y = "Number of eggs")+
  theme_bw(base_family = "serif")+scale_colour_manual(values=cbbPalette)




