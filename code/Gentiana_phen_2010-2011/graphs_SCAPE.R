

par(mfrow=c(1,1),
    pch=20,family="serif",bty="l",cex=2.5,cex.axis=0.6,cex.lab=0.6,mgp=c(2, 1, 0),
    mar=c(4, 4, 0.5, 0.5) )
plot(ants_mean_max$logants,ants_mean_max$pred,xlab=expression(italic(Myrmica)~"abundance (log)"),
     ylab=expression(italic(P.alcon)~presence),bty="l",pch=19,xlim=c(-4, 4),col=adjustcolor("black", alpha=0.2))
curve(predict(logistic, data.frame(logants=x), type="resp"), add=TRUE,col ="black",lwd=3) 

pres_theme <- theme_bw(base_family = "")+theme(panel.grid.major = element_line(size = 0, color = "white"),
            axis.line = element_line(size=.7, color = "black"), text = element_text(size=18),
            axis.title.x = element_text(vjust=-0.50,size=18),axis.title.y = element_text(vjust=0.70,size=18),
            axis.text.x=element_text(size=18),axis.text.y=element_text(size=18))+
            theme( plot.background = element_blank() ,panel.grid.major = element_blank() ,
            panel.grid.minor = element_blank() , panel.border = element_blank() ,
            panel.background = element_blank() ) +  theme(axis.line = element_line(color = 'black'))

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#CC6666", "#9999CC", "#66CC99")

ggplot(data10_pred, aes(x=most_adv, y=attack,color=LokalID)) +
  geom_smooth(method=lm, se=FALSE, size=0.7,fullrange=T)+ 
  scale_x_continuous(breaks=c(0,1,2,3,4,5,6))+
  labs(x = "Most advanced bud", y = "Number of eggs")+
  pres_theme+scale_colour_manual(values=cbbPalette)

