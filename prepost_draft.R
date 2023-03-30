#1. Rotavirus 

#Dose response value 
alpha<-2.53E-01
N50<-6.17E+00
beta<-N50/(2^(1/alpha)-1)

##1.1 Baseline scenario ----------------------------------------

Conc.h.b<- TE.all*Frac.HS*Conc.i.surface
Dose.b<- TE.face*Conc.h.b
Risk.b<- 1-(1+(Dose.b/beta))^(-alpha)

##1.2 Intervention scenario ----------------------------------------

Conc.h.i <- TE.all*Frac.HS*(Conc.i.surface/Reduc.intv)
Dose.i<- TE.face*Conc.h.i
Risk.i<- 1-(1+(Dose.i/beta))^(-alpha)

#plotting
library(ggplot2)
library(ggpubr)

Conc.h.df<-as.data.frame(t(Conc.h))
Dose.df<-as.data.frame(t(Dose))
Risk.df<-as.data.frame(t(Risk))

event<-rep(c(rep(1,iterations),rep(2,iterations)),3)

type<-c(rep("Hand",2*iterations),rep("Dose",2*iterations),rep("Risk",2*iterations))
value<-c(Conc.h.df$'1', Conc.h.df$'2',
         Dose.df$'1', Dose.df$'2',
         Risk.df$'1', Risk.df$'2')

data<-data.frame(event,type,value)

windows()
ggplot(data)+geom_violin(aes(x=event,y=value,fill=type, group=event),alpha=0.3,draw_quantiles = c(0.25,0.5,0.75))+
  facet_wrap(~type,scales="free") +
  scale_y_continuous(trans="log10")
