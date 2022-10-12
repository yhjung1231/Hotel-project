organism<-"Norovirus"
source("Parameters and distributions.R")
Risk[2,]<- 0.722*(1-exp(-Dose[2,]/1106))

#1.A single fomite contact & a single orifice contact 

##1.0 Matrix -------------------------------------------------
numevents<-2
eventsname<-c("pre", "post")
Conc<-matrix(nrow=numevents,ncol=iterations)
rownames(Conc)<-eventsname

Dose<-matrix(nrow=numevents, ncol=iterations)
rownames(Dose)<-eventsname

Risk<-matrix(nrow=numevents, ncol=iterations)
rownames(Risk)<-eventsname

##1.1 Baseline scenario ----------------------------------------

Conc[1,] <- TE.all*Frac.HS*Conc.i.surface
Dose[1,]<- TE.face*T.handarea*Frac.HF*Conc [1, ]
Risk[1,]<- 0.722*(1-exp(-Dose[1,]/1106))

##1.2 Intervention scenario ----------------------------------------

Conc[2,] <- TE.all*Frac.HS*(Conc.i.surface/10^(Reduc.intv)) 
Dose[2,]<- TE.face*T.handarea*Frac.HF*Conc[2, ]
Risk[2,]<- 0.722*(1-exp(-Dose[2,]/1106))

##1.3 plotting---------------------------------------------------------------
library(ggplot2)
library(ggpubr)

Conc.df<-as.data.frame(t(Conc))
Dose.df<-as.data.frame(t(Dose))
Risk.df<-as.data.frame(t(Risk))

event<-rep(c(rep("pre",iterations),rep("post",iterations)),3)

type<-c(rep("Conc",2*iterations),rep("Dose",2*iterations),rep("Risk",2*iterations))
value<-c(Conc.df$"pre", Conc.df$"post", Dose.df$"pre", Dose.df$"post",Risk.df$"pre", Risk.df$"post")

data<-data.frame(event,type,value)

windows()
ggplot(data)+geom_violin(aes(x=event,y=value,fill=type, group=event),alpha=0.3,draw_quantiles = c(0.25,0.5,0.75))+
  facet_wrap(~type,scales="free") +
  scale_y_continuous(trans="log10") +
  scale_x_discrete(limits=c("pre","post"))+
  ggtitle("Comparison between Pre- and Post-intervention (Noro)")

ggsave("noro_intevention.tiff", dpi=600, dev='tiff', height=4, width=6, units="in")



##1.4 Data pulling-----------------------------------------------------------

#Conc
matrix.Conc<-matrix(nrow=2, ncol=4)
colnames(matrix.Conc)<-c('mean', 'sd', 'min', 'max')
rownames(matrix.Conc)<-c("pre","post")


for (f in 1:2){
  matrix.Conc[f,1]<-mean(Conc[f,])
  matrix.Conc[f,2]<-sd(Conc[f,])
  matrix.Conc[f,3]<-min(Conc[f,])
  matrix.Conc[f,4]<-max(Conc[f,])
}

#Dose
matrix.Dose<-matrix(nrow=2, ncol=4)
colnames(matrix.Dose)<-c('mean', 'sd', 'min', 'max')
rownames(matrix.Dose)<-c("pre","post")

for (f in 1:2){
  matrix.Dose[f,1]<-mean(Dose[f,])
  matrix.Dose[f,2]<-sd(Dose[f,])
  matrix.Dose[f,3]<-min(Dose[f,])
  matrix.Dose[f,4]<-max(Dose[f,])
}

#Risk 
matrix.Risk<-matrix(nrow=2, ncol=4)
colnames(matrix.Risk)<-c('mean', 'sd', 'min', 'max')
rownames(matrix.Risk)<-c('pre','post')

for (f in 1:2){
  matrix.Risk[f,1]<-mean(Risk[f,])
  matrix.Risk[f,2]<-sd(Risk[f,])
  matrix.Risk[f,3]<-min(Risk[f,])
  matrix.Risk[f,4]<-max(Risk[f,])
}
#Check the data 
View(matrix.Conc)
View(matrix.Dose)
View(matrix.Risk)

#Pull out the data 
library(openxlsx)
write.csv(matrix.Conc, file="Conc.noro.csv")
write.csv(matrix.Dose, file="Dose.noro.csv")
write.csv(matrix.Risk, file="Risk.noro.csv")


#--------------------------------------------------------------------------------
#Sensitivity Analysis---------------------------------------------------

spear.Rota<-data.frame(T.handarea, Surface.area.laundry, Frac.HS, Frac.HF, Item.laundry,
                       Contact.time.laundry, Contact.time.face.w, Contact.time.face.d, Contact.time.face.f,
                       Reduc.wash, Reduc.dry, Reduc.hwash, TE.dry, TE.wet, TE.face, Conc.feces, 
                       Mass.feces, Conc.onecloth, Inact.h, Inact.s, Risk.3[8,])  

spear.anal<-cor(spear.Rota,method="spearman")

View(spear.anal)

library(openxlsx)
write.xlsx(spear.anal, sheetName="Rota1", file="Sensitivity.rota1.xlsx")


