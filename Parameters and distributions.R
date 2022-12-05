#File 1 Parameters and distributions 

library (truncdist)
library(triangle)
library(EnvStats)

iterations <- 10000
set.seed(1)

#Total hand area (cm^2)
T.handarea<-runif(iterations, min=445, max=535) 

#Fraction of hand surface(hand-surface, hand-face) (unitless)
Frac.KH <-runif(iterations, min=0.010, max=0.017)
Frac.HF <-runif(iterations, min=0.008, max=0.012) 


#Initial concentration on hand/face
Conc.i.hand.raw <-1.78E+08
Conc.i.hand<-Conc.i.hand.raw/T.handarea
Conc.i.face<-0

Conc.i.surface<-rlnormTrunc(iterations, meanlog =log(8.55E-04), sdlog =log(8.80), min = 0, max = 9.37E+04 )

# log Reduction (intervention)
Reduc.intv <-1.58


#Transfer efficiency
TE.all <-runif(iterations, min=0.0001, max=0.406)
TE.face<-rtrunc(iterations,"norm", mean=0.3390, sd=0.1318, a=0, b=1)


if (organism == "Rotavirus"){
  
  

  
  #Dose response value 
  alpha<-2.53E-01
  N50<-6.17E+00
  beta<-N50/(2^(1/alpha)-1)
  
  } else if(organism == "SARS-CoV-2"){  
  #Transfer efficiency
  TE.all <-runif(iterations, min=0.0001, max=0.406)
  TE.face<-rtrunc(iterations,"norm", mean=0.3390, sd=0.1318, a=0, b=1)
  
  } else if (organism=="InfluenzaA"){
  #Transfer efficiency
  TE.all <-runif(iterations, min=0.0001, max=0.406)
  TE.face<-rtrunc(iterations,"norm", mean=0.3390, sd=0.1318, a=0, b=1)
  
  #Dose response value 
  alpha<-0.581
  N50<-9.45E+05
  beta<-N50/(2^(1/alpha)-1)
  
  } else if (organism == "Rhinovirus"){
  #Transfer efficiency
  TE.all <-runif(iterations, min=0.0001, max=0.406)
  TE.face<-rtrunc(iterations,"norm", mean=0.3390, sd=0.1318, a=0, b=1)
  
  #Dose response value 
  alpha<-0.221
  N50<-1.81
  beta<-N50/(2^(1/alpha)-1)
  
  } else if (organism == "Norovirus"){
  
  #Transfer efficiency
  TE.all <-runif(iterations, min=0.0001, max=0.406)
  TE.face<-rtrunc(iterations,"norm", mean=0.3390, sd=0.1318, a=0, b=1)
  
  } else if (organism == "Adenovirus"){
    
    #Transfer efficiency
    TE.all <-runif(iterations, min=0.0001, max=0.406)
    TE.face<-rtrunc(iterations,"norm", mean=0.3390, sd=0.1318, a=0, b=1)
  
  } else if (organism == "E.coli"){
    #Transfer efficiency
    TE.all <-runif(iterations, min=0.0002, max=0.935)
    TE.face<-rtrunc(iterations,"norm", mean=0.3397, sd=0.1604, a=0, b=1)
    
    #Dose response value 
    alpha<-0.155
    N50<-2.11E+06
    beta<-N50/(2^(1/alpha)-1)
    
  } else if (organism == "Salmonella"){
    #Transfer efficiency
    TE.all <-runif(iterations, min=0.0002, max=0.935)
    TE.face<-rtrunc(iterations,"norm", mean=0.3397, sd=0.1604, a=0, b=1)
    
    #Dose response value 
    alpha<-0.318
    N50<-3.71E+04
    beta<-N50/(2^(1/alpha)-1)
    
  } else {#organism == "campylobacter"
  
    #Transfer efficiency
    TE.all <-runif(iterations, min=0.0002, max=0.935)
    TE.face<-rtrunc(iterations,"norm", mean=0.3397, sd=0.1604, a=0, b=1)
    
    #Dose response value 
    alpha<-0.144
    N50<-8.9E+02
    beta<-N50/(2^(1/alpha)-1)
}




