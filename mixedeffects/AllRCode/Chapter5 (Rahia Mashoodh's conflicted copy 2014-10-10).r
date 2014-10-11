#    Mixed Effects Models and Extensions in Ecology with R (2009)
#    Zuur, Ieno, Walker, Saveliev and Smith.    Springer
#    This file was produced by Alain Zuur (highstat@highstat.com)
#    www.highstat.com

setwd("~/Dropbox/Lab/R-Club/MixedEffects (RClub2)/ZuurDataMixedModelling") #specify your working dir
RIKZ<-read.table(file = "RIKZ.txt", header = TRUE, dec = ".") 

################################
## linear regression analysis ##
################################

bad <- lm(Richness ~ NAP, data=RIKZ)
summary(bad) #indicates there is an effect of NAP

#dignostics
#helfpul for interpreting the plots:
#http://stats.stackexchange.com/questions/58141/interpreting-plot-lm
plot(bad)
hist(bad$res)
boxplot(bad.model$res~sleepstudy$Days)

##but this is inappropriate because....

#can do some plotting
library(lattice)

xyplot(Richness~NAP | factor(Beach),
       xlab = "NAP", col=1,
       ylab = "species richness",
       data = RIKZ)

# add in loess fit line; the 'span' determines how rigid the line is
xyplot(Richness~NAP | factor(Beach),
       prepanel = function(x, y) prepanel.loess(x, y, span = 1),
       panel = function(x, y) {
         panel.grid(h = -1, v = 2)
         panel.xyplot(x, y)
         panel.loess(x, y, span=1)},
       xlab = "NAP", col=1,
       ylab = "species richness",
       data = RIKZ)


################################
###  2-Stage Analysis Method ###
################################

## STEP 1 ###

# compute a regression line for each beach
# model = intercept + beachslop

Beta<-vector(length=9)
for (i in 1:9){
 tmp<-summary(lm(Richness~NAP,subset = (Beach==i),data=RIKZ)) #regression line for each beach
 Beta[i]<-tmp$coefficients[2,1]                               #extracts the betas    
}

Beta #outputs beta values for each beach (n=9)
# lots of differences in Betas across each beach

#can do this with lme4 package using "lmList"
library(lme4)
Beta2 <- coef(lmList(Richness ~ NAP|Beach, data=RIKZ))


### STEP 2 ###

#model the estimated regression coefficients (from step 1) are modelled as a function of exposure
ExposureBeach <- factor(c(0, 0, 1, 1, 0, 1, 1, 0, 0)) #exp = factor with levels 0 and 1
tmp2 <- lm(Beta ~ factor(ExposureBeach),data=RIKZ)

##this is not significant, therefore no effect of exposure? but what about NAP values, ?

##########################
### Random Slope Model ###
##########################

### nlme version ###
library(nlme)
RIKZ$fBeach <- factor(RIKZ$Beach)
Mlme1 <- lme(Richness ~ NAP, random = ~1 | fBeach,data=RIKZ)
summary(Mlme1)

### lme4 version ###
library(lme4)
model.a <- lmer(Richness ~ NAP + (1|Beach), data=RIKZ)
summary(model.a)

# note lmer does not provide a p-value
# for this you can use lmertest
# apply Kenward-Roger correction or Satterthwaite correction (used in SAS)
library(lmerTest)
summary (model.a)

# another option is to do a model comparison
# run a model that has your random effect predicting your DV
model.a0 <- lmer(Richness ~ (1|fBeach), data=RIKZ)
anova(model.a0, model.a)

## how to interpret? ##

# can generate a plot
# one line represents the fitted values for the fixed effect (thicker, population model)
# other lines represent the fitted line for each individual beach, that can be above/below
# the fixed effect line

F0<-fitted(Mlme1,level=0)
F1<-fitted(Mlme1,level=1)
I<-order(RIKZ$NAP)
NAPs<-sort(RIKZ$NAP)
plot(NAPs,F0[I],lwd=4,type="l",ylim=c(0,22),
     ylab="Richness",xlab="NAP")
for (i in 1:9){
  x1<-RIKZ$NAP[RIKZ$Beach==i]
  y1<-F1[RIKZ$Beach==i]
  K<-order(x1)
  lines(sort(x1),y1[K])
}
text(RIKZ$NAP,RIKZ$Richness,RIKZ$Beach,cex=0.9)

######################################
### Random Slope & Intercept Model ###
######################################

# this is helpful
# http://stats.stackexchange.com/questions/31569/questions-about-how-random-effects-are-specified-in-lmer

# suppose that species richness and NAP is different on each beach...
# need to include an NAPxBeach interaction term
# apply the mixed effects model with a random intercept (as before) and a random slope

### nlme version ###
Mlme2 <- lme(Richness ~ NAP, random = ~1 + NAP | fBeach, data = RIKZ)
summary(Mlme2)

### lmer version ###
model.b <- lmer(Richness ~ NAP + (1+NAP|fBeach), data=RIKZ)
summary(model.b)


#############################
### Random Effects Models ###
#############################

# this model only contains the random effects
# i.e. no beta for fixed effect

### nlme version ###
Mlme3 <- lme(Richness ~ 1, random = ~1 | fBeach, data = RIKZ)
summary(Mlme3)

### lmer version ###
model.c <- lmer(Richness ~ (1+NAP|fBeach), data=RIKZ)
summary(model.c)

## why do these 2 somethings give different numbers?

M.mixed <- lme(Richness ~ NAP, random = ~1 | fBeach,
                 method = "REML", data = RIKZ)
M.gls <- gls(Richness ~ NAP, method = "REML",
          correlation = corCompSymm(form =~1 | fBeach),
          data = RIKZ)

##########################################################################



RIKZ$fExp<-RIKZ$Exposure
RIKZ$fExp[RIKZ$fExp==8]<-10
RIKZ$fExp<-factor(RIKZ$fExp,levels=c(10,11))
M0.ML <- lme(Richness ~ NAP, data = RIKZ,
              random = ~1| fBeach, method = "ML")
M0.REML <-lme(Richness ~ NAP, random = ~1|fBeach,
              method = "REML", data = RIKZ)
M1.ML <- lme(Richness ~ NAP+fExp, data = RIKZ,
              random = ~1| fBeach, method = "ML")
M1.REML <- lme(Richness ~NAP+fExp, data = RIKZ,
              random = ~1|fBeach, method = "REML")





Wrong1 <- gls(Richness ~ 1 + NAP, method = "REML",
               data = RIKZ)
Wrong2 <- lme(Richness ~ 1 + NAP, random = ~1|fBeach,
               method = "REML", data = RIKZ)
Wrong3 <- lme(Richness ~ 1 + NAP, method = "REML",
               random = ~1 + NAP | fBeach, data = RIKZ)



AIC(Wrong1,Wrong2,Wrong3)

RIKZ$fExp<-RIKZ$Exposure
RIKZ$fExp[RIKZ$fExp==8]<-10
RIKZ$fExp<-factor(RIKZ$fExp,levels=c(10,11))

Wrong4 <- lme(Richness ~1 + NAP * fExp,
             random = ~1 + NAP | fBeach,
             method = "REML", data = RIKZ)
anova(Wrong4)
summary(Wrong4)

#Drop the interaction

Wrong4 <- lme(Richness ~1 + NAP + fExp,
             random = ~1 + NAP | fBeach,
             method = "REML", data = RIKZ)

summary(Wrong4)









 lmc <- lmeControl(niterEM = 5200, msMaxIter = 5200)
 Wrong4A <- lme(Richness ~1 + NAP, method="ML",
             control = lmc, data = RIKZ,
               random = ~1+NAP|fBeach)
 Wrong4B <- lme(Richness ~ 1 + NAP + fExp,
               random = ~1 + NAP | fBeach, method="ML",
               data = RIKZ,control = lmc)
 Wrong4C <- lme(Richness ~1 + NAP * fExp,
               random = ~1 + NAP | fBeach, data = RIKZ,
               method = "ML", control = lmc)
 anova(Wrong4A, Wrong4B, Wrong4C)


 B1 <- gls(Richness ~ 1 + NAP * fExp,
            method = "REML", data = RIKZ)
 B2 <- lme(Richness ~1 + NAP * fExp, data = RIKZ,
        random = ~1 | fBeach, method = "REML")
 B3 <- lme(Richness ~ 1 + NAP * fExp,data = RIKZ,
        random = ~1 + NAP | fBeach, method = "REML")

summary(B2)

#Drop interaction
B2 <- lme(Richness ~1 + NAP + fExp, data = RIKZ,
        random = ~1 | fBeach, method = "REML")
summary(B2)



#Owls
library(AED) ; data(Owls)
names(Owls)

# "FoodTreatment"      "SexParent"
#[4] "ArrivalTime"        "SiblingNegotiation" "BroodSize"
#[7] "NegPerChick"



boxplot(NegPerChick~Nest,data=Owls)


boxplot(NegPerChick~FoodTreatment,data=Owls)
boxplot(NegPerChick~SexParent,data=Owls)

plot(x=Owls$ArrivalTime,y=Owls$NegPerChick)


M.lm=lm(NegPerChick~SexParent*FoodTreatment+SexParent*ArrivalTime,data=Owls)
plot(M.lm,select=c(1))

Owls$LogNeg<-log10(Owls$NegPerChick+1)
M2.lm=lm(LogNeg~SexParent*FoodTreatment+SexParent*ArrivalTime,data=Owls)
E=rstandard(M2.lm)

op<-par(mar=c(3,3,2,2))
boxplot(E~Nest,data=Owls,axes=FALSE,ylim=c(-3,3))
abline(0,0)
axis(2)
text(1:27,-2.5, levels(Owls$Nest),cex=0.75,srt=65)
par(op)



#Step 2 of protocol
library(nlme)
Form<-formula(LogNeg~SexParent*FoodTreatment+SexParent*ArrivalTime)
M.gls=gls(Form,data=Owls)

M1.lme=lme(Form,random=~1|Nest,method="REML",data=Owls)

anova(M.gls,M1.lme)

E2<-resid(M1.lme,type="normalized")
F2<-fitted(M1.lme)
op<-par(mfrow=c(2,2),mar=c(4,4,3,2))
MyYlab="Residuals"

plot(x=F2,y=E2,xlab="Fitted values",ylab=MyYlab)
boxplot(E2~SexParent,data=Owls,main="Sex of parent",ylab=MyYlab)
boxplot(E2~FoodTreatment,data=Owls,main="Food treatment",ylab=MyYlab)
plot(x=Owls$ArrivalTime,y=E,main="Arrival time",ylab=MyYlab,xlab="Time (hours)")
par(op)


#step 7 of the protocol
M1.lme=lme(Form,random=~1|Nest,method="REML",data=Owls)
summary(M1.lme)
anova(M1.lme)

M1.Full=lme(Form,random=~1|Nest,method="ML",data=Owls)
M1.A=update(M1.Full,.~.-SexParent:FoodTreatment)
M1.B=update(M1.Full,.~.-SexParent:ArrivalTime)
anova(M1.Full,M1.A)
anova(M1.Full,M1.B)


Form2<-formula(LogNeg~SexParent+FoodTreatment+SexParent*ArrivalTime)
M2.Full=lme(Form2, random= ~1| Nest, method = "ML", data = Owls)
M2.A=update(M2.Full, .~. -FoodTreatment)
M2.B=update(M2.Full, .~. -SexParent:ArrivalTime)
anova(M2.Full,M2.A)
anova(M2.Full,M2.B)



Form3 <- formula(LogNeg~SexParent+FoodTreatment+ArrivalTime)
M3.Full <- lme(Form3, random= ~1| Nest, method = "ML", data = Owls)
M3.A <- update(M3.Full, .~. -FoodTreatment)
M3.B <- update(M3.Full, .~. -SexParent)
M3.C <- update(M3.Full, .~. -ArrivalTime)
anova(M3.Full,M3.A)
anova(M3.Full,M3.B)
anova(M3.Full,M3.C)




Form4 <- formula(LogNeg ~ FoodTreatment + ArrivalTime)
M4.Full <- lme(Form4, random= ~1| Nest, method = "ML", data = Owls)
M4.A <- update(M4.Full, .~. -FoodTreatment)
M4.B <- update(M4.Full, .~. -ArrivalTime)
anova(M4.Full,M4.A)
anova(M4.Full,M4.B)


M5 <- lme(LogNeg ~ FoodTreatment + ArrivalTime, random= ~1| Nest, method = "REML", data = Owls)
summary(M5)


E2<-resid(M5,type="normalized")
F2<-fitted(M5)
op<-par(mfrow=c(2,2),mar=c(4,4,3,2))
MyYlab="Residuals"

plot(x=F2,y=E2,xlab="Fitted values",ylab=MyYlab)
boxplot(E2~SexParent,data=Owls,main="Sex of parent",ylab=MyYlab)
boxplot(E2~FoodTreatment,data=Owls,main="Food treatment",ylab=MyYlab)
plot(x=Owls$ArrivalTime,y=E,main="Arrival time",ylab=MyYlab,xlab="Time (hours)")
par(op)





library(lattice)

xyplot(E2~ArrivalTime|SexParent*FoodTreatment,data=Owls,
  ylab="Residuals",xlab="Arrival time (hours)",
      panel=function(x,y){
    panel.grid(h=-1, v= 2)
    panel.points(x,y,col=1)
    panel.loess(x,y,span=0.5,col=1,lwd=2)})
    
    


library(mgcv)
M6 <- gamm(LogNeg ~ FoodTreatment + +s(ArrivalTime),
        random=list(Nest=~1),data=Owls)
        
summary(M6)
plot(M6$gam)
anova(M6$gam)
summary(M6$gam)


M7=gamm(NegPerChick~FoodTreatment+
       s(ArrivalTime,by=as.numeric(FoodTreatment=="Deprived"))+
       s(ArrivalTime,by=as.numeric(FoodTreatment=="Satiated")),
        random=list(Nest=~1),data=Owls)

M8=gamm(NegPerChick~FoodTreatment+
       s(ArrivalTime,by=as.numeric(SexParent=="Female"))+
       s(ArrivalTime,by=as.numeric(SexParent=="Male")),
        random=list(Nest=~1),data=Owls)

AIC(M6$lme,M7$lme,M8$lme)

############################################

