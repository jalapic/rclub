# load data & libraries
setwd("~/Dropbox/R-Club/MixedEffects/")

library(doBy)   # contrasts
library(nlme)    # mixed effects modeling (guasian distributions)
library(lme4)   # general mixed effects modeling

####################
## visualize data ##
####################
# we'll start with a simple dataset looking at effect of sleep deprivation on RT
colnames(sleepstudy)

# now well use the lattice package (comes with lmer) to look at the relationship between 
# days with sleep deprivation and RT

# simple dot plot
xyplot(Reaction~Days | factor(Subject),
       xlab = "number of days sleep deprived", col=1,
       ylab = "reaction time",
       data = sleepstudy)

# add in loess fit line; the 'span' determines how rigid the line is
xyplot(Reaction~Days | factor(Subject),
       prepanel = function(x, y) prepanel.loess(x, y, span = 1),
       panel = function(x, y) {
         panel.grid(h = -1, v = 2)
         panel.xyplot(x, y)
         panel.loess(x, y, span=1)},
       xlab = "number of days sleep deprived", col=1,
       ylab = "reaction time",
       data = sleepstudy)

#######################################
## Inappropriate regression analysis ##
#######################################

bad.model<-lm(Reaction~Days,data=sleepstudy)
summary(bad.model)

#dignostics
plot(bad.model)
hist(bad.model$res)
boxplot(bad.model$res~sleepstudy$Days)


# what's worse is that inferences are done with 178 df, but data within 
# subjects will be correlated so this is an overestimate
# also this model does not adjust for individual differences in the sample
# so inferences are restricted to the data leaving it unclear what should be 
# extrapolated to the population


######################
## 2-Stage Analysis ##
######################

# a simple solution is to get a summary statistic which reflects the magnitude of 
# effect in each subject and then see how reliable it is across participants
# running model in each subject 

Beta <- vector(length = 18) # make a vector of length N (subject #)
# loop through each subject running a regression
sCount=0
for (i in levels(sleepstudy$Subject)){
  sCount=sCount+1
  model <- lm(Reaction~Days,data=sleepstudy, subset=(Subject==i))
  Mi<-summary(model) # taking out the coefficients 
  Beta[sCount] <- Mi$coefficients[2, 1]
}

Beta # summary statistic, strength of relationship for each subject 

# fDays<-1:10
Two.step<-lm(Beta~1)
summary(Two.step)

summary(bad.model) # compare to regression model - main difference is in degrees of freedom

##################
## Mixed Models ##
##################

# but this approach looses a lot of information about the strucuture of the data
# - are some subjects noisier than others, do some subjects have more data?
# - may underestimate degrees of freedom because the number of data points per subject not taken into account

# Random intercept model with lme
lme.model<-lme(Reaction~Days, random= ~1|Subject, data=sleepstudy) #allows each subject to have different RTs (intercept)
summary(lme.model) #be cautious with p-values in mixed models; df is tricky so all stats are 'estimated'

# Random intercept model with lmer
lmer.model<-lmer(Reaction~Days+(1|Subject), data=sleepstudy) #random effect specified in parenthises
summary(lmer.model) #note you aren't given p-values here because df issue

#if you want to force lmer to give you a p-value you have a few options:
## Model comparison
lmer.model0<-lmer(Reaction~(1|Subject), data=sleepstudy,)
anova(lmer.model0,lmer.model)


## apply Kenward-Roger correction or Satterthwaite correction (used in SAS)
install.packages("lmerTest")
library(lmerTest) # masks lmer so need to re-run model
lmer.model_pval<-lmer(Reaction~Days+(1|Subject), data=sleepstudy)
summary(lmer.model_pval)
anova(lmer.model_pval, ddf="Kenward-Roger")

# Random slope + intercept model # random effect of subject and days 
lmer.model2a<-lmer(Reaction~Days+(1|Subject)+(0+Days|Subject), data=sleepstudy)

# seperately estimating slope and intercept; 1 specifies intercept; 0 is required because there is an implicit +1 
lmer.model2b<-lmer(Reaction~Days+(1+Days|Subject), data=sleepstudy)
lmer.model2c<-lmer(Reaction~Days+(Days|Subject), data=sleepstudy) #b and c are equivalent
summary(lmer.model2)

# test whether addition of random slope improved model
anova(lmer.model,lmer.model2)

# calculate random intercept and slope independently (assume that random effects are not correlated)
lmer.model2_nocor<-lmer(Reaction~Days+(0+Days|Subject)+(1|Subject), data=sleepstudy)
anova(lmer.model,lmer.model2_nocor,lmer.model2) #added covariance structure may not be needed

# can look at ranef of slope
plot(x=ranef(lmer.model2_nocor)$Subject$Days, y=t(Beta),xlab="ranef",ylab="regresion beta")
plot(x=ranef(lmer.model2)$Subject$Days, y=t(Beta),xlab="ranef",ylab="regresion beta")

# ML and REML: mixed effects models are fit by maximizing maximum likelihood, not minimizing OLS. 
# by default lmer uses restricted maximum likelihood (REML) which is less biases in its standard error estimates
# when comparing models using anova(), models are refit using ML 
# shoud not make a huge diference for models with lots of data and not many predictors, but differences get larger when you have big models

lmer.model2_nocor.ML<-lmer(Reaction~Days+(0+Days|Subject)+(1|Subject), data=sleepstudy,REML=F)
summary(lmer.model2_nocor.ML)
summary(lmer.model2_nocor)

# Model selection

#load in a new dataset for more complex analyses
MemDec <- read.csv("MemDec.csv")
colnames(MemDec)

# visualize
xyplot(ChooseOld~PriorPay | factor(Sub),groups=(PSNew),
       prepanel = function(x, y) prepanel.spline(x, y, span = 1, npoint=4),
       panel = function(x, y) {
         panel.grid(h = -1, v = 2)
         panel.xyplot(x, y,jitter.x=TRUE, factor=1,"p")
         panel.spline(x, y,npoints=4)},
       xlab = "old card value", col=1,
       ylab = "choose old card",
       data = MemDec)

# Start with large Fixed Effects model while dertermining Random Effects
Model.FXFull.Rand0<-lmer(ChooseOld~PriorPayc*PSNew+PriorPayc*PSNewEnc+(1|Sub),data=MemDec,family=binomial)

# Fixed effects of PSNew and PSNewEnc are correlated with interaction terms, can re-code as effects (not dummy)
MemDec$PSNewEf<-MemDec$PSNew*2-1
MemDec$PSNewEncEf<-MemDec$PSNewEnc*2-1

Model.FXFull.Rand0<-lmer(ChooseOld~PriorPayc*PSNewEf+PriorPayc*PSNewEncEf+(1|Sub),data=MemDec,family=binomial)

# Start Adding random effects that make sense
Model.FXFull.Rand1<-lmer(ChooseOld~PriorPayc*PSNewEf+PriorPayc*PSNewEncEf+(1|Sub)+(0+PriorPayc|Sub),data=MemDec,family=binomial)
Model.FXFull.Rand2<-lmer(ChooseOld~PriorPayc*PSNewEf+PriorPayc*PSNewEncEf+(1|Sub)+(0+PriorPayc|Sub)+(0+PSNewEf|Sub),data=MemDec,family=binomial)
Model.FXFull.Rand3<-lmer(ChooseOld~PriorPayc*PSNewEf+PriorPayc*PSNewEncEf+(1|Sub)+(0+PriorPayc|Sub)+(0+PSNewEf|Sub)+(0+PSNewEncEf|Sub),data=MemDec,family=binomial)

anova(Model.FXFull.Rand0,Model.FXFull.Rand1,Model.FXFull.Rand2,Model.FXFull.Rand3)

Model.FXFull.Rand4<-lmer(ChooseOld~PriorPayc*PSNew+PriorPayc*PSNewEnc+(1|Sub)+(0+PriorPayc|Sub)+(0+PSNewEf:PriorPayc|Sub),data=MemDec,family=binomial)
Model.FXFull.Rand5<-lmer(ChooseOld~PriorPayc*PSNew+PriorPayc*PSNewEnc+(1|Sub)+(0+PriorPayc|Sub)+(0+PSNewEf:PriorPayc|Sub)+(0+PSNewEncEf:PriorPayc|Sub),data=MemDec,family=binomial)
anova(Model.FXFull.Rand0,Model.FXFull.Rand1,Model.FXFull.Rand4,Model.FXFull.Rand5)

# Try a model with the most complex and complete random effects covariance structure
Model.FXFull.RandFull<-lmer(ChooseOld~PriorPayc*PSNewEf+PriorPayc*PSNewEncEf+(PriorPayc*PSNewEf+PriorPayc*PSNewEncEf|Sub),data=MemDec,family=binomial)
anova(Model.FXFull.Rand1,Model.FXFull.RandFull) # random slope for PriorPay seems to be the onlything that matters

# See if added covariance improves Rand1 Model
Model.FXFull.Rand1_cor<-lmer(ChooseOld~PriorPayc*PSNewEf+PriorPayc*PSNewEncEf+(PriorPayc|Sub),data=MemDec,family=binomial)
anova(Model.FXFull.Rand1,Model.FXFull.Rand1_cor)

# Now remove Fixed effects that don't seem to matter 
Model.Final<-lmer(ChooseOld~PriorPayc*PSNewEf+(1|Sub)+(0+PriorPayc|Sub),data=MemDec,family=binomial)

# esticon funciton in doBy package can be used to run contrasts
InteractionComp<-esticon(Model.FXFull.Rand1, c(0,0,0,0,1,-1))

# Models can be run on subsets of data
PrimeModel.FXFull.Rand0<-lmer(ChooseOld~PriorPayc*PSNew+PriorPayc*PSNewEnc+(1|Sub),data=MemDec,family=binomial,subset=(ScenePrime==1))
PrimeModel.FXFull.Rand1<-lmer(ChooseOld~PriorPayc*PSNew+PriorPayc*PSNewEnc+(0+PriorPay|Sub)+(1|Sub),data=MemDec,family=binomial,subset=(ScenePrime==1))
PrimeModel.FXFull.Rand2<-lmer(ChooseOld~PriorPayc*PSNew+PriorPayc*PSNewEnc+(0+PSNew|Sub)+(0+PriorPay|Sub)+(1|Sub),data=MemDec,family=binomial,subset=(ScenePrime==1))
PrimeModel.FXFull.Rand3<-lmer(ChooseOld~PriorPayc*PSNew+PriorPayc*PSNewEnc+(0+PSNewEnc|Sub)+(0+PSNew|Sub)+(0+PriorPay|Sub)+(1|Sub),data=MemDec,family=binomial,subset=(ScenePrime==1))

anova(PrimeModel.FXFull.Rand0,PrimeModel.FXFull.Rand1,PrimeModel.FXFull.Rand2,PrimeModel.FXFull.Rand3)


# Visualizing Results
# can plot interactions

library(languageR)
plotLMER.fnc(PrimeModel.FXFull.Rand0,pred="PriorPayc",control=(list("PSNew",0)), addlines=T,linecolor=2)+title("Preceding Scene")
plotLMER.fnc(PrimeModel.FXFull.Rand0,pred="PriorPayc",control=(list("PSNew",1)), addlines=T,addToExistingPlot=T)

plotLMER.fnc(PrimeModel.FXFull.Rand0,pred="PriorPayc",control=(list("PSNewEnc",0)), addlines=T,linecolor=2)+title("Preceding Encoding Scene")
plotLMER.fnc(PrimeModel.FXFull.Rand0,pred="PriorPayc",control=(list("PSNewEnc",1)), addlines=T,addToExistingPlot=T)

# use stimulus as a random effect
MemDec$ObjBf<-factor(MemDec$ObjB)
PrimeModel.FXFull.RandObj<-lmer(ChooseOld~PriorPayc*PSNew+PriorPayc*PSNewEnc+(0+PriorPay|Sub)+(1|Sub)+(1|ObjBf),data=MemDec,family=binomial,subset=(ScenePrime==1))
anova(PrimeModel.FXFull.Rand1,PrimeModel.FXFull.RandObj)

# Model validation

# see whether residuals are roughly normal
hist(resid(PrimeModel.FXFull.Rand1))
qqnorm(resid(PrimeModel.FXFull.Rand1))

# see whether random efects are roughly normally distributed
plot(density(ranef(PrimeModel.FXFull.Rand1)$Sub[,1]),main=("RanEf Intercept"))
plot(density(ranef(PrimeModel.FXFull.Rand1)$Sub[,2]),main=("RanEf Slope"))
