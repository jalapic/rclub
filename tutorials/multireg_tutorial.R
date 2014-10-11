# Multiple Regression 

# import "happydata"
attach(happydata)

# take a look at pair-wise combinations
plot(happydata)

# linear model 
happymodel=lm(happy~apt+tv+drink)

# coefficients & summary
happymodel
summary(happymodel)

# interactions 
happyinteractions=lm(happy~tv*drink)
summary(happyinteractions)

# compare full to a reduced model
reducedhappy=lm(happy~apt+drink)
anova(reducedhappy,happymodel)
summary(reducedhappy)

# confidence interval
predict(happymodel,data.frame(apt=2000,tv=250,drink=4000),interval="confidence")

# prediction interval
predict(happymodel,data.frame(apt=2000,tv=250,drink=4000),interval="prediction")

# automatic methods - library leaps 
library(leaps)
happyleaps=regsubsets(happy~apt+tv+drink,data=happydata,nbest=6)

# adjusted R-squared
plot(happyleaps,scale="adjr2")
# BIC
plot(happyleaps,scale="bic")

# forward selection (AIC)
null=lm(happy~1,data=happydata)
full=lm(happy~.,data=happydata)
step(null,scope=list(lower=null,upper=full),direction="forward")
# backward selection 
step(full,data=happydata,direction="backward")
# stepwise regression 
step(null,scope=list(upper=full),data=happydata,direction="both")

# Model Diagnostics 
plot(happymodel)

hist(happymodel$res)
plot(drink,happymodel$res)

# compute leverage
lev=hat(model.matrix(happymodel))
plot(lev)
happydata[lev>0.6,]

# Cook's distance
happycook=cooks.distance(happymodel)
plot(happycook)

# multicollinearity
library(HH)
vif(happymodel)