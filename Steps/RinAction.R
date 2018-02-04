attach(women)
lm1 <- lm(weight~height)
fitted(lm1)
weight
residuals(lm1)
a=fitted(lm1)+residuals(lm1)
plot(  height,weight)
abline(lm1); summary(lm1)
#The Residual Standard error 1.525 lbs can be thought of as average error in predicting weight from height
# F Statics ....
lm2 <- lm(weight ~ height + I(height^2)) #Quadratic regression
plot(height,weight); 
lines(height, fitted(lm2),lty=2)
summary(lm2)
diff(fitted(lm2)); diff(fitted(lm1)) 
#Now
# Weight = 261.8 - 7.34 * height + 0.83 height^2    is a better fit
# weighted sum of predictor also fits under rubrik of linear regression; 
#in general an nth degree polynomial produces n-1 bends
lm3 <- lm(weight ~ height + I(height^2)+I(height^3)) #cubic regression
plot(height,weight); 
lines(height, fitted(lm3))
summary(lm3)
diff(fitted(lm3)); diff(fitted(lm2)) 
##########------------------------------------------------------------------###########
states = as.data.frame(state.x77[,c('Murder','Population','Illiteracy','Income','Frost')])
s2 <- states
lm1 <- lm(Murder ~ . , data=states)
summary(lm1)
###-----------
library(ggcorrplot)
ggcor <- cor(states[,-1])
ggcorrplot(ggcor, hc.order = TRUE, type = "lower", lab = TRUE, lab_size = 3, method="circle",colors = c("tomato2", "white", "springgreen3"), title="Correlogram of Illitercy in US", ggtheme=theme_bw)
###-----------
lm1 <- lm(Murder~. + Income:Frost, data=states)
summary(lm1)
plot(lm1)
outlierTest(lm1) # Bonferroni outlier test
influencePlot(lm1) # Regression Influence
sort(residuals(lm1)) # Nevada has highest residual value 7.6
states <- states[!(row.names(states) %in% c('Nevada','Alaska')),]
lm1 <- lm(Murder~. , data=states)
summary(lm1)
plot(lm1)
library(car); 
scatterplotMatrix(states)
qqPlot(lm1,simulate = T) #when simulate=T , 95% confidence envelope is produced using parametric bootstrap
avPlots(lm1)
influencePlot(lm1)
scatterplot(Murder ~ Illiteracy  ,data=states,spread=F, lty.smooth=2,pch=19)
##INDEPENDENCE OF ERRORS (AUTOCORRELATED) -use durbin watson test
durbinWatsonTest(lm1)
# the non-sig value 0.24 suggests a lack of autocorrelation, and conversely independence of errors
# so the model is good. This test is best for time interval data since closer in time data correlates
lm2 <- lm (Murder ~ . , data=states)
crPlots(lm2) # the model is linear; non-linearity in any one plot suggests including log or polynomial components
ncvTest(lm1)
# the non-sig p = 0.42 suggests that you have met the constant variance assumption
# the above tests  homoscedasticity ; hence heteroscedasticity assumption is satisfied
spreadLevelPlot(lm1)
# the random points around the horizontal line of best fit suggest constant error variance
# if you'd voileted the assumption, you'd expect non-horizontal line
# the power transformation 1.33 stabilizes the non-constant error variance
# if you'd got .5 instead of 1.2, then using sqrt(Y) instead of Y might lead to model that satisfied homoscedasticity

# GOLBAL VALIDATION OF LINEAR MODEL ASSUMPTION 
library(gvlma)
gv <- gvlma(lm1)
summary(gv)
# IF p WERE SIG IN GLOBAL STAT E.G. 0.05 YOU WOULD HAVE TO ASSESS THE DATA USING PREVIOUS METHODS DISCUSSED

# evaluating multicollinearity
vif (lm1) # for this to satisfy sqrt(vif) > 2
sqrt(vif(lm1)) > 2
# (all false): this suggests multicollinearity isn't a problem with our predictors

lm2 <- lm(Murder ~ . , data = s2)
outlierTest(lm2)
# the studentized residual should be between -2 and 2 as right hand rule, but its 3.5 and significant
# outliertest calculates the one largest residual. You must delete it and rerun the test to see others
x11()
avPlots(lm2, ask=F, onepage=T, id.method='identify')
#right click , esc, or click at the plot to change/identify the outliers 

#Another way is ...
influencePlot(lm2, id.method='identify',sub='Circle size is proportional to cooks distance')
#New York, California, Hawaii, and Washington have high leverage; and Nevada, Alaska, and
#Hawaii are influential observations. Leverage --> unsual combinations of predictors > .2 or .3

#TRANSFORMING PREDICTORS TO A BETTER FIT
boxTidwell(weight ~ height, data=women)
#suggests that lambda 4.2 or 4, using I(height^4) betters fits the model

#As you’ve just seen, one approach to dealing with multicollinearity is to fit a different
#type of model (ridge regression in this case). If there are outliers and/or influential
#observations, you could fit a robust regression model rather than an OLS regression.
#If you’ve violated the normality assumption, you can fit a nonparametric regression
#model. If there’s significant nonlinearity, you can try a nonlinear regression model. If
#you’ve violated the assumptions of independence of errors, you can fit a model that
#specifically takes the error structure into account, such as time-series models or multi-
 # level regression models. Finally, you can turn to generalized linear models to fit a wide
#range of models in situations where the assumptions of OLS regression don’t hold.

library(MASS)
stepAIC(lm2, direction = 'backward') # but
stepAIC(lm1, direction = 'backward')

