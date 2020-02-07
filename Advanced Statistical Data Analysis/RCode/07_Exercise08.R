#
## Worksheet Week 7 - Solution
## ***************************
##
## Question 1
## ~~~~~~~~~
##
## 1a turb Data
##    ~~~~~~~~~
turb <- read.table("Data/turbines.dat", header=T)
str(turb)
turb.glm <- glm(cbind(Fissures, Turbines-Fissures) ~ Hours,
				family=binomial, data=turb)
summary(turb.glm)
##     Null deviance: 112.670  on 10  degrees of freedom
## Residual deviance:  10.331  on  9  degrees of freedom

## Because the response is binomially distributed with m > 1,
## we can test on overdispersion:
1-pchisq(10.331, 9)  ## 0.3243594
## Since the p-value > 0.05 we have no evidence against the
## null hypothesis that phi=1 --> no overdispersion

source("../RFn_Plot-glmSim.R") # --> plot.glmSim

par(mfrow=c(2,4))
plot(turb.glm)
plot.glmSim(turb.glm, SEED=111)
## Residual Plot: Since the red smoother is within (depends on the seed) the
##   stochastic fluctuation (gray spaghettis), there is no evidence that the
##   expectation is incorrectly specified.
## Normal Plot: Since the black points is within the stochastic fluctuation
##   (gray points), there is no evidence that there are outliers or some
##   distortions of the distributional assumption.
## Scale-location Plot: Since the red smoother is within the stochastic
##   fluctuation (gray spaghettis), there is no evidence that the
##   variance is incorrectly specified.
## Residuals vs Leverage: Since all points have a Cook's distance smaller
##   than 1 there is nor to influential observation.
##   Accoding to Hubers rule of thumb there are there leverage points which
##   are however not dangerous. However, according to the other rule of thumb,
##   there are no leverage points:
2*2/11 ## = 0.3636364

## There is no evidence that the model does not fit the data adequately.

## To use a non-robust lowess, a bit smoother (span=0.8)
par(mfrow=c(2,4))
plot(turb.glm,
     panel=function(x,y) panel.smooth(x, y, iter=1, span=0.8))
plot.glmSim(turb.glm, SEED=184,
            smoother=function(x,y) lowess(x, y, iter=1, f=0.8))
## Residual Plot:
##   The smoother indicates a banana form of the expectation.
##   Since the red smoother is outside the stochastic fluctuation
##   (gray spaghettis) on the right hand side, there is some evidence that the
##   expectation is incorrectly specified (either wrong link or something wrong
##   with the lineaqr predictor).



## 1b Premature Birth Data
##    ~~~~~~~~~~~~~~~~~~~~

bw <- read.table("Data/birth-weight.dat", header=T)
str(bw)

bw.glm1 <- glm(cbind(Y, m-Y) ~ weight, family=binomial, data=bw)
summary(bw.glm1)
##     Null deviance: 87.046  on 9  degrees of freedom
## Residual deviance: 12.439  on 8  degrees of freedom

## Because the response is binomially distributed with m > 1,
## we can test on overdispersion:
1-pchisq(12.439, 8)  ## 0.1326652
## Since the p-value > 0.05 we have no evidence against the
## null hypothesis that phi=1 --> no overdispersion

par(mfrow=c(2,4))
plot(bw.glm1,
     panel=function(x,y) panel.smooth(x, y, iter=1, span=0.8))
plot.glmSim(bw.glm1, SEED=184,
            smoother=function(x,y) lowess(x, y, iter=1, f=0.8))
## Residual plot: The smoother indicates a banana form of the expectation.
##   The curvature is too strong to be ignored, since the red smoother is
##   outside the stochastic fluctuation.
## (Otherwise there is no evidence that the assumptions are violated because
##  the "structures" are within the stochastic fluctuation.)

## Hence:
bw$lWeight <- log(bw$weight)
bw.glm2 <- glm(cbind(Y, m-Y) ~ lWeight, family=binomial, data=bw)
summary(bw.glm2)
##     Null deviance: 87.0456  on 9  degrees of freedom
## Residual deviance:  8.7335  on 8  degrees of freedom

## The residual deviance is smaller and thus there will be
## no evidence against the null hypothesis that phi=1
## --> no overdispersion

par(mfrow=c(2,4))
plot(bw.glm2,
     panel=function(x,y) panel.smooth(x, y, iter=1, span=0.8))
plot.glmSim(bw.glm2, SEED=184,
            smoother=function(x,y) lowess(x, y, iter=1, f=0.8))
## The Residual Plot still show the same deficiencies. However, since the red
## smoother is within the stochastic fluctuation (gray spaghettis), there is
## no evidence that the expectation is is incorrectly specified.

## But still, let's try
library(gam)
bw.gam2 <- gam(cbind(Y, m-Y) ~ lo(lWeight, span=0.6), family=binomial,
               data=bw)
par(mfrow=c(1,1))
plot(bw.gam2, residuals=TRUE, se=T)
## The gam fit does not indicate a necessary transformation. So there
## might be variables which are important for modelling the survival of
## babies, but are not included.

## Let's also look at the extended dataset baby (Exercise 1 of Worksheet Week 6)
baby <- read.table("Data/baby.dat", header=T)
str(baby)

## In the lecture (week 7) we applied the identified the following
## transformations:
baby$tWeight <- ifelse(baby$Weight < 1000, 0, baby$Weight-1000)
baby$tpH <- ifelse(baby$pH < 7.27, 0, baby$pH-7.27)
baby.glm1 <- glm(Survival ~ Weight + tWeight + Age + Apgar1
                 + pH + tpH, family=binomial, baby)

par(mfrow=c(2,4))
plot(baby.glm1,
     panel=function(x,y) panel.smooth(x, y, iter=1, span=0.9))
plot.glmSim(baby.glm1, SEED=184,
            smoother=function(x,y) lowess(x, y, iter=1, f=0.9))
## See slides of Week 7



## 1c Dial-a-ride Data
##    ~~~~~~~~~~~~~~~~

DaR <- read.table("Data/Dial-a-ride.dat", header=T)
str(DaR)

DaR$lPOP <- log(DaR$POP)
DaR$lAR <- log(DaR$AR)
DaR$lHR <- log(DaR$HR)
DaR$lVH <- log(DaR$VH)
DaR$lF <- log(DaR$F)

DaR.glm1 <- glm(RDR ~ lPOP + lAR + lHR + lVH + lF + IND,
                family=poisson, data=DaR)
summary(DaR.glm1)
##     Null deviance: 20655.3  on 53  degrees of freedom
## Residual deviance:  3436.1  on 47  degrees of freedom

## The residual deviance is so much times larger than the df, that there is very
## clear evidence that there is overdispersion.

par(mfrow=c(2,4))
plot(DaR.glm1,
     panel=function(x,y) panel.smooth(x, y, iter=1, span=0.6))
plot.glmSim(DaR.glm1, SEED=184,
            smoother=function(x,y) lowess(x, y, iter=1, f=0.6))
## There very, very much too influential points because about 9 observations
## have a realy too large Cook's distance. They are caused by leverage and too
## large residuals.
## Because of the too large overdispersion all "structures" are outsice the
## stochastic fluctuation. Hence there is clear evidence that the model does
## not describe the data adequately.

## Removing the two clear outliers and observation 1:
DaR.glm2 <- glm(RDR ~ lPOP + lAR + lHR + lVH + lF + IND,
                family=poisson, data=DaR[-c(45,53,1),])
summary(DaR.glm2)
##     Null deviance: 4573.04  on 50  degrees of freedom
## Residual deviance:  487.69  on 44  degrees of freedom

## The residual deviance looks much better, but indicates still overdispersion.

par(mfrow=c(2,4))
plot(DaR.glm2,
     panel=function(x,y) panel.smooth(x, y, iter=1, span=0.6))
plot.glmSim(DaR.glm2, SEED=184,
            smoother=function(x,y) lowess(x, y, iter=1, f=0.6))


## There is no evidence that the expectation or the varince are incorrectly
## specified or that there are distributional violations.
## But there are still 5 observations with a too large Cook's distance

DaR.gam2 <- gam(RDR ~ lo(lPOP) + lo(lAR) + lo(lHR) + lo(lVH) + lo(lF) + IND,
                family=poisson, data=DaR[-c(45,53,1),], bf.maxit=500)
par(mfrow=c(2,3))
plot(DaR.gam2, residuals=TRUE, se=T)
## To obtain better insights:
plot(DaR.gam2, se=T)
## There are further hints to transform the variables ....


## 1d Transaction Data
##    ~~~~~~~~~~~~~~~~
transa <- read.table("Data/transactions.dat", header=T)
str(transa)
transa.glmI  <- glm(Time ~ Type1 + Type2, family=Gamma(link=identity),
		data=transa)
par(mfrow=c(2,4))
plot(transa.glmI,
     panel=function(x,y) panel.smooth(x, y, iter=1, span=0.6))
plot.glmSim(transa.glmI, SEED=184,
            smoother=function(x,y) lowess(x, y, iter=1, f=0.6))

## Residual plot: The smoother is almost a horizontal straight line. Because it
##   is within the stochastic fluctuation, there there is no  evidence that the
##   expectation is incorrectly specified.
## Location-scale plot: The smoother shows a slightly decreasing trend. Since
##   it is outside the stochastic fluctuation, there is evidence that the
##   variance is incorrectly specified.
## Normal Q-Q plot: The points scatter quite nicely around a straight line and
##   within the stochstic flucuation expect at the right end. Hence, there is
##   a light evidence that there is something wrong with the distributional
##   assumption.
## Residual against leverage: Since all Cook's distances are smaller than 1
##   there is no observation with a too large influence.
2*3/nrow(transa) ## = 0.02298851
##   --> there are at least 5 leverage points but they do not harm

## Conclusion: Model might be not fully adequate.




## 1e Nambeware Polishing Times
##    ~~~~~~~~~~~~~~~~~~~~~~~~~
nw <- read.table("Data/nambeware.txt", header=T)

nw.glm1 <- glm(Time ~ Diam + Type, family=Gamma(link=log), data=nw)
summary(nw.glm1)
par(mfrow=c(2,4))
plot(nw.glm1,
     panel=function(x,y) panel.smooth(x, y, iter=1, span=0.6))
plot.glmSim(nw.glm1, SEED=184,
            smoother=function(x,y) lowess(x, y, iter=1, f=0.6))
## Residual Plot: Since the red smoother is within the stochastic fluctuation
##   (gray spaghettis), there is no evidence that the expectation is
##    incorrectly specified.
## Normal Plot: Since the black points is within the stochastic fluctuation
##   (gray points), there is no evidence that there are outliers.
## Scale-location Plot: Since the red smoother is within the stochastic
##   fluctuation, there is no evidence that the variance is incorrectly
##   specified.
## Residual against leverage: Since all Cook's distances are smaller than 1
##   there is no observation with a too large influence.
2*6/nrow(nw) ## = 0.2033898  -
##   -> there are 2 leverage points but they do not harm

## Conclusion: Model might be adequate.


## ======================================================================
##
## Question 2
## ~~~~~~~~~~
Mine <- read.table("Data/mine.dat", header=T)
str(Mine)
summary(Mine)

## (a)
Mine.glm <- glm(Y ~ INB + EXTRP + oTime, family=poisson, data=Mine)
summary(Mine.glm)
## Coefficients:
##               Estimate Std. Error z value Pr(>|z|)
## (Intercept) -3.7206821  0.9788770  -3.801 0.000144 ***
## INB         -0.0014793  0.0008244  -1.794 0.072757 .
## EXTRP        0.0627011  0.0122711   5.110 3.23e-07 ***
## oTime       -0.0316514  0.0163095  -1.941 0.052298 .
## ---
## (Dispersion parameter for poisson family taken to be 1)
##     Null deviance: 74.984  on 43  degrees of freedom
## Residual deviance: 38.031  on 40  degrees of freedom

## The response  Y_i is ~ Pois(lambda_i), independent with E(Y_i)=mu_i
## The explanatory variables are IND, EXTRP and oTimes.
## A linear combination of them yield the linear predictor eta_i
## The canonical link, log(), is used: log(mu_i) = eta_i


## (b)
## Since the residual deviance is even smaller than the expectation of
## its corresponding chi-squared distribution with 40df we have no evidence
## that there is overdispersion.


## (c)
## 95% Wald confidence interval:
h <- summary(Mine.glm)$coefficients
round(cbind(h[,1] - 1.96*h[,2], h[,1] + 1.96*h[,2]), 6)
##                  [,1]      [,2]
## (Intercept) -5.639281 -1.802083
## INB         -0.003095  0.000137
## EXTRP        0.038650  0.086752
## oTime       -0.063618  0.000315

## 95%profile confidence interval:
confint(Mine.glm)
## Waiting for profiling to be done...
##                    2.5 %        97.5 %
## (Intercept) -5.740009869 -1.891366e+00
## INB         -0.003204226  4.720791e-05
## EXTRP        0.039618906  8.785501e-02
## oTime       -0.064924357 -8.453101e-04

## There is little differences between these two approaches of estimating
## the 95% confidence intervals


## (d)
par(mfrow=c(2,4))
plot(Mine.glm,
     panel=function(x,y) panel.smooth(x, y, iter=1, span=0.6))
plot.glmSim(Mine.glm, SEED=18417,
            smoother=function(x,y) lowess(x, y, iter=1, f=0.8))
## Residual plot: Since the red smoother is outside the stochastic fluctuation
##   there is clear evidence that the expectation is incorrectly specified.
## Location-scale plot: The smoother is slightly decreasing on the r.h.s. Since
##   the red smoother is within the stochastic fluctuation there is no evidence
##   that the variance is incorrectly specified.
## Normal Q-Q plot: The points scatter more or less around a straight line and
##   are within the stochastic fluctuation. Hence there is no evidence for
##   outliers or some distortions of the distributional assumption.
## Residual against leverage: Since all Cook's distances are smaller than 1
##   there is no observation with a too large influence.
2*4/nrow(Mine) ## = 0.1818182
##     --> there are 5 leverage points but they do not harm.

## Conclusion: Try to improve the model and get a better fit in the expectation


## (e)
Mine.gam <- gam(Y ~ lo(INB) + lo(EXTRP, span=0.7) + lo(oTime), family=poisson,
                data=Mine, bf.maxit=500)
par(mfrow=c(2,2))
plot(Mine.gam, se=TRUE, residuals=TRUE)
## The plots do not strongly indicate the needs of any transformations.
## But the logarithm of INB will improve the plots:
Mine$lINB <- log(Mine$INB)
Mine.gam2 <- gam(Y ~ lo(lINB) + lo(EXTRP,span=0.7) + lo(oTime), family=poisson,
                data=Mine, bf.maxit=500)
par(mfrow=c(2,2))
plot(Mine.gam2, se=T, residuals=T)
## We might transform the variable EXTRP as well. However, I could not  find a
## simple transformation which gives a better plot.


## Are we closer to an adequate model?
Mine.glm2 <- glm(Y ~ lINB + EXTRP + oTime, family=poisson, data=Mine)
summary(Mine.glm2)

par(mfrow=c(2,4))
plot(Mine.glm2,
     panel=function(x,y) panel.smooth(x, y, iter=1, span=0.6))
plot.glmSim(Mine.glm2, SEED=17,
            smoother=function(x,y) lowess(x, y, iter=1, f=0.8))
## It's better! Depending on the seed we have no or some weak evidence for
## violations of the assumptions.

##
## ======================================================================
##
## Question 3
## ~~~~~~~~~~
eU <- read.table("Data/eUsage.dat", header=T)
str(eU)


## (a)
eU.lm <- lm(Y ~ x,  data=eU)
coef(eU.lm)
##  (Intercept)            x
## -0.831303660  0.003682843

source("RFn/RFn_Plot-lmSim.R")
par(mfrow=c(2,4))
plot(eU.lm)
plot.lmSim(eU.lm, SEED=1798)
## Residual plot: The smoother indicates some problems with the specification
##   of the expectation at the r.h.s., because it is outside the stochastic
##   fluctuation. But it is caused by one observation. This is (too) little
##   evidence that the expectation is incorrectly specified
## Location-scale plot: The smoother is steeply increasing on the l.h.s. and
##   later more moderately. On the r.h.s., it is outside the  stochastic
##   fluctuation. So there is no evidence that the variance is incorrectly
##   specified.
## Normal Q-Q plot: The points scatter within the stochastic fluctuation, hence
##    we have no evidence for outliers or some distortions of the
##    distributional assumption.
## Residual against leverage: Since all Cook's distances are smaller than 1
##   there is no observation with a too large influence.
2*2/nrow(Mine) ## = 0.09090909
##                --> there is one clear leverage points but it does not harm

## Conclusion: The model does not fit the data adequately.
## Possible remedy: Either log-transform the response or use a gamma model.


## (b)
eU.glm <- glm(Y ~ x, family=Gamma(link=identity), data=eU)
coef(eU.glm)
##  (Intercept)            x
## -0.767512719  0.003661987

## The coefficient estimates are very similar to those in part (a)


par(mfrow=c(2,4))
plot(eU.glm,
     panel=function(x,y) panel.smooth(x, y, iter=1, span=0.6))
plot.glmSim(eU.glm, SEED=184,
            smoother=function(x,y) lowess(x, y, iter=1, f=0.6))
## Residual plot: The smoother is almost a horizontal straight line and the red
##   smoother is within the stochastic fluctuation. Hence there is no evidence
##   that the expectation is incorrectly specified.
## Location-scale plot: The smoother is decreasing on the r.h.s.  Since the red
##   smoother is outside the stochastic fluctuation, there is some evidence that
##   the variance is incorrectly specified.
## Normal Q-Q plot: The points scatter nicely around a straight line and are
##   within the stochastic fluctuation. Hence there is no evidence for outliers
##   or some distortions of the distributional assumption.
## Residual against leverage: Since all Cook's distances are smaller than 1
##   there is no observation with a too large influence.
2*2/nrow(Mine) ## = 0.09090909
##                --> there are three clear leverage points but they do not harm


## (c)
library(gam)
eU.gam <- gam(Y ~ lo(x), family=Gamma(link=identity), data=eU)
par(mfrow=c(1,1))
plot(eU.gam, se=TRUE, residuals = TRUE)
## The estimated curve can be approximated well by a straight line. Thus there
## is no need for transforming the explanatory variable x.



## (d)
## Wald (from the summary output)
summary(eU.glm)
##               Estimate Std. Error t value Pr(>|t|)
## (Intercept) -0.7675127  0.1894508  -4.051 0.000174 ***
## x            0.0036620  0.0003701   9.895 1.85e-13 ***
## (Dispersion parameter for Gamma family taken to be 0.2810353)

h <- summary(eU.glm)$coefficients
c(h[2,1] - qnorm(0.975)*h[2,2], h[2,1] + qnorm(0.975)*h[2,2])
## [1] 0.002936609 0.004387364

## or with t distribution ("since the dispersion parameter is estimated"):
c(h[2,1] - qt(0.975, df=51)*h[2,2], h[2,1] + qt(0.975, df=51)*h[2,2])
## [1] 0.002918985 0.004404988

## the difference are very small, since qt(0.975, df=51) ~=  qnorm(0.975):
qt(0.975, df=51) - qnorm(0.975) ## = 0.04761979


## with deviances (i.e., profiling):
confint(eU.glm)
## Warning message: glm.fit: algorithm did not converge

## The algorithm needs more iterations within profiling; hence
eU.glm <- glm(Y ~ x, family=Gamma(link=identity), data=eU, maxit=100)
confint(eU.glm)
## Waiting for profiling to be done...
##                    2.5 %       97.5 %
## (Intercept) -1.075321127 -0.349845578
## x            0.002984377  0.004404863

## the 95% confidence interval is  [0.0030 0.0044].

## The differences between these three different types of confidence intervals
## is neglecting.


##
## (e)
## If the response is exponentially distributed then the dispersion parameter
## is fixed at 1: So we can test the null hypothesis phi=1 as in the Poisson
## and the binomial case

## From the summary output we have
## Residual deviance: 18.05  on 51  degrees of freedom

## Since the residual deviance is outside of the acceptance region
qchisq(c(0.025,0.975), 51) ## = 33.16179 72.61599
## the null hypothesis must be rejected on the 5% level. Hence the response
## cannot be exponentially distributed, it must be a gamma distribution.


#
##=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=* END *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=

