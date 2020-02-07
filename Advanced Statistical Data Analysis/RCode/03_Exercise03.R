#
## Worksheet Week 3 - Solution
## ***************************
##
## Question 1
## ~~~~~~~~~
SyD <- read.table("Data/Synthetic.dat", header=T)
str(SyD)
## 'data.frame':	83 obs. of  3 variables:
##  $ Y : num  33.5 27.3 22.6 13.4 20.7 ...
##  $ x1: num  19.2 17.6 18.6 22.1 18.1 ...
##  $ x2: num  -3.42 -4.52 -6.82 -12.33 -7.09 ...


## Question 1 (a)
## Robust fit
library("robustbase")
SyD.rlm <- lmrob(Y ~ x1 + x2, data=SyD)
summary(SyD.rlm)
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)  6.52018    2.07332   3.145  0.00233 **
## x1           1.90838    0.11012  17.330  < 2e-16 ***
## x2           2.95177    0.04072  72.495  < 2e-16 ***
## Robust residual standard error: 1.111
##
## Robustness weights:
##  8 observations c(7,17,27,37,47,57,67,77)
## 	 are outliers with |weight| = 0 ( < 0.0012);

## The last three lines tell us that there are 8 observations identified as
## outliers.

## Estimated standard deviation of the error, sigma^:   1.111
## Estimated values of the coefficients:
coef(SyD.rlm)
## (Intercept)          x1          x2
##    6.520180    1.908378    2.951771



## Residuenananlyse
par(mfrow=c(2,3))
plot(SyD.rlm)
## The graphic top left replaces the classical graphic "Residuals against
## leverage". Robust distances measures the outlyingness of observations in
## the x-space. It replaces the classical measure of leverage, H_ii,  and is
## not distorted by outliers. The two dotted horizontal lines is the band
## 0 +/- 2.5 sigma^. Most residuals should be within this band. All residuals
## right of the dotted vertical line are leverage points; i.e. they are too
## far from the bulk of the data.
##
## In all of the five graphics, 8 distinct outliers are visible. Hence the
## residuals are not Gaussian distributed.
## The is a slight decreasing trend visible in the last graphic. Hence, it
## might be that the variance is not constant. But the hint is weak.
## There is no evidence that the expectation is not constant.
## Conclusion: There are 8 distinct outliers. Inferential results must be
## based on robust estimation. Least squares estimation will not deliver
## reliable results.

##
## Question 1 (b)
## Classical fit
SyD.lm <- lm(Y ~ x1 + x2, data=SyD)
summary(SyD.lm)
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)  72.9020     9.6482   7.556 5.96e-11 ***
## x1           -2.0837     0.4882  -4.268 5.37e-05 ***
## x2            1.4258     0.1828   7.802 1.98e-11 ***
##
## Residual standard error: 5.799 on 80 degrees of freedom
## Multiple R-squared: 0.4963,     Adjusted R-squared: 0.4837
## F-statistic: 39.41 on 2 and 80 DF,  p-value: 1.226e-12

## Estimated standard deviation of the error, sigma^:   5.799
## Estimated values of the coefficients:
coef(SyD.lm)
## (Intercept)          x1          x2
##   72.902036   -2.083705    1.425830


source("RFn/RFn_Plot-lmSim.R")
par(mfrow=c(2,4))
plot(SyD.lm)
plot.lmSim(SyD.lm)
## There is NO evidence at all that any of the assumption (expectation
## equal 0, constant variance, Gaussian distributed errors) is violated because
## all patterns are within the stochastic fluctuations.  There are also no
## leverage points and there are no too influential observations since Cook's
## distance is always smaller than 1.


## (c)
## The two fits provide significantly different estimates (even with sign
## change!) and standard errors.
##
## The residual analysis of a robust fit ("MM") reveals 8 distinct outliers.
## Using a least squares fit there was not the slightest indication of
## problems in the data!

## (If you drop these eight outliers and refit the data by least square,  the
##  results will hardly differ and correspond to the results of the robust fit
##  with all data.)


## Optional: Fit the model to the data without the 8 outliers using least
## squares
SyD.lm2 <- lm(Y ~ x1 + x2, data=SyD, subset=-c(7,17,27,37,47,57,67,77))
summary(SyD.lm2)
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)   6.5116     2.0129   3.235  0.00184 **
## x1            1.9089     0.1095  17.440  < 2e-16 ***
## x2            2.9543     0.0415  71.197  < 2e-16 ***
## ---
## Residual standard error: 0.953 on 72 degrees of freedom

library(car)
library(rgl)
scatter3d(SyD$x1, SyD$Y, SyD$x2, revolution=1, speed=0.1,
          point.col=1, surface.col="white", surface=FALSE)
scatter3d(SyD$x1, SyD$Y, SyD$x2, revolution=1, speed=0.1,
          point.col=1, surface.col="white", surface=TRUE) # mit Fit



## ======================================================================
##
## Question 2
## ~~~~~~~~~~
EDS <- read.table("Data/ExpressDS.dat", header=T)
str(EDS)
## 'data.frame':	20 obs. of  3 variables:
##  $ weight  : num  5.9 3.2 4.4 6.6 0.75 0.7 6.5 4.5 0.6 7.5 ...
##  $ distance: int  47 145 202 160 280 80 240 53 100 190 ...
##  $ cost    : num  2.6 3.9 8 9.2 4.4 1.5 14.5 1.9 1 14 ...


## Question 1 (a)
EDS$lWeight <- log(EDS$weight)
EDS$lDist <- log(EDS$distance)
EDS$lCost <- log(EDS$cost)

library(gam)
EDS.gam <- gam:::gam(lCost ~ lo(lWeight) + lo(lDist), data=EDS)

par(mfrow=c(1,2))
plot.Gam(EDS.gam, se=TRUE)
## According to our rule of thumb that if a straight line fits between the
## confidence band, no transformation is needed for lDist, but an additional
## one for lWeight.

EDS.gam2 <- gam:::gam(lCost ~ lo(weight) + lo(lDist), data=EDS)


par(mfrow=c(1,2))
plot.Gam(EDS.gam2, se=TRUE)
##  that fits adequately


## Question 1 (b)
EDS.lm1 <- lm(lCost ~ weight  + lDist, data=EDS)

## source("RFn/RFn_Plot-lmSim.R")
par(mfrow=c(2,4))
plot(EDS.lm1)
plot.lmSim(EDS.lm1, SEED=4711)
## In the Tukey-Anscombe plot the smoother shows a light trend downwards which
## is, however, within the stochastic fluctuation. Hence, there is no evidence
## that the expectation is not constant.
## The smoother in the scale-location plot shows a decreasing trend on the
## l.h.s. and is just outside the stochastic fluctuation there, so there
## is some (light) evidence that the variance is not constant.
## The points of the normal q-q plot show one clear outlier which is also
## outside the stochastic fluctuation, so there is also some evidence that
## the error is not Gaussian distributed.
## Since the Cook's distance of all observations is smaller than 1, there are
## NO too influential observations, even observation 9, the outlier, is not
## too influential.

## Question 1 (c)
## One of the findings in part (b) is that there is an outliers, so let's
## apply a robust estimator:

library(robustbase)
EDS.rlm1 <- lmrob(lCost ~ weight + lDist, data=EDS)

## source("RFn/RFn_Plot-lmSim.R")
par(mfrow=c(2,4))
plot(EDS.rlm1)
## Observation 9 is a very distinct outlier and it might have caused all the
## difficulties.

## Repeat the least-squares analysis without observation 9:
EDS.lm2 <- lm(lCost ~ weight + lDist, data=EDS[-9,])

## source("RFn/RFn_Plot-lmSim.R")
par(mfrow=c(2,4))
plot(EDS.lm2)
plot.lmSim(EDS.lm2)
## All patterns are within the stochastic fluctuations, so there is no evidence
## anymore that one of the assumptions is violated. Since Cook's distances of
## all observations are smaller than 1, there is no too influential observation.


## ---------------------------------------------------------
## With gam from package mgcv:
EDS <- read.table("Data/ExpressDS.dat", header=T)

## Question 1 (a)
EDS$lWeight <- log(EDS$weight)
EDS$lDist <- log(EDS$distance)
EDS$lCost <- log(EDS$cost)

library(mgcv)
EDS.gam1 <- gam(lCost ~ s(lWeight) + s(lDist), data=EDS)
EDS.gam2 <- gam(lCost ~ s(weight) + s(distance), data=EDS)

par(mfrow=c(1,2))
plot(EDS.gam1, se=TRUE)
## According to our rule of thumb that if a straight line fits between the
## confidence band, both variables need additional transformations, but they are not straightforward to find!

plot(EDS.gam2, se=TRUE)
## According to our rule of thumb that if a straight line fits between the
## confidence band, no transformation is needed for weight but maybe one
## for distance. Probably, a log-transformation is too strong.

EDS.gam3 <- gam(lCost ~ s(weight) + s(lDist), data=EDS)
plot(EDS.gam3, se=TRUE)
## Indeed, a log-transformation of distance is too strong.

EDS$sDist <- sqrt(EDS$distance)
EDS.gam4 <- gam(lCost ~ s(weight) + s(sDist), data=EDS)
plot(EDS.gam4, se=TRUE)
## That works well!


EDS.lmA1 <- lm(lCost ~ weight  + sDist, data=EDS)
## source("RFn/RFn_Plot-lmSim.R")
par(mfrow=c(2,4))
plot(EDS.lmA1)
plot.lmSim(EDS.lmA1, SEED=4711)
## In the Tukey-Anscombe plot the smoother shows a light banan form which
## is, however, within the stochastic fluctuation. Hence, there is no evidence
## that the expectation is not constant.
## The smoother in the scale-location plot shows a distinct decreasing trend
## which is outside the stochastic fluctuation, so there
## is  evidence that the variance is not constant.
## The points of the normal q-q plot show no ion, so there is no evidence that
## the error is not Gaussian distributed.
## Since the Cook's distance of all observations is smaller than 1, there are
## NO too influential observations, even observation 9, the outlier, is not
## too influential.



#
##=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=* ENDE *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=

