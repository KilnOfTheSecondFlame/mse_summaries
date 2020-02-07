##
## Worksheet Week 2 - Solution
## ***************************
##
## Question 1
## ~~~~~~~~~
sniffer <- read.table("Data/sniffer.dat", header=T)
summary(sniffer)

## For a first impression:
pairs(sniffer, las=1)
## The explanatory variables are highly correlated with each other and with
## the response. Technically, Tukey's first-aid transformations will have a
## very small effect since the two pressure variables have a small spread:
## max/min ~= 3


## Question 1 (a)
## Fit
sn.fit <- lm(Y ~ . , data=sniffer)

## (a) (i)
coef(sn.fit)
## (Intercept)       Temp.Tank        Temp.Gas      Vapor.Tank Vapor.Dispensed
##  1.01501756     -0.02860886      0.21581693     -4.32005167      8.97488928

## Note that there is an additional parameter which must be estimated: sigma

## (a) (ii)
summary(sn.fit)
## Coefficients:
##                 Estimate Std. Error t value Pr(>|t|)
## (Intercept)      1.01502    1.86131   0.545  0.59001
## Temp.Tank       -0.02861    0.09060  -0.316  0.75461
## Temp.Gas         0.21582    0.06772   3.187  0.00362 **
## Vapor.Tank      -4.32005    2.85097  -1.515  0.14132
## Vapor.Dispensed  8.97489    2.77263   3.237  0.00319 **
##
## Residual standard error: 2.73 on 27 degrees of freedom
## Multiple R-squared: 0.9261,     Adjusted R-squared: 0.9151
## F-statistic: 84.54 on 4 and 27 DF,  p-value: 7.249e-15

## The P-value of the F-statistic in the model sn.fit indicates that the model
## can not only consist of the intercept (since the null hypothesis that all
## coefficients except this of the intercept are 0 is rejected due to a
## P-value ~ = 0 << 0.05). However, according to the marginal t-tests
## (individual lines under coefficients) not all variables are important for
## the explanation of the response.


## (a) (iii)
## VIF
library(car)
vif(sn.fit)
##      Temp.Tank        Temp.Gas      Vapor.Tank Vapor.Dispensed
##      12.997379        4.720998       71.301491       61.932647

## All variables but Temp.Gas have a VIF greater than 10. So, according to the
## rule of thumb, there are problems with multicollinearities. The Vapor.Tank
## variable is most affected according to the VIF values.


## (b)
step(sn.fit)
## Stepwise model selection
## Initial Model:
## Y ~ Temp.Tank + Temp.Gas + Vapor.Tank + Vapor.Dispensed
##
## Final Model:
## Y ~ Temp.Gas + Vapor.Tank + Vapor.Dispensed

## Just one variable has been dropped!


## (c)
sn.fit2 <- lm(Y ~ Temp.Gas + Vapor.Tank + Vapor.Dispensed, data=sniffer)

vif(sn.fit2)
##       Temp.Gas      Vapor.Tank Vapor.Dispensed
##       4.255787       42.899447       55.907555

## The problem with multicollinearity persists.


## What could we do better? - Again, look at
pairs(sniffer[,-5])
## The 4 explanatory variables are pairwise correlated; especially strong
## 'Vapor.Tank' and 'Vapor.Dispensed'

##
## Build a new dataset:
sn <- data.frame(MeanVapor=0.5*(sniffer$Vapor.Tank + sniffer$Vapor.Dispensed),
                 DeltaVapor=sniffer$Vapor.Tank - sniffer$Vapor.Dispensed)
## Since the two temperature variables are less correlated and, as shown above,
## only one of them may be needed to describe the response, we do not change
## these two variables.
## This trick only works when the highly correlated variables have the same unit
sn <- cbind(sn, sniffer[, c('Temp.Tank', 'Temp.Gas', 'Y')])

## Fit
snM.fit <- lm(Y ~ . , data=sn)
step(snM.fit)
## As expected: Y ~ MeanVapor + DeltaVapor + Temp.Gas

snM.fit1 <- lm(Y ~  MeanVapor + DeltaVapor + Temp.Gas, data=sn)
vif(snM.fit1)
## MeanVapor DeltaVapor   Temp.Gas
##  4.450470   1.538981   4.255787

## Everything is fine now.

## Note that the two models
##    Y ~ Temp.Gas + Vapor.Tank + Vapor.Dispensed
## and
##    Y ~  MeanVapor + DeltaVapor + Temp.Gas
## yield the same fitted values:
sum(abs(fitted(snM.fit1)- fitted(sn.fit2))) ## 1.314504e-13


## Can you interpret the coefficients?
summary(snM.fit1)
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)  1.06553    1.82437   0.584  0.56386
## MeanVapor    4.35974    0.71833   6.069 1.52e-06 ***
## DeltaVapor  -7.06810    2.36554  -2.988  0.00579 **
## Temp.Gas     0.20910    0.06325   3.306  0.00260 **
## ---
## Residual standard error: 2.686 on 28 degrees of freedom
## Multiple R-squared:  0.9258,	Adjusted R-squared:  0.9178
## F-statistic: 116.4 on 3 and 28 DF,  p-value: 6.427e-16


##
source("RFn/RFn_Plot-lmSim.R")
par(mfrow=c(2,4))
plot(snM.fit1)
plot.lmSim(snM.fit1)
## There is no evidence that one of the assumption is violated because ...


## ======================================================================
##
## Question 2
## ~~~~~~~~~~
Jet <- read.table("Data/Jet.dat", header=T)
pairs(Jet)
summary(Jet)

## Step 1: Tukey's first-aid transformations (forgot to ask for that):
## Because the first four explanatory variables (x1 to x4) and the response
## are amounts we log-transform them:
Jet$lx1 <- log(Jet$x1)
Jet$lx2 <- log(Jet$x2)
Jet$lx3 <- log(Jet$x3)
Jet$lx4 <- log(Jet$x4)
Jet$lY <- log(Jet$Y)
## x5 and x6 are not transformed because temperature can be negative.

## But:
## Since the respective value range from the minimum to the maximum never
## exceeds the factor 2, the log-transformation will not bring any visible
## effects (it is well approximated by a straight line in this range).
## However, the transformation of the response will change the error structure
## in the model.


## Step 2: Fit of the full (main effect) model:
##
Jet.lm1 <- lm(lY ~ lx1 + lx2 + lx3 + lx4 + x5 + x6, data=Jet)
summary(Jet.lm1)
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)
## (Intercept) -9.704e+00  8.614e+00  -1.127   0.2681
## lx1          4.090e-01  1.766e-01   2.316   0.0269 *
## lx2         -6.751e-02  2.043e-01  -0.330   0.7431
## lx3          1.364e+00  9.452e-01   1.443   0.1584
## lx4          2.897e-01  1.389e-01   2.086   0.0448 *
## x5           2.494e-04  9.415e-05   2.648   0.0123 *
## x6          -3.925e-03  7.019e-04  -5.592 3.21e-06 ***
## ---
## Residual standard error: 0.007329 on 33 degrees of freedom
## Multiple R-squared:  0.9974,	Adjusted R-squared:  0.997
## F-statistic:  2143 on 6 and 33 DF,  p-value: < 2.2e-16

source("RFn/RFn_Plot-lmSim.R")
par(mfrow=c(2,4))
plot(Jet.lm1)
plot.lmSim(Jet.lm1, SEED=1809)
## Observation 20 is an outlier and it is too influential (Cook's distance is
## lager than 1). The smoother in the Tukey-Anscombe plot shows a banana form
## which is not within the stochastic fluctuation.


Jet.lm1a <- lm(lY ~ lx1 + lx2 + lx3 + lx4 + x5 + x6, data=Jet[-20,])
par(mfrow=c(2,4))
plot(Jet.lm1a)
plot.lmSim(Jet.lm1a, SEED=1809)
## As identified above observation 20 is too influential so we will drop it for
## the variable selcetion (a check later again)


## Step 3:
## Variable selection without observation 20:
## [You will obtain a different selected model when observation 20 is included.
## Hence, observation 20 is also too influential in the variable selection
## process!]
step(Jet.lm1a, scope=list(upper=~ lx1 + lx2 + lx3 + lx4 + x5 + x6, lower=~1))
## ly ~ lx1 + lx2 + x6

step(lm(lY ~ 1, data=Jet[-20,]), direction="both",
     scope=list(upper=~ lx1 + lx2 + lx3 + lx4 + x5 + x6, lower=~1))
## ly ~ lx1 + x6 + lx2 ; that is the same solution as before
## Watch the path of the added and dropped variables.


Jet.lm2 <- lm(lY ~ lx1 + lx2 + x6, data=Jet, subset=-20)
summary(Jet.lm2)
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)
## (Intercept) -6.5248156  1.4320310  -4.556 6.08e-05 ***
## lx1          0.5217840  0.0690280   7.559 7.36e-09 ***
## lx2          1.1337552  0.1989418   5.699 1.93e-06 ***
## x6          -0.0032750  0.0002786 -11.757 1.04e-13 ***
## ---
## Residual standard error: 0.005775 on 35 degrees of freedom
## Multiple R-squared:  0.9982,	Adjusted R-squared:  0.9981
## F-statistic:  6550 on 3 and 35 DF,  p-value: < 2.2e-16

##
## Multicollinearity?
library(car)
vif(Jet.lm2)
##        lx1        lx2         x6
## 109.721932 108.742539   1.991382
## Uhhh....

## (Observation 20 is flagged!)
h.col <- rep("blue", nrow(Jet)); h.col[20] <- "red"
pairs(Jet[, c("lx1", "lx2", "x6")], col=h.col)

## Since both variables (lx1 and lx2) are rotational speeds, one can e.g.
## construct the following two new variables: mean rotation speed and delta
## rotation speed
Jet$lProdRG <- 0.5*(Jet$lx1 + Jet$lx2)
Jet$lQuodRG <- Jet$lx1 - Jet$lx2
pairs(Jet[, c("lProdRG", "lQuodRG", "x6")])

pairs(Jet[, c("lProdRG", "lQuodRG", "lx1", "lx2")])
## This is leading nowhere... (it might be because the variable lx1 is
## dominating the variable lx2 with respect to the range and lx2 has much
## higher values. thus, both variables  lProdRG and lQuadRG are highly
## correlated with lx1.

## Alternatives?


#
##=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=* ENDE *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=

