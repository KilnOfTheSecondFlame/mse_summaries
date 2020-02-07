#
## Worksheet Week 5 - Solution
## ***************************
##
## Question 1
## ~~~~~~~~~
## Poisson density
## f_lambda(y) = (lambda^y * exp(-lambda)) / (y!)
## log-density
## log(f_lambda(y)) = y*log(lambda) - lambda - log(y!)
## log-density expressed with mu (=lambda)
## log(f_mu(y)) = y*log(mu) - mu - log(y!)

## so phi=1, b(mu)=log(mu), c(mu)=mu, and d(y,phi)=-log(y!)

## V(mu) = (c''(mu)-mu*b''(mu)) / (b'(mu))^2
##       = (0 - mu*(-1/mu^2))   / (1/mu)^2   = 1/mu * mu^2 = mu


## ======================================================================
##
## Question 2
## ~~~~~~~~~
DaR <- read.table("Data/Dial-a-ride.dat", header=T)
str(DaR)


## (a)
summary(DaR)
## better (because it results in a more useful summary of fvIND)
DaR$fvIND <- as.factor(DaR$IND)
summary(DaR)

pairs(DaR)               ## better to transform some/all variables?

DaR$lPOP <- log(DaR$POP)
DaR$lAR <- log(DaR$AR)
DaR$lRDR <- log(DaR$RDR)
DaR$lHR <- log(DaR$HR)
DaR$lVH <- log(DaR$VH)
DaR$lF <- log(DaR$F)

pairs(DaR[,1:7])         ## untransformed variables
pairs(DaR[,8:ncol(DaR)]) ## transformed variables

par(mfrow=c(2,3))
plot(RDR ~ lPOP + lAR + lHR + lVH + lF + fvIND, data=DaR)




## (b)
## According to Tukey's first-aid transformations we should transform VH,
## RDR and POP by a square root:
DaR$sqrtVH <- sqrt(DaR$VH)
DaR$sqrtPOP <- sqrt(DaR$POP)
DaR$sqrtRDR <- sqrt(DaR$RDR)

## But let's start with a linear predictor based on log-transformed
## explanatory variables:
## Least squares fit on the response sqrtRDR
DaR.lm1 <- lm(sqrtRDR ~ lPOP + lAR + lHR + lVH + lF + IND, data=DaR)

## Residual and Sensitivity Analysis
source("RFn/RFn_Plot-lmSim.R")
par(mfrow=c(2,4))
plot(DaR.lm1)
plot.lmSim(DaR.lm1, SEED=1798)
## There is clear evidence that the expectation is not constant, the residuals
## are not Gaussian distributed (they are skewed to the right) and that there
## is something wrong with the variance because the structures are outside
## the stochastic fluctuation.

## We could try
DaR.lm2 <- lm(loRDR ~ lPOP + lAR + lHR + lVH + lF + IND, data=DaR)
par(mfrow=c(2,4))
plot(DaR.lm2)
plot.lmSim(DaR.lm2, SEED=1798)
## better but still some evidence that the model is not yet adequate.

## using sqrt-transformation:
DaR.lm3 <- lm(sqrtRDR ~ sqrtPOP + lAR + lHR + sqrtVH + lF + IND, data=DaR)
par(mfrow=c(2,4))
plot(DaR.lm3)
plot.lmSim(DaR.lm3, SEED=1798)
## just a little improvement to DaR.lm1

DaR.lm4 <- lm(lRDR ~ sqrtPOP + lAR + lHR + sqrtVH + lF + IND, data=DaR)
par(mfrow=c(2,4))
plot(DaR.lm4)
plot.lmSim(DaR.lm4, SEED=1798)
## just a little improvement to DaR.lm2

## you can try even some more transformations
library(robustbase)
DaR.rlm3 <- lmrob(sqrtRDR ~ sqrtPOP + lAR + lHR + sqrtVH + lF + IND, data=DaR)
par(mfrow=c(2,3))
plot(DaR.rlm3)

#



## (c)
DaR.glm1 <- glm(RDR ~ lPOP + lAR + lHR + lVH + lF + fvIND,
				   family=poisson, data=DaR)
## The response  RDR_i is independently Poisson distributed
## with expectation mu_i. The log-transformed expectation
## (i.e., the canonical link) is affected by a linear combination of
## the log-transformed explanatory variables  lPOP, lAR, lHR, lVH, lF and IND.

summary(DaR.glm1)
## Coefficients:
##              Estimate Std. Error z value Pr(>|z|)
## (Intercept)  0.707481   0.197490   3.582  0.00034 ***
## lPOP         0.203218   0.017451  11.645  < 2e-16 ***
## lAR         -0.280124   0.010691 -26.202  < 2e-16 ***
## lHR          0.694960   0.036706  18.933  < 2e-16 ***
## lVH          0.952992   0.024028  39.662  < 2e-16 ***
## lF           0.138048   0.008055  17.137  < 2e-16 ***
## fvIND1       0.858702   0.013794  62.253  < 2e-16 ***
## ---
## (Dispersion parameter for poisson family taken to be 1)
##     Null deviance: 20655.3  on 53  degrees of freedom
## Residual deviance:  3436.1  on 47  degrees of freedom
## Number of Fisher Scoring iterations: 4

## --> Fitting procedure has converged within 4 iterations


## ======================================================================
##
## Question 3
## ~~~~~~~~~~
Bak <- read.table("Data/bacteria.dat", header=T)
str(Bak)


## (a)
## response: N
## distribution: Poisson (The response is a count variable)
## explanatory variable: Time
## link function: log (given)


## (b)
Bak.glm <- glm(N ~ Time, family=poisson, data=Bak)
summary(Bak.glm)
## Just the part of the output we should be able to interpret:
## Coefficients:
##              Estimate Std. Error z value Pr(>|z|)
## (Intercept)  5.981772   0.041902  142.76   <2e-16 ***
## Time        -0.218920   0.007414  -29.53   <2e-16 ***

## Both coefficients are significant on the 5% level since both p-values
## are smaller than 0.05. Because the 2nd coef. is negative, the
## number of survivors is decreasing as exposure time is prolonged.

## The number of initial bacteria is
exp(5.981772)  ## n_0 = 396.14  --> rounded to 396


plot(Bak$Time, Bak$N, xlim=c(0,15), ylim=c(0,396))
x <- seq(0,15,length=50)
lines(x, exp(coef(Bak.glm)[1] + coef(Bak.glm)[2]*x))
## or
Bak.p <- predict(Bak.glm, newdata=data.frame(Time=x), type="response")
lines(x, Bak.p, col="red")

##
## (c)
names(summary(Bak.glm))
xx <- summary(Bak.glm)$coefficients[2,1:2]
xx[1] + c(-1,1)*1.96*xx[2]
## -0.21892 + c(-1,1)*1.96*0.007413474  = [-0.2334504, -0.2043895]

confint(Bak.glm,2) ## -0.2335835 -0.2045186
## he values do almost agree. Is this just a question of numerical inaccuracy
## or is there more to it than that?

## ======================================================================
##

## Question 4
## ~~~~~~~~~~
transa <- read.table("Data/transactions.dat", header=T)
str(transa)

## (a)
summary(transa)

par(mfrow=c(1,2))
plot(Time ~ ., data=transa)

## (b)
transa.lm <- lm(Time ~ ., data=transa)
summary(transa.lm)
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)
## (Intercept) 1.446e+04  1.705e+04   0.848    0.397
## Type1       5.463e+00  4.332e-01  12.609   <2e-16 ***
## Type2       2.034e+00  9.433e-02  21.567   <2e-16 ***
## ---
## Residual standard error: 114200 on 258 degrees of freedom
## Multiple R-squared: 0.9091,	Adjusted R-squared: 0.9084
## F-statistic:  1290 on 2 and 258 DF,  p-value: < 2.2e-16

## Residual and sensitivity analysis:
par(mfrow=c(2,4))
plot(transa.lm)
plot.lmSim(transa.lm, SEED=1848)
## - Since the smoother is within the stochastic fluctuation in Tukey-Anscombe
##   plot there is NO evidence that the expectation is not constant.
## - Since the smoother is outside the stochastic fluctuation in the
##   scale-location plot  there is evidence that the variance is not constant.
## - Since the points are outside the stochastic fluctuation in the normal
##   q-q plot there is evidence that the residuals are not Gaussian
##   distributed.
## - Since all observations have a Cook's distance smaller than 1 there is
##   no too influential observation

## These problems could possibly be solved with a suitable transformation of
## the response variable. However, this approach disagree  the model ideas.



## (c)
## The response is gamma distributed
## Link: identity
## expalantory variables: Type1 + Type2
transa.glmI  <- glm(Time ~ Type1 + Type2, family=Gamma(link=identity),
		data=transa)
summary(transa.glmI)
## Just the part of the output we should be able to interpret:
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)
## (Intercept) 1.536e+04  5.183e+03   2.964  0.00332 **
## Type1       5.705e+00  4.257e-01  13.401  < 2e-16 ***
## Type2       2.007e+00  5.803e-02  34.582  < 2e-16 ***
## ---
## (Dispersion parameter for Gamma family taken to be 0.02938966)


## ======================================================================
##
## Question 5
## ~~~~~~~~~~
nw <- read.table("Data/nambeware.txt", header=T)
summary(nw)

## (a)
nw.glm1 <- glm(Time ~ Diam + Type, family=Gamma(link=log), data=nw)

coef(nw.glm1)
## (Intercept)         Diam TypeCassDish     TypeDish    TypePlate     TypeTray
##  2.54897318   0.07670807   0.47516081   0.28939601  -0.18791439   0.14472101


## (b)
## The estimated expected response is
##   exp(2.54897 + 0.07670807*Diam + beta2)
##   = exp(2.54897) * exp(0.07670807*Diam) * exp(beta2)
## where beta2 depends on whether type is
##      Bowls (beta2=0), CassDish (beta2=0.47516), Dish (beta2=0.28940), etc.
## Thus the estimated expected response is affected by a factor exp(beta2)
## which depends on the type of product



nw.glm2 <- glm(Time ~ Diam * Type, family=Gamma(link=log), data=nw)
## is identical to     Time ~ 1 + Diam + Type + Diam : Type
summary(nw.glm2)

## Now, the estimated expected response is not just affected by a factor
## exp(beta2) but also the factor exp(beta1*Diam) depends on the type of
## product because the "slope" beta1 depends on the type of product.
h.levTy <- levels(nw$Type)
par(mfrow=c(2,3))
for(k in 1:length(h.levTy)){
    plot(Time ~ Diam, data=nw, subset=nw$Type==h.levTy[k])
    abline(lm(Time ~ Diam, data=nw, subset=nw$Type==h.levTy[k]))
    title(main=h.levTy[k])
}

## optional with lattice graphics
library(lattice)
xyplot(Time ~ Diam | Type, data=nw, col=2)

#
##=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=* END *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=

