#
## Worksheet Week 4 - Solution
## ***************************
##
## Question 1
## ~~~~~~~~~
turb <- read.table("Data/turbines.dat", header=T)
str(turb)

## The R function sunflowerplot is in one of the core packages so you do
## not have to load the package sfsmisc.

## (a)
plot(turb$Hours, turb$Fissures/turb$Turbines)
## Does not show all the information given for each observation
## Still not appropriate
sunflowerplot(turb$Hours, turb$Fissures, las=1)
sunflowerplot(turb$Hours, turb$Fissures/turb$Turbines, las=1)

## much better::
sunflowerplot(x=turb$Hours, y=turb$Fissures/turb$Turbines,
				number=turb$Turbines, las=1)
symbols(x=turb$Hours, y=turb$Fissures/turb$Turbines,
		 circles=sqrt(turb$Turbines), inches=0.75/2.54)

##
## (b)
# Fit with logit link, on the left hand side there are number of (Successes,Failures)
turb.glm <- glm(cbind(Fissures, Turbines-Fissures) ~ Hours,
				family=binomial, data=turb)
coef(turb.glm)
## (Intercept)        Hours
##   -3.923597 0.0009992372


##
## (c)
# Overlay fitted line
turbN <- data.frame(Hours=seq(100, 10000, by=100))
y.p <- predict(turb.glm, newdata=turbN, type="response")
sunflowerplot(x=turb$Hours, y=turb$Fissures/turb$Turbines, las=1,
              number=turb$Turbines, xlim=c(0, 10000), ylim=c(0,1))
lines(turbN$Hours, y.p, col=3)

abline(h=c(0.1, 0.3, 0.5, 0.7, 0.9), lty=7, col=2)
## Estimated operation time at which xx% of the turbine wheels exhibits any
## fissures:
## xx=  10%,   30%,   50%,   70%,    90%
##    1'900, 3'200, 4'000, 4'500, 6'000
## these values are read from the graphic and are very crude readings.

## Note that the values for 70% and 90% are based on an extrapolation. Such an
## extrapolation just possible with a parametric model. However, the
## extrapolation is only reliable when the model is adequate!


## (d)
turb.Probit <- glm(cbind(Fissures, Turbines-Fissures) ~ Hours,
                   family=binomial(link=probit), data=turb)
coef(turb.Probit)
##   (Intercept)         Hours
## -2.2758074623  0.0005783211

turb.cll <- glm(cbind(Fissures, Turbines-Fissures) ~ Hours,
                family=binomial(link=cloglog), data=turb)
coef(turb.cll)
##   (Intercept)         Hours
## -3.6032798443  0.0008104936

## To compare the results, let's overlay the fitted lines:
y.p2 <- predict(turb.Probit, newdata=turbN, type="response")
y.p3 <- predict(turb.cll, newdata=turbN, type="response")
lines(turbN$Hours, y.p2, col="blue", lty=2)
lines(turbN$Hours, y.p3, col="magenta", lty=5)


## Comparing the coefficient estimates, the logit model and the
## complementary log-log model seem to be similar, the probit model has
## clearly different estimates.

## With respect to the fitted curves, the probit and the logit model are
## very similar, but the  complementary log-log model shows a clearly
## different curve. The comparission of the fitted curves is much more
## crutial than the comparission of the estimated coefficients.

## ======================================================================
##
## Question 2
## ~~~~~~~~~~
bw <- read.table("Data/birth-weight.dat", header=T)
str(bw)

## (a)
par(mfrow=c(1,2), oma=c(2,0,0,0), las=1)
symbols(x=bw$weight, y=bw$Y/bw$m, circles=sqrt(bw$m), inches=0.5/2.54,
        xlab="Weight", ylab="Survival Probability")
sunflowerplot(x=bw$weight, y=bw$Y/bw$m, number=bw$m, ylim=c(0,1),
              xlab="Weight", ylab="Survival Probability")
mtext(side=1, text="Premature Birth Data", outer=TRUE)


par(mfrow=c(1,1), oma=c(0,0,0,0), las=1)
## Empirical logits: We should observe a linear relationship:
yel <- log((bw$Y+0.5)/(bw$m-bw$Y+0.5))
sunflowerplot(x=bw$weight, y=yel, number=bw$m, ylab="Empirical Logits")
## This does not look like a linear relationship!

## Let's try a first-aid transformation on weight
sunflowerplot(x=log(bw$weight), y=yel, number=bw$m,
              xlab="log(Weight)", ylab="Empirical Logits")

## that is much better
## Hence
bw$lWeight <- log(bw$weight)


##
## (b) least squares using empirical logits
bw.elm <- lm(yel ~ lWeight, data=bw)

## display data and fit in the logitic scale:
sunflowerplot(x=bw$lWeight, y=yel, number=bw$m,
              xlab="log(Weight)", ylab="Empirical Logits")
abline(bw.elm, col=4, lwd=4, lty=6)
## looks fine!

## display data and fit in the response scale
sunflowerplot(x=bw$lWeight, y=bw$Y/bw$m, number=bw$m,
              xlab="log(Weight)", ylab="Survival Probability")
x <- seq(min(bw$lWeight), max(bw$lWeight), length=50)
mu.logit.p <- predict(bw.elm, newdata=data.frame(lWeight=x))
## back-transformation into the response scale:
lines(x, 1/(1+exp(-mu.logit.p)), col="blue", lwd=2, lty=6)




##
## (c)
## With GLM
bw.glm <- glm(cbind(Y, m-Y) ~ lWeight, family=binomial, data=bw)

## display data and both fits in the logitic scale:
sunflowerplot(x=bw$lWeight, y=yel, number=bw$m,
              xlab="log(Weight)", ylab="Empirical Logits")
abline(bw.elm, col="blue", lwd=4, lty=6)
abline(coef(bw.glm), col="magenta", lwd=4) ## glm fit
## Two almost paralell lines

## display data and both fits in the response scale
sunflowerplot(x=bw$lWeight, y=bw$Y/bw$m, number=bw$m,
              xlab="log(Weight)", ylab="Survival Probability")
x <- seq(min(bw$lWeight), max(bw$lWeight), length=50)
mu.logit.p <- predict(bw.elm, newdata=data.frame(lWeight=x))
lines(x, 1/(1+exp(-mu.logit.p)), col="blue", lwd=4, lty=6)
mu.glm.p <- predict(bw.glm, newdata=data.frame(lWeight=x), type="response")
lines(x, mu.glm.p, col="magenta", lwd=4)
## Difference in the fits is small.
## It might be that the glm fit fits better on the r.h.s.



### display data and both fits in a scatterplot of the response against Weights:
sunflowerplot(x=bw$weight, y=bw$Y/bw$m, number=bw$m,
              xlab="Weight", ylab="Survival Probability")
lines(exp(x), 1/(1+exp(-mu.logit.p)), col='blue', lwd=4, lty=6)
lines(exp(x), mu.glm.p, col="magenta", lwd=4) ## from GLM

#
##=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=* END *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=

