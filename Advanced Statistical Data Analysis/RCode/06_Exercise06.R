#
## Worksheet Week 6 - Solution
## ***************************
##
## Question 1
## ~~~~~~~~~
##
baby <- read.table("Data/baby.dat", header=T)

summary(baby)
stem(baby$Apgar1)
par(mfrow=c(2,2))
for(k in 3:5) sunflowerplot(baby[,k], baby$Survival, ylim=c(-.1,1.1))


## (a)
baby.fit1 <- glm(Survival ~ ., family=binomial, baby)
summary(baby.fit1)
## Coefficients:
##               Estimate Std. Error z value Pr(>|z|)
## (Intercept) -3.0933685 14.3053767  -0.216   0.8288
## Weight       0.0037341  0.0008468   4.410 1.03e-05 ***
## Age          0.1588001  0.0761061   2.087   0.0369 *
## Apgar1       0.1159864  0.1108339   1.046   0.2953
## Apgar5       0.0611499  0.1202222   0.509   0.6110
## pH          -0.7380214  1.8964578  -0.389   0.6972
## ---
## (Dispersion parameter for binomial family taken to be 1)
##     Null deviance: 319.28  on 246  degrees of freedom
## Residual deviance: 236.14  on 241  degrees of freedom


## No, just Age is significant on the 5% level.
## Apgar1, Apgar5 and pH are not significant at the 5% level because the
## corresponding p-value is less than 5%. Consequently, at least one variable
## may be omitted, possibly all three.


## Since (from the summary output)
1-pchisq(319.28-236.14, df=246-241)  ## = 2.220446e-16
## is smaller than the significant level of 5%, we cannot drop all explanatory
## variables. At least one of them is significant.
## or without plugging in the numbers explicitly:
(h <- summary(baby.fit1)$null.deviance - summary(baby.fit1)$deviance)
1 - pchisq(h, 246-241) ## = 2.220446e-16

## Note that this test is identical to
baby.fit0 <- glm(Survival ~ 1, family=binomial, baby)
anova(baby.fit1, baby.fit0, test="Chisq")
## ...
##   Resid. Df Resid. Dev Df Deviance  Pr(>Chi)
## 1       241     236.14
## 2       246     319.28 -5  -83.137 < 2.2e-16 ***


## (b) AIC - stepwise procedure
step(baby.fit1, scope=list(upper=~ Weight + Age + Apgar1 + Apgar5 + pH,
                           lower=~1))
## ...
##          Df Deviance    AIC
## <none>        236.56 244.56
## - Apgar1  1   239.85 245.85
## + Apgar5  1   236.29 246.29
## + pH      1   236.40 246.40
## - Age     1   241.56 247.56
## - Weight  1   259.10 265.10

## Call:  glm(formula = Survival ~ Weight + Age + Apgar1, family = binomial,
##     data = baby)

## Coefficients:
## (Intercept)       Weight          Age       Apgar1
##   -8.484190     0.003791     0.165297     0.142989

## Degrees of Freedom: 246 Total (i.e. Null);  243 Residual
## Null Deviance:	    319.3
## Residual Deviance: 236.6 	AIC: 244.6

## Two variables (Agpar5 and pH) have been dropped.


## Optional with BIC (Bayesian Information Criterion)
## (Because the penalty of model complexity (i.e., the number of the parameters)
## is greater, this criterion usually results in a smaller optimal model.
## k: the multiple of the number of degrees of freedom used for the penalty. Only k = 2 gives the genuine AIC: k = log(n) is sometimes referred to as BIC or SBC.

step(baby.fit1, scope=list(upper=~ Weight + Age + Apgar1 + Apgar5 + pH,
                           lower=~1), k=log(nrow(baby)))
## ...
##          Df Deviance    AIC
## <none>        244.62 255.63
## + Age     1   239.85 256.38
## + Apgar1  1   241.56 258.09
## + Apgar5  1   241.94 258.47
## + pH      1   244.57 261.10
## - Weight  1   319.28 324.79

## Call:  glm(formula = Survival ~ Weight, family = binomial, data = baby)

## Coefficients:
## (Intercept)       Weight
##   -4.669478     0.005152

## Degrees of Freedom: 246 Total (i.e. Null);  245 Residual
## Null Deviance:	    319.3
## Residual Deviance: 244.6 	AIC: 248.6

## Just the explanatory variable Weight has a significant influence on the
## response.


## (c)
baby.fit2 <- glm(Survival ~ Weight + Age, family=binomial, baby)

anova(baby.fit1, baby.fit2, test="Chisq")
## Model 1: Survival ~ Weight + Age + Apgar1 + Apgar5 + pH
## Model 2: Survival ~ Weight + Age
##   Resid. Df Resid. Dev Df Deviance P(>|Chi|)
## 1       241     236.14
## 2       244     239.85 -3  -3.7091    0.2946

## Since the p-value of 0.2946 is larger than 5% we have no evidence against
## the null hypothesis; i.e., there is no significant difference between these
## two models. Therefore, we assume that the reduced model
## "Survival ~ Weight + Age" describes the data statistically equally well as
## the full one.


## (optional) p-value fishing (i.e., find statistical significant results)
baby.fit3 <- glm(Survival ~ Weight, family=binomial, baby)

## (i)
anova(baby.fit1, baby.fit3, test="Chi")  ## p-value = 0.07568
## (ii)
anova(baby.fit2, baby.fit3, test="Chi")  ## p-value = 0.02905

## If you want to show that there is no evidence against the model
## "Survival ~ Weight" you use the result of (i)
## If you do not like the model "Survival ~ Weight", you will prefer the
## test in (ii)

## Hence, null hypothesis, alternative and test statistic have to be defined
## before data are collected!


## ======================================================================
##
## Question 2
## ~~~~~~~~~
tm <- read.table("Data/twomodes.dat", header=T)
str(tm)
pairs(tm)
## An interpretation of the scatterplots is difficult because there are too
## few observations

## (a)
## The response is independent and Poisson distributed. The text suggests to
## use the "identity" link, because one rather wants a direct influence of the
## operating time on the failure rate in each mode. This choice is supported
## by the fact that both operating times are positive explanatory variables,
## and thus, with positive parameter values, the linear predictor is also
## positive. Therefore, the link "identity" guarantees a positive failure rate.
## -- But the log link is not excluded by these arguments!

## glm(Failures ~ Mode1 + Mode2, family=poisson(link="identity"), data=tm)

## (b)
tm.glm <- glm(Failures ~ Mode1 + Mode2, family=poisson(link="identity"),
              data=tm)
summary(tm.glm)
## Coefficients:
##             Estimate Std. Error z value Pr(>|z|)
## (Intercept)  5.99773    3.63545   1.650  0.09899 .
## Mode1        0.12081    0.04578   2.639  0.00832 **
## Mode2        0.05459    0.06356   0.859  0.39037
## ---
## (Dispersion parameter for poisson family taken to be 1)
##     Null deviance: 16.9964  on 8  degrees of freedom
## Residual deviance:  4.1971  on 6  degrees of freedom

## As hoped for, the coefficients have  positive signs
## (-> positive linear predictor).


## (c)
## According to the marginal tests in the summary output in (b) both the
## intercept and the coefficient of Mode2 are not significant on the 5% level.
## So we might suspect that the model "Failures ~ Mode1 -1" may be adequate.

## It is imprudent not to include the intercept. Of course, it must be assumed
## (at least theoretically) that with an operating time of 0 hours no failures
## will occur. However, in practical application, it has been repeatedly shown
## that the intercept collects systematic errors in both the response and the
## explanatory variables! (For example, it is not clear whether booting time
## should be part of operating time or not.) Accordingly, the intercept must
## also be interpreted!

## As suggested:
tm.glmA <- glm(Failures ~ 0 + Mode1, family=poisson(link="identity"), data=tm)
summary(tm.glmA) ##
## Do you understand why there is an 'Inf' in the 'null deviance' line?
##     Null deviance:    Inf  on 9  degrees of freedom

anova(tm.glm , tm.glmA , test="Chisq")
## Model 1: Failures ~ Mode1 + Mode2
## Model 2: Failures ~ -1 + Mode1
##   Resid. Df Resid. Dev Df Deviance P(>|Chi|)
## 1         6     4.1971
## 2         8     9.5237 -2  -5.3265   0.06972 .

## At the 5% level, there is no significant difference between these two
## models as the p-value of 0.0697 is greater than 5%. One can therefore
## assume that the smaller model describes the data just as well.



## Optional: Let's assume another hypothesis:
## The influence of both variables Mode1 and Mode2 is equal on the response;
## i.e., beta1 = beta2:
tm$Mode <- tm$Mode1 + tm$Mode2
tm.glmB <- glm(Failures ~ Mode, family=poisson(link="identity"), data=tm)
summary(tm.glmB) ##
## okay

anova(tm.glm , tm.glmB, test="Chisq")
## Since the p-value of 0.4918 is larger than 5% we cannot reject the null
## hypothesis that the two parameters beta1 and beta2 are equal. Hence hence
## we have no evidence that the second models describes the data less
## adequately than the first model.


## ======================================================================
##
## Question 3
## ~~~~~~~~~~
nw <- read.table("Data/nambeware.txt", header=T)
str(nw)
summary(nw)

## (a)
nw.glm1 <- glm(Time ~ Diam + Type, family=Gamma(link=log), data=nw)
summary(nw.glm1)
nw.glm2 <- glm(Time ~ Diam * Type, family=Gamma(link=log), data=nw)
summary(nw.glm2)

anova(nw.glm1, nw.glm2, test="Chisq")
## Model 1: Time ~ Diam + Type
## Model 2: Time ~ Diam * Type
##   Resid. Df Resid. Dev Df Deviance P(>|Chi|)
## 1        53     4.5039
## 2        49     3.9210  4  0.58292    0.1442

## Notice that P(>|Chi|) is calculated based on deviance/phi:
summary(nw.glm2)$dispersion ## =0.0851398
1-pchisq(0.58292/0.0851398, 4) # = 0.144219

## or with F test (notice the discussion in the lecture)
anova(nw.glm1, nw.glm2, test="F")
## Model 1: Time ~ Diam + Type
## Model 2: Time ~ Diam * Type
##   Resid. Df Resid. Dev Df Deviance      F Pr(>F)
## 1        53     4.5039
## 2        49     3.9210  4  0.58292 1.7117 0.1625

## Notice that P(>F) is calculated as
1-pf(1.7117, 4, 53) # = 0.1611457

## Although the resulting p-values are slightly different, we get the same
## result at a level of 5%: Since the p-value of 0.144 and 0.163,
## respectively, is larger than 5 % we have no evidence against the null
## hypothesis "all interaction coefficients equal 0". Therefore we use the
## reduced model.


## ======================================================================
##
## Question 4
## ~~~~~~~~~~

chal <- read.table("Data/O-rings.dat", header=T)
str(chal)

sunflowerplot(chal$Temp, chal$Fails/chal$m, number=chal$m, las=1)

## (a)
## Response is binomial distributed with expectation mu=pi_i and size=m_i
## Link = canonical link because there is none mentioned explicitly.
##        but alternative: complementary log-log link
##        because the topic is material fatigue

## glm(cbind(Fail, n-Fail) ~ Pres + Temp, family=binomial(link=logit), ...)


## (b)
chal.glm1 <- glm(cbind(Fails, m-Fails) ~ Pres + Temp,
                 family=binomial(link=logit), data=chal)
## The relevance of leak-check pressure (Pres) is unclear; hence
chal.glm2 <- glm(cbind(Fails, m-Fails) ~ Temp, family=binomial(link=logit),
                 data=chal)
anova(chal.glm1, chal.glm2, test="Chisq")
##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)
## 1        20     16.565
## 2        21     18.086 -1  -1.5212   0.2174

## This model comparison, based on the deviance statistics, shows that the
## two models are indistinguishable at the 5% level, since the P-value of 0.217
## is greater than 5%. We will therefore use the reduced model in practice.
## However, we do not know what risk of using the reduced model is (i.e. the
## probability of type II error)


## We come to the same conclusion when we look at the marginal Wald statistics
## in the summary output
## p-value we focus on is marked with '<---'
summary(chal.glm1)
## Coefficients:
##              Estimate Std. Error z value Pr(>|z|)
## (Intercept)  3.409728   3.178539   1.073   0.2834
## Pres         0.007380   0.006447   1.145   0.2523    ( <----)
## Temp        -0.107747   0.044648  -2.413   0.0158 *
## ---
##     Null deviance: 24.230  on 22  degrees of freedom
## Residual deviance: 16.565  on 20  degrees of freedom

## Both statistical tests are based on asymptotic results. However, empirical
## studies show that the approximation is better with the deviance statistic
## than with the Wald statistic. Thus, we can trust the results using the
## deviance test more. In this case, however, we come to the same conclusion.

##
## Testing using the confidence intervals
## Wald:
h <- summary(chal.glm1)$coefficients
h[2,1] + c(-1,1)*qnorm(0.975) * h[2,2]  ##  -0.0053  0.0200
## The 95% confidence interval  covers the null hypothesis 'beta1 = 0'.
## Hence we have no evidence against the null hypothesis.

## Deviance (via profiling):
confint(chal.glm1)
##                    2.5 %      97.5 %
## (Intercept) -2.776236540  9.93358512
## Pres        -0.004030283  0.02272544
## Temp        -0.201164111 -0.02229717

## This 95% confidence interval ([-0.004030283,  0.02272544]) covers the null
## hypothesis 'beta1 = 0' as well. So we obtain the same conclusion.



## (c) 95% confidence interval of mu at a temperature of 31?F
##     using model chal.glm2
(h1 <- predict(chal.glm2, newdata=data.frame(Temp=31), type="response", se=T))
h1$fit  ## 0.8177744
h1$fit + c(-1,1) * qnorm(0.975)*h1$se.fit
## 0.346496 1.289053
## This confidence interval is large and parts of it are outside the
## support [0,1]

## better:
h2 <- predict(chal.glm2, newdata=data.frame(Temp=31), type="link", se=T)
h2a <- h2$fit + c(-1,1)*qnorm(0.975)*h2$se.fit
1/(1 + exp(-h2a))
## [1] 0.1596025 0.9906582
## The 95% confidence interval covers almost the whole support except the area
## color to 0. But the probability that an o-ring will fail may be close to 1!
## There is not much confidence that the o-ring will sustain.

## optional:
## Since the object chal.glm2 knows which family and which link is used we can
## do the calculation also in this way:
h2 <- predict(chal.glm2, newdata=data.frame(Temp=31), type="link", se=T)
family(chal.glm2)$linkinv(h2$fit + c(-1,1)*qnorm(0.975)*h2$se.fit)
## [1] 0.1596025 0.9906582


## (d)
## Repeat the analysis based on with just those observations in which at least
## one failure occurred.

chal1 <- chal[chal$Fails > 0,]

chal1.glm1 <- glm(cbind(Fails, m-Fails) ~ Pres + Temp,
                  family=binomial(link=logit), data=chal1)
chal1.glm2 <- glm(cbind(Fails, m-Fails) ~ Temp,
                  family=binomial(link=logit), data=chal1)
chal1.glm0 <- glm(cbind(Fails, m-Fails) ~ 1,
                  family=binomial(link=logit), data=chal1)

## model simplification
anova(chal1.glm1, chal1.glm2, test="Chisq")
## Model 1: cbind(Fail, n - Fail) ~ Pres + Temp
## Model 2: cbind(Fail, n - Fail) ~ Temp
##   Resid. Df Resid. Dev Df Deviance P(>|Chi|)
## 1         4     1.1070
## 2         5     1.3339 -1 -0.22684    0.6339
anova(chal1.glm1, chal1.glm0, test="Chisq")
## Model 1: cbind(Fail, n - Fail) ~ Pres + Temp
## Model 2: cbind(Fail, n - Fail) ~ 1
##   Resid. Df Resid. Dev Df Deviance P(>|Chi|)
## 1         4     1.1070
## 2         6     1.3347 -2 -0.22765    0.8924

step(chal1.glm1)
## Final model:  cbind(Fail, n - Fail) ~ 1, family = binomial(link = logit)

## Indeed, neither variable, especially the temperature, seems to influence
## our response. So we can start at any temperature.

##  95% confidence interval for the probability of a defect o-ring
h10 <- predict(chal1.glm0, newdata=data.frame(Temp=31), type="link", se=T)
family(chal.glm2)$linkinv(h10$fit + c(-1,1)*qnorm(0.975)*h10$se.fit)
## 0.1154411 0.3630300 (*)

## to compare with CI based on chal.glm2
h12 <- predict(chal1.glm2, newdata=data.frame(Temp=31), type="link", se=T)
family(chal.glm2)$linkinv(h12$fit + c(-1,1)*qnorm(0.975)*h12$se.fit)
## [1] 0.009704477 0.873707558
## this one is much wider than the one in (*)

## Based on this "reduced" dataset, one could easily be convinced that
## temperature does not affect O-ring performance. Hence, based on this
## "reduced" dataset the conclusion which the scientists and engineers drew
## was correct.

## But, when you conduct a statistical analysis on a sample of the available
## data, you can induce what in statistics is known as a sample selection
## problem. Running an analysis on less than the entire data set is not always
## a problem, but it can lead to mistaken conclusions depending on the question
## you are trying to answer.

## Lessons learned:
## (1) Be very, very careful when predicting "out-of-sample" support.
## (2) Don't sample when all data points are available ... all launches, not
##     just ones with O-ring distress.


## (e)
new.chal <- data.frame(Temp=seq(h.xlim[1], h.xlim[2], length=50))
h.predGLM2 <- predict(chal.glm2, newdata=new.chal, type="response")
h.pred1GLM2 <- predict(chal1.glm2, newdata=new.chal, type="response")

h.xlim <- c(30, max(chal$Temp))
sunflowerplot(chal$Temp, chal$Fails/chal$m, number=chal$m, las=1,
              xlim=h.xlim, ylim=c(0,1))
## fit
lines(new.chal$Temp, h.predGLM2, col="blue")
lines(new.chal$Temp, h.pred1GLM2, col="red")
legend(x=50, y=1, legend=c("Fit using all data", "Fit using reduced dataset"),
       col=c("blue", "red"), lty=c(1,1))

## confidence intervals
h2 <- predict(chal.glm2, newdata=data.frame(Temp=31), type="link", se=T)
h2.ci <- family(chal.glm2)$linkinv(h2$fit + c(-1,1)*qnorm(0.975)*h2$se.fit)
h12 <- predict(chal1.glm2, newdata=data.frame(Temp=31), type="link", se=T)
h12.ci <- family(chal1.glm2)$linkinv(h12$fit + c(-1,1)*qnorm(0.975)*h12$se.fit)

lines()
lines(c(31,31), h2.ci, col="blue", lwd=2)
lines(c(31.3,31.3), h12.ci, col="red", lwd=2)
## Both confidence intervals are huge indicating that there is a great
## uncertainty in the predicted probabilities independent of the applied fit.

##
##=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=* END *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=

