################################################################################
# Task 1:
# a/(a+b) =.1 and ab/((a+b+1)*(a+b)^2)=0.05^2
# b = 9a and a = (9/0.25 -1)/10 = 3.5
# a = 3.5 b = 31.5
n=100000
postType1 = rbeta(n,3.5 +  8,31.5 + 92)
postType2 = rbeta(n,3.5 + 12,31.5 + 88)

plot(postType1, postType2)
abline(0,1)
sum(postType1< postType2)/n

expType1 = (3.5 +   8)/ (3.5 +   8 +31.5 + 92)
expType2 = (3.5 +  12)/ (3.5 +  12 +31.5 + 88)

(1-expType1)
(1-expType2)/.9

# Type 2 is better than Type 1.

################################################################################
# Task 2
# We know nothing about the failure rate,
# so we assume a flat prior:
sumObs = 5+8+3+9+2+6
qbeta(c(0.05,0.95),1+6,1+sumObs)
1-pbeta(0.25,1+6,1+sumObs)

################################################################################
# Task 3
sumObsM1 = 10+15+18+20+5+12+3
sumObsM2 = 23+16+19+28+37

qbeta(c(0.025,0.975),1+7,20+sumObsM1)
qbeta(c(0.025,0.975),1+5,20+sumObsM2)

expM1 = 8/(28+sumObsM1)
expM2 = 6/(26+sumObsM2)

mapM1 = 7/(26+sumObsM1)
mapM2 = 5/(24+sumObsM2)

################################################################################
# Task 5
curve(dgamma(x,5+55,1+10),0,10)
abline(v=qgamma(c(0.05,0.95),5+55,1+10))
qgamma(c(0.05,0.95),5+55,1+10)

n=100000
# simulate the requests per day
request = rpois(n,rgamma(n,5+55,1+10))
hist(request,breaks=30,col="blue")
# are there more than 5 requests per day in more than 50% of all days?
sum(request>5)/n
# no, not beneficial

################################################################################
# Task 6
library("rmutil")
# prior: a=2, b=6
plot(dbetabinom(0:100,100,2/8,8))
# posterior: a=2+3, b=6+7
plot(3:93,dbetabinom(0:90,90,5/18,18))
abline(v=3+qbetabinom(c(0.05,0.95),90,5/18,18))
3+qbetabinom(c(0.05,0.95),90,5/18,18)
