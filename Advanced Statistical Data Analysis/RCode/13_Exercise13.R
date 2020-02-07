################################################################################
# Task 1: Stochastic integration
f = function(x){return(x^2*cos(.6*x)+.2)}
plot(NA,xlim=c(-1.2,2.2),ylim=c(0,2),xlab="x",ylab="f(x)")
points(seq(-1,2,.01),f(seq(-1,2,.01)),type="l",lwd=2)
abline(h=c(0,2),v=c(-1,2))
n = 1000
hits = 0
for(i in 1:n)
{
    x_ = runif(1,-1,2)
    y_ = runif(1,0,2)
    col="black"
    if(y_<f(x_))
    {
        hits = hits+1
        col="green"
    }
    points(x_,y_,pch=16,col=col)
}
# hits/n = I/6  =>  I = hits/n*6
hits/n*6 # Exact Integral: 2.50

################################################################################
# Task 2
# stationary distribution:
9/27  = 9/27*0.0 + 8/27*0.5 + 10/27*0.5  ok
8/27  = 9/27*0.8 + 8/27*0.1 + 10/27*0.0  ok
10/27 = 9/27*0.2 + 8/27*0.4 + 10/27*0.5  ok
# => (9/27,8/27,10/27) is stationary distribution
# detailed balance:
# no detailed balance, consider state 2 and 3: 8/27*0.4 is not 10/27*0

################################################################################
# Task 3
# stationary distribution:
m = matrix(c(.5,0,1,.25,.5,.0,.25,.5,0),3)
m = m%*%m
m
m = m%*%m
m
# m doesn't change any more
# => (0.5,0.25,0.25) is stationary distribution
# Proof this:
0.50 = 0.50*0.5  + 0.25*0.0 + 0.25*1.0  ok
0.25 = 0.50*0.25 + 0.25*0.5 + 0.25*0.0  ok
0.25 = 0.50*0.25 + 0.25*0.5 + 0.25*0.0  ok
# detailed balance:
# no detailed balance, consider state 2 and 3!

################################################################################
# Task 4
# stationary distribution:
m = matrix(c(.8,0.4,.2,.6),2)
m = m%*%m
m = m%*%m
m = m%*%m
...
m
# (2/3,1/3)  is stationary distribution, proof this as above...
# detailed balance is fulfilled:
2/3*0.2 = 1/3*0.4


################################################################################
# Task 5
# 6 from 16 sign up for salomon
# Baysian Data Analysis via MCMC
nrSignups =  6
nrTotal   = 16

nSimulations = 100000

signUpRate_i = runif(1)

sample_signUpRates = rep(NA,nSimulations)

for(i in 1:nSimulations)
{
    signUpRate_prop = runif(1)
    u = runif(1)
    if( u < (dbinom(nrSignups,nrTotal,signUpRate_prop) / dbinom(nrSignups,nrTotal,signUpRate_i)))
    {
        signUpRate_i = signUpRate_prop
    }
    sample_signUpRates[i] = signUpRate_i
}

#hist(sample_signUpRates,breaks=seq(0,1,.02),col="blue")

#plot(sample_signUpRates)
#plot(sample_signUpRates[1:100])

signUpRates_iid = sample_signUpRates[seq(100,nSimulations,50)]
hist(signUpRates_iid,breaks=seq(0,1,.02),col="blue")
abline(v=nrSignups/nrTotal,col="green",lwd=3)
abline(v=mean(signUpRates_iid),col="red",lwd=3)
abline(v=quantile(signUpRates_iid,c(0.025,.975)),col="red",lty=3,lwd=3)
