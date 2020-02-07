#############################################################
# Task 1
# Simulated data-points from rnorm(10,3,2)
obs = c(2.27,  1.17,  3.58,  3.95,  3.13, -1.50,  4.71,  2.58,  4.05, 5.20)
# Posterior
logStatDist = function(mu,sigma)
{
  -3*log(sigma)-4*sigma-(mu-1)^2/200-(sum((obs-mu)^2)/(2*sigma^2))
}

m = rnorm(1,1,10)
s = rgamma(1,8,4)
m_sample = c()
s_sample = c()
# Baysian Data Analysis via MCMC
for(i in 1:100000)
{
  mean_prop = rnorm(1,m,.1)
  sd_prop   = abs(rnorm(1,s,.1))
  u = runif(1,0,1)
  if( u < exp(logStatDist(mean_prop,sd_prop)-logStatDist(m,s)) )
  {
    m = mean_prop
    s   = sd_prop
  }
  m_sample = c(m_sample,m)
  s_sample = c(s_sample,s)
}
# Monitor acceptance rate:
# proportion of samples without a change
sum(diff(m_sample)==0) #sum(abs(diff(m_sample))<=1e-08)
# acceptance rate, maybe a bit to high, resulting in to hihgly correlated samples
1-sum(diff(m_sample)==0)/length(m_sample)

# To propose candidates from a wider neighbourhood, change line 16 and 17 for example to
mean_prop = rnorm(1,m,.2)
sd_prop   = abs(rnorm(1,s,.2))

# to update just one parameter at a time, change line 16 and 17 for example to
if(runif(1)<0.5)
{
  mean_prop = rnorm(1,m,.2)
  sd_prop   = s
}
else
{
  mean_prop = m
  sd_prop   = abs(rnorm(1,s,.2))
}
# now you have to be careful, when monitoring the acceptance rate in each case

#########################################################################
# Task 2
# 6 from 16 sign up for salomon
# Baysian Data Analysis via MCMC
nrSignups =  6
nrTotal   = 16

nSimulations = 100000

signUpRate_i = runif(1) # don't have to change the initial starting value

sample_signUpRates = c()

for(i in 1:nSimulations)
{
  signUpRate_prop = runif(1) # don't have to change the proposal distribution
  u = runif(1,0,1)
  # change the prior here
  if( u < (dbinom(nrSignups,nrTotal,signUpRate_prop)*dbeta(signUpRate_prop,2,20) / (dbinom(nrSignups,nrTotal,signUpRate_i)*dbeta(signUpRate_i,2,20))))
  {
    signUpRate_i = signUpRate_prop
  }
  sample_signUpRates = c(sample_signUpRates, signUpRate_i)
}
# Traceplot
plot(sample_signUpRates[1:2000],type="l")
# Histogram
signUpRates_iid = sample_signUpRates[seq(100,nSimulations,100)]
hist(signUpRates_iid,breaks=seq(0,1,.02),col="magenta")
abline(v=nrSignups/nrTotal,col="green",lwd=3)
abline(v=mean(signUpRates_iid),col="red",lwd=3)
abline(v=quantile(signUpRates_iid,c(0.025,.975)),col="red",lty=3,lwd=3)
mean(signUpRates_iid)
quantile(signUpRates_iid,c(0.025,.975))


#########################################################################
# Task 3
mcmcSimLinReg = function()
{
  x = c(3.4, -4.2, -0.7, -2.6, -1.6, -1.2, -2.2, -3.7, -0.9, -3.1)
  y = c(5.7, -0.1,  4.7, 2.7,  3.1,  2.3,  3.4,  1.0,  2.2,  1.5)
  # Baysian Data Analysis via MCMC
  b0_sample = c()
  b1_sample = c()
  s_sample  = c()
  # Function to calculate the Metropolis Hastings Ratio
  R = function(b0_prop, b1_prop, s_prop, b0_i, b1_i, s_i)
  {
    llh_prop = sum(dnorm(b0_prop+b1_prop*x-y, 0, s_prop, log = TRUE))
    logPosterior_prop = llh_prop + dnorm(b0_prop, 0, 3, log = TRUE)+ dnorm(b1_prop, 0, 3, log = TRUE)
    llh_i = sum(dnorm(b0_i+b1_i*x-y, 0, s_i, log = TRUE))
    logPosterior_i = llh_i + dnorm(b0_i, 0, 3, log = TRUE)+ dnorm(b1_i, 0, 3, log = TRUE)
    return(exp(logPosterior_prop-logPosterior_i))
  }
  # start values
  b0_i = rnorm(1)
  b1_i = rnorm(1)
  s_i  = runif(1)
  for(i in 1:100000)
  {
    b0_prop =     rnorm(1,b0_i,.2)
    b1_prop =     rnorm(1,b1_i,.2)
    s_prop  = abs(rnorm(1,s_i,.2))
    u = runif(1,0,1)
    if( u < R(b0_prop,b1_prop,s_prop,b0_i,b1_i,s_i) )
    {
      b0_i = b0_prop
      b1_i = b1_prop
      s_i  = s_prop
    }
    b0_sample = c(b0_sample, b0_i)
    b1_sample = c(b1_sample, b1_i)
    s_sample  = c(s_sample , s_i)
  }
  return(list(b0_sample=b0_sample,b1_sample=b1_sample,s_sample=s_sample))
}
d = mcmcSimLinReg()
for(i in 1:3)
{
  d_ = d[[i]][seq(1000,100000,300)]
  print(mean(d_))
  print(quantile(d_,c(0.05,0.95)))
}



##################################################################################
# Task 4
# change this function to compare results:
logStatDist = function(p)
{
  40*log(p/2+0.25) + 80*log((1-p)/2+0.25) # with uniform prior
  #40*log(p/2+0.25) + 80*log((1-p)/2+0.25) + dbeta(p,1,10,log=TRUE) # with beta prior
}
# number of samples
n = 100000
# Starting value
p = rbeta(1,1,2)
# Collect sample
p_sample = rep(NA,n)
#  Bayesian Data Analysis via MCMC
for(i in 1:n)
{
  p_prop = rbeta(1,1,2)
  R =  exp(logStatDist(p_prop)-logStatDist(p))
  u = runif(1,0,1)
  if( u < R)
  {
    p = p_prop
  }
  p_sample[i] = p
}

#Evaluation
hist(p_sample[1:1000*100],col="magenta")
abline(v=mean(p_sample[1:1000*100]),col="red")
mean(p_sample[1:1000*100])
abline(v=quantile(p_sample[1:1000*100],c(0.05,0.95)),col="red",lty=3)
quantile(p_sample[1:1000*100],c(0.05,0.95))
