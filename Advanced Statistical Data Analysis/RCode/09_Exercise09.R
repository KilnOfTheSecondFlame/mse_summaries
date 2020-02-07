################################################################################
# Task 1:
################################################################################
# Define n to be the number of random draws from the prior:
n = 10000
nInvited = 16
nSignups = 6
prior = runif(n,0,1) # Here you sample n draws from the prior
hist(prior) # It’s always good to eyeball the prior to make sure it looks ok
# Define the generative model:
generativemodel = function(theta){
    rbinom(1,nInvited,theta)
}
# Simulate data using parameters from the prior and the generative model:
simdata = rep(NA, n)
for(i in 1:n) {
  simdata[i] = generativemodel( prior[i] )
}
# Filter out all draws that do not match the data:
posterior = prior[simdata == nSignups]
hist(posterior) # Eyeball the posterior
length(posterior) # Are there enought draws left after the filtering?
# There are no rules here, but you probably want to aim for >1000 draws.
# Now, summarize the posterior (posteriori mean, 90% credible interval):
mean(posterior)
quantile(posterior,c(.05,.95))


################################################################################
# Task 2:
################################################################################
sum(posterior>.2)/length(posterior)


################################################################################
# Task 3:
################################################################################
signups = rbinom(length(posterior),100,posterior)
hist(signups)
# remark: wrong solution: posterior*100
# you would neglect the variability of the binomial
# to see this, consider a very dense posterior distribution


################################################################################
# Task 4:
################################################################################
# See Task 1 with
nInvited = 160
nSignups = 60
# The maximum likelihood estimate is the same in both cases: 60/160


################################################################################
# Task 5:
################################################################################
# f(x) = 2.5 for x in [0.1,0.5], 0 else
runif(16,0.1,0.5)
# a/(a+b) =.2 and ab/((a+b+1)*(a+b)^2)=0.1^2
# => b = 4a  and 400a^2 = (5a+1)(5a)^2
# => a = 3 and b = 12
rbeta(16,3,12)
curve(dbeta(x,3,12))
# Use this prior in Task 1
# for each of the three priors, calculate
hist(posterior)
mean(posterior)
quantile(posterior,c(.05,.95))


################################################################################
# Task 6:
################################################################################
# Modify Task 1:
n = 10000
nInvited = 68+52
nSignups = 68
prior = rbeta(n,40,40)
...
# What's the probability that party A will win the election?
# remark: wrong solution: sum(posterior>0.5)/length(posterior)
# you can't say this whithout assuming/modelling the number of voters

# What's the probability that there are more supporter for party A...?
sum(posterior>0.5)/length(posterior)

# population size is 200. How many people will vote for party B?
Bvoters = rbinom(length(posterior),200, 1-posterior)
# compare with the wrong solution:
Bvoters_wrong = 200*(1-posterior)
