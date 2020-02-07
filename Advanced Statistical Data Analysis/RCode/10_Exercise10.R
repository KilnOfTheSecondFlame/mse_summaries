### Exercise 1-2

nSamples = 100000

# Ex01
asked = 16
SingnupA = 6
SingnupB = 10

# Ex02
asked = 32
SingnupA = 12
SingnupB = 20

# Simulate prior
# Case 1
priorA = runif(nSamples,0,1)
priorB = runif(nSamples,0,1)
# Case 2
priorA = rbeta(nSamples,2,4)
priorB = rbeta(nSamples,2,4)
# Case 3
priorA = rbeta(nSamples,3,25)
priorB = rbeta(nSamples,3,25)

# Simulate generative model (likelihood)
simSingnupA = rbinom(nSamples,asked,priorA)
simSingnupB = rbinom(nSamples,asked,priorB)

# Condition on observed data
ind = ( (simSingnupA==SingnupA) & (simSingnupB==SingnupB) )
margPosteriorA = priorA[ind]
margPosteriorB = priorB[ind]

# evaluate results
plot(margPosteriorA,margPosteriorB,cex=1,pch=16,xlim=c(0,1),ylim=c(0,1), col='red')
abline(0,1, col='blue')

par(mfcol=c(2,1))
hist(margPosteriorA,xlim=0:1)
abline(v=mean(margPosteriorA),col="red")
abline(v=quantile(margPosteriorA,c(0.05,0.95)),col="blue")
hist(margPosteriorB,xlim=0:1)
abline(v=mean(margPosteriorB),col="red")
abline(v=quantile(margPosteriorB,c(0.05,0.95)),col="blue")
par(mfcol=c(1,1))



### Exercise 5
# Find prior by searching
b=1:5
(4*b-3)*b/(5*b-2)/(5*b-3)^2
# => b = 3 => a = 9
curve(dbeta(x,9,3))
# Which earphones have a higher probability that you like them?
likes_ABTest = function(nSamples = 100)
{
    likesAB = matrix(NA,nrow=nSamples,ncol=2)
    j = 1
    for(i in 1:10000000)
    {
        xA = rbeta(1,9,3)
        xB = rbeta(1,9,3)
        nLikesA = rbinom(1,550,xA)
        nLikesB = rbinom(1,23,xB)
        if(nLikesA == 500 && nLikesB == 21)
        {
           likesAB[j,] = c(xA,xB)
           if(j>=nSamples)
           {
               break;
           }
           j = j+1
        }
    }
    print(i)
    invisible(likesAB)
}
# get 1000 samples
likes_ABTest(1000) -> result
plot(result[,1],result[,2],xlim=c(.5,1),ylim=c(.5,1), col='red')
abline(0,1,col='blue')
sum(result[,1]>result[,2])

mean(result[,1])
mean(result[,2])

# Probability that you like earphones A
a = rbinom(1000,1,result[,1])
sum(a)/1000
#Probability that you like earphones B
b = rbinom(1000,1,result[,2])
sum(b)/1000

# independent observations are more efficient to simulate
likes_ABTest = function(nSamples = 100)
{
    likesAB = matrix(NA,nrow=nSamples,ncol=2)
    j = 1
    for(i in 1:10000000)
    {
        xA = rbeta(1,9,3)
        nLikesA = rbinom(1,550,xA)
        if(nLikesA == 500)
        {
            likesAB[j,1] = xA
            if(j>=nSamples)
            {
                break;
            }
            j = j+1
        }
    }
    j=1
    for(i in 1:10000000)
    {
        xB = rbeta(1,9,3)
        nLikesB = rbinom(1,23,xB)
        if(nLikesB == 21)
        {
            likesAB[j,2] = xB
            if(j>=nSamples)
            {
                break;
            }
            j = j+1
        }
    }
    print(i)
    invisible(likesAB)
}



### Exercise 6
# you should use an uninformative flat prior beta(1,1), because there is
# given no prior information

seriesTossing = function(nSamples = 100)
{
    posteriorHead = rep(NA,nSamples)
    j = 1
    for(i in 1:10000000)
    {
        priorHead     = rbeta(1,1,1)
        observations  = rbinom(10,1,priorHead)
        lenghtrun = max(rle(observations)$lengths)
        if(lenghtrun >= 7)
        {
           posteriorHead[j] = priorHead
           if(j>=nSamples)
           {
               break;
           }
           j = j+1
        }
    }
    print(i)
    invisible(posteriorHead)
}
# get 1000 samples
seriesTossing(10000) -> posterior
hist(posterior,breaks=50,col="magenta")
abline(v=.5)

abline(v=quantile(posterior,c(.45,.55)))
quantile(posterior,c(.45,.55))

# Answer:
# c = .29



### Exercise 7
# 2stageCoinTossing
CoinTossing = function(nSamples = 100)
{
    posteriorHead = rep(NA,nSamples)
    j = 1
    for(i in 1:10000000)
    {
        priorHead = rbeta(1,12,12)
        observations  = rbinom(20,2,priorHead)
        nHeads = sum(observations==2)
        nBoth  = sum(observations==1)
        nTails = sum(observations==0)
        if(nHeads == 7 && nBoth == 10 && nTails==3)
        {
           posteriorHead[j] = priorHead
           if(j>=nSamples)
           {
               break;
           }
           j = j+1
        }
    }
    print(i)
    invisible(posteriorHead)
}
# get 1000 samples
CoinTossing(1000) -> result
hist(result,breaks=30,col="magenta")
abline(v=.5)

sum(result>0.5)/length(result)

