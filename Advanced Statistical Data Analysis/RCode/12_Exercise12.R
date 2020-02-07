################################################################################
# Task 1
# prior: a=9, b=0.3
a = 9
b = 0.3
curve(dgamma(x,a,b),0,100,col="cyan")
curve(dgamma(x,a+120,b+3),0,100,col="magenta",add=T)
qgamma(c(0.05,0.95),a+120,b+3)
mode = (a+120-1)/(b+3)
abline(v=mode)
# more observations
qgamma(c(0.05,0.95),a+1200,b+30)
mode = (a+1200-1)/(b+30)

################################################################################
# Task 2
# prior
curve(dnorm(x,325,2),310,340)
obs  = 1590
nObs = 5
sigma = 9
m = 325
s = 2

post_m = (m/s^2+obs/sigma^2)/(1/s^2+nObs/sigma^2)
post_s = (1/s^2+nObs/sigma^2)^-.5

curve(dnorm(x,post_m,post_s),310,340, col="magenta", lwd=5,n=1001)
curve(dnorm(x,m,s),310,340, add=T, col="cyan", lwd=5,n=1001)
qnorm(c(0.025,0.975),post_m,post_s)
abline(v=qnorm(c(0.025,0.975),post_m,post_s),col="magenta")

#ABC...

################################################################################
# Task3
# prior:
# the precision is
1/c(0.4,0.5,0.6)^2 #mean=4 and sd is approx. 2
curve(dgamma(x,8,2),0,10, col="cyan", lwd=5) #is a suitable prior with mean=4 and sd=2
# posterior on precision:
obs = c(2.3, 0.1, 1.2, 0.3, 1.3, 1.6)
curve(dgamma(x,8 + 3 , 2 + sum((obs-mean(obs))^2)/2 ),0,10,add=T, col="magenta", lwd=5)
post_precision = qgamma(c(0.05,0.5,0.95),8 + 3 , 2 + sum((obs-mean(obs))^2)/2)
11/ ( 2 + sum((obs-mean(obs))^2)/2 )
post_sd = sqrt(post_precision)^-1
#check:
sd(c(2.3, 0.1, 1.2, 0.3, 1.3, 1.6))

