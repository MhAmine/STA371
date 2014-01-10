library(mosaic)
 
truemean = 0.25
nullmean = 0
N = 10
sigma = 1

y = rnorm(N, truemean, sigma)
z = {mean(y) - nullmean}/sd(y)
abs(z) > 2


powersim1 = do(1000)*{
  y = rnorm(N, truemean, sigma)
  ybar = mean(y)
  ybar.se = sd(y)/sqrt(N)
  z = {ybar - nullmean}/ybar.se
  abs(z) > 2
}


NMC = 1000
truemean = seq(-2,2, length=30)
result = rep(0, length(truemean))
for(i in seq_along(truemean)) {
  powersim1 = do(NMC)*{
    y = rnorm(N, truemean[i], sigma)
    ybar = mean(y)
    ybar.se = sd(y)/sqrt(N)
    z = {ybar - nullmean}/ybar.se
    abs(z) > 2
  }
  result[i] = sum(powersim1==TRUE)/NMC
}

plot(truemean, result)
