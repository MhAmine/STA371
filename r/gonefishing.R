library(mosaic)

gonefishing = read.csv("gonefishing.csv", header=TRUE)

npop = nrow(gonefishing)

gonefishing$volume = (gonefishing$height)*(gonefishing$length)*gonefishing$width

plot(volume~weight, data=gonefishing, pch=19, col=rgb(30,30,30,30, maxColorVal=256))

lmfull = lm(weight~volume, data=gonefishing)
coef(lmfull)


# Take a sample of size 30 from the population
# and fit a linear model to that sample

# First define the sample size
nsamp = 30

# Try taking a sample a few different times
lmsamp = lm(weight~volume, data=sample(gonefishing,30))
coef(lmsamp)

# We can automate the process of taking multiple samples

# Try 10 first
do(10)*lm(weight~volume, data=sample(gonefishing,30))


# How about 1000?
do(1000)*lm(weight~volume, data=sample(gonefishing,30))

# We can avoid the screen dump by saving the output.

montecarlo = do(1000)*lm(weight~volume, data=sample(gonefishing,30))

colMeans(montecarlo)
sd(montecarlo)

hist(montecarlo)



### Now try bootstrapping

# First get a sample of size 30
myfishingtrip = sample(gonefishing,30)

# The model using your sample
lmmytrip = lm(weight~volume, data=myfishingtrip)
coef(lmmytrip)

# Try a single bootstrapped sample from your sample

lmboot = lm(weight~volume, data=resample(myfishingtrip))
coef(lmboot)

# How about 10 bootstrapped samples?
do(10)*lm(weight~volume, data=resample(myfishingtrip))

# Now 1000
myboot = do(1000)*lm(weight~volume, data=resample(myfishingtrip))

colMeans(myboot)
sd(myboot)

hist(myboot)
