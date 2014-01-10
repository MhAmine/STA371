# Import data set
# sumsq=read.csv('sumsq.csv', header=TRUE)

# a function to compute the mean absolute deviation
meanad = function(vec)
{
	mu = mean(vec)
	n = length(vec)
	return( sum( abs(vec-mu) )/n)
}

plot(sumsq$x, sumsq$y1)

lm1 = lm(y1~x, data=sumsq)
meanad(sumsq$y1)
meanad(fitted(lm1))
meanad(resid(lm1))

sd(sumsq$y1)
sd(fitted(lm1))
sd(resid(lm1))

