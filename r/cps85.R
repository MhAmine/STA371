library(mosaic)
data(CPS85)

# Is there a wage premium for men?
boxplot(wage~sex, data=CPS85)
mean(wage~sex, data=CPS85)
# A difference of 2.12

# What happens if we re-deal the cards?
mean(shuffle(wage)~sex, data=CPS85)

# Try this 1000 times and plot the "observed" differences in wages
permtest = do(1000)*mean(shuffle(wage)~sex, data=CPS85)
hist(permtest$M - permtest$F, xlim=c(-2.5,2.5))
abline(v=2.12, col='red')



# But could the wage premium for men be explained by differential union membership?
boxplot(wage~union, data=CPS85)
xtabs(~sex+union,data=CPS85)

# Let's plot wages stratified by both sex and union membership
boxplot(wage~(sex:union), data=CPS85)

# How to test the hypotheses that, once we adjust for union membership,
# there is no additional wage premium for men?

# First try fitting a separable model for wage versus union membership and sex
# What should the male coefficient be under the null hypothesis?
lm1 = lm(wage ~ union + sex, data=CPS85)
coef(lm1)

# Is that coefficient for men "significant"?
permtest2 = do(1000)*lm(shuffle(wage)~union+sex, data=CPS85)
hist(permtest2)
# What's the problem here?

# Re-conceptualize the problem!
permtest3 = do(1000)*lm(wage~union+shuffle(sex), data=CPS85)
hist(permtest3)