library(mosaic)
birthwt = read.csv("birthwt.csv", header=TRUE)

plot(BWT~AGE, data=birthwt)
lm1 = lm(BWT~AGE, data=birthwt)
abline(lm1)

summary(lm1)

# Compare the p value with what you'd get from a permutation test
perm1 = do(1000)*lm(BWT~shuffle(AGE), data=birthwt)

hist(perm1)
