library(mosaic)

# Read in ut2000 data
#ut2000 = read.csv('ut2000.csv', header=TRUE)

xyplot(GPA~SAT.C | School, data=ut2000)

# Example 1: do we need dummy variables for School?
lm1 = lm(GPA ~ SAT.C, data=ut2000)
lm2 = lm(GPA ~ SAT.C + School, data=ut2000)
summary(lm1)
summary(lm2)

# H0 = "GPA is unrelated to School, adjusting for SAT.C"
# We can use R^2 as a test statistic
# Get p(R^2 | H0 true) by simulation (permutation test)
permtest1 = do(1000)*lm(GPA ~ SAT.C + shuffle(School), data=ut2000)

# Visualize the sampling distribution and choose a rejection region
head(permtest1)
hist(permtest1$r.squared)
summary(lm1)
abline(v=0.1524, col='red')

# Say R = anything bigger than 0.1556
# Calculate alpha from the right tail area
pdata(0.1556, permtest1$r.squared)

# Compare with the actual (not shuffled) model
summary(lm2)

# Reject H0 and conclude that the dummy variables should be in the model


# Example 2: do we need an interaction term between SAT.C and School?
lm3 = lm(GPA ~ SAT.C + School + SAT.C:School, data=ut2000)
summary(lm3)

# test statistic = R^2
# Calculate p(R^2 | H0) by simulation
permtest2 = do(1000)*lm(GPA ~ SAT.C + School + SAT.C:shuffle(School), data=ut2000)

# Visualize the sampling distribution
hist(permtest2$r.squared)

# Choose a tail area (alpha = 0.05) and get the critical value
qdata(0.95, permtest2$r.squared)

# Compare with R^2 from actual (not shuffled) model
summary(lm3)

# Optional: calculate the p-value
hist(permtest2$r.squared)
summary(lm3)
pdata(0.1871, permtest2$r.squared)
1-pdata(0.1871, permtest2$r.squared)


# Example 3: house data

# Basic model for house price
lm4 = lm(price ~ sqft + nbhd + brick + bedrooms + bathrooms, data=house)
summary(lm4)

# H0: no interaction necessary between sqft and nbhd
# test statistic = R^2
# Get p(R^2 | H0) by simulation
permtest3 = do(1000)*lm(price ~ sqft + nbhd + brick + bedrooms + bathrooms + sqft:shuffle(nbhd), data=house)

# Visualize the sampling distribution
hist(permtest3$r.squared)

# Choose a tail area (alpha = 0.05) and get the critical value
qdata(0.95, permtest3$r.squared)

# Compare with non-shuffled model
lm5 = lm(price ~ sqft + nbhd + brick + bedrooms + bathrooms + sqft:nbhd, data=house)
summary(lm5)
