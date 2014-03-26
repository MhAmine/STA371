library(mosaic)

profs = read.csv('profs.csv', header=TRUE)

# Plots
par(mfrow=c(2,2))
plot(eval ~ beauty, data=profs)
boxplot(eval ~ minority, data=profs)
plot(eval ~ age, data=profs)
boxplot(eval ~ gender, data=profs)

boxplot(eval ~ division, data=profs)
plot(eval ~ students, data=profs)
plot(eval ~ allstudents, data=profs)
boxplot(eval ~ credits, data=profs)

plot(eval ~ log(students), data=profs)
plot(eval ~ log(allstudents), data=profs)
boxplot(eval ~ tenure, data=profs)
boxplot(eval ~ native, data=profs)

# Start with beauty, minority status, gender, division, credits, allstudents, tenure, and native

lm1 = lm(eval ~ beauty + minority + gender + division + credits + log(allstudents) + tenure + native, data=profs)
summary(lm1)

# A couple of useful functions
anova(lm1)  # How much does each term contribute to the predictable variation (PV)?
drop1(lm1)	# Consider each one-variable deletion

# The tenure coefficient is especially small, and has a small effect on R^2
# We'll try a permutation test:
perm1 = do(1000)*lm(eval ~ beauty + minority + gender + division + credits + log(allstudents) + shuffle(tenure) + native, data=profs)
hist(perm1$r.squared)

# Compare with actual model:
summary(lm1)

# Now division looks pretty useless
drop1(lm2)

# This gives us a reasonable model
lm3 = lm(eval ~ beauty + minority + gender + credits + log(allstudents) + native, data=profs)
summary(lm3)
drop1(lm3)

# Let's assess beauty in the context of this model via a permutation test
perm3 = do(1000)*lm(eval ~ shuffle(beauty) + minority + gender + credits + log(allstudents) + native, data=profs)
hist(perm3$r.squared)

# Check the model with actual beauty and see where R^2 falls
summary(lm3)

# Now check interactions with gender and division

lm4 = lm(eval ~ beauty + minority + gender + credits + log(allstudents) + native + gender:beauty, data=profs)
perm4 = do(1000)*lm(eval ~ beauty + minority + gender + credits + log(allstudents) + native + shuffle(gender):beauty, data=profs)

# We may need an interaction with gender
hist(perm4$r.squared)
summary(lm4)

lm5 = lm(eval ~ beauty + minority + gender + credits + log(allstudents) + native + gender:beauty + division:beauty, data=profs)
perm5 = do(1000)*lm(eval ~ beauty + minority + gender + credits + log(allstudents) + native + gender:beauty + shuffle(division):beauty, data=profs)

# But probably not with division
hist(perm5$r.squared)
summary(lm5)

