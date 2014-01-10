# Load the mosaic library after installing
library(mosaic)

# Now we read in the data (in .txt format)
# tab-delimited
rxntime = read.table("rxntime.txt", header=TRUE, sep="\t")


###################################
# Some initial exploratory analysis
###################################


summary(rxntime)

# Compute the group-wise means
mean(PictureTarget.RT ~ FarAway, data=rxntime)
mean(PictureTarget.RT ~ Littered, data=rxntime)

sd(PictureTarget.RT ~ FarAway, data=rxntime)
sd(PictureTarget.RT ~ Littered, data=rxntime)


# Some plots to show between-group and within-group variation
boxplot(PictureTarget.RT ~ FarAway, data=rxntime)
stripchart(PictureTarget.RT ~ Littered, data=rxntime, method='jitter', vertical=TRUE, jitter=.01)


###################################
# A couple of one-way ANOVA models
###################################

# First by whether the scene was littered
lm1 = lm(PictureTarget.RT ~ Littered, data=rxntime)
coef(lm1)

# Compare the model coefficients with the group means
mean(PictureTarget.RT ~ Littered, data=rxntime)

# Analysis of variance
anova(lm1)

# Can pick off R^2 directly from the summary command
summary(lm1)

# Now by subject
# Notice the factor command
# Used to tell R that Subject is a category label, not a number
lm2 = lm(PictureTarget.RT ~ factor(Subject), data=rxntime)
anova(lm2)
summary(lm2)


###################################
# Looking at more than one grouping factor
###################################

# Stratify means and variances by two categories
# Notice that the design is balanced: 480 in each group
mean(PictureTarget.RT ~ Littered + FarAway, data=rxntime)

# Can also compute standard deviations by group
sd(PictureTarget.RT ~ Littered+FarAway, data=rxntime)


# Fit a model with main effects only
# That is, where the Littered anf FarAway effects are separable
lm3 = lm(PictureTarget.RT ~ Littered+FarAway, data=rxntime)

# Examine the coefficients
coef(lm3)


lm3int = lm(PictureTarget.RT ~ Littered+FarAway+Littered:FarAway, data=rxntime)

lm3int = lm(PictureTarget.RT ~ Littered*FarAway, data=rxntime)


# And the variance decomposition.
anova(lm3)

# What if we flipped the order?
lm3b = lm(PictureTarget.RT ~ FarAway + Littered, data=rxntime)

# Notice that the coefficients are the same.
# This will always be true.
coef(lm3); coef(lm3b)

# The variance decomposition is also the same.
# This will NOT always be true!
# Here it is a consequence of balanced design -- no collinearity.
anova(lm3);
anova(lm3b)


boxplot(PictureTarget.RT~factor(Subject), data=rxntime)

boxplot(resid(lm3)~factor(Subject), data=rxntime)



