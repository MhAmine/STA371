library(mosaic)

#######  UT Class of 2000 GPA/SAT data

# Read in the data set, which includes every student who
# entered the University of Texas in the Fall of 2000
# Just use the Import Dataset button under Workspace.

# What are the variables called?
names(ut2000)

# Summary statistics for each variable
summary(ut2000)

# Look at the first and last few rows
head(ut2000)
tail(ut2000)

# GPA Histogram
# Notice we reference a variable by DataSet$VariableName
histogram(ut2000$GPA)

# Compute some basic summary statistics
mean(ut2000$GPA)
sd(ut2000$GPA)
favstats(ut2000$GPA)

# Three basic bivariate plots
bwplot(GPA ~ School, data=ut2000)
dotplot(GPA ~ School, data=ut2000)
xyplot(GPA ~ SAT.C, data=ut2000)

# A lattice plot (three variables)
xyplot(GPA ~ SAT.C | School, data=ut2000)

# Interactive plotting
mPlot(ut2000)


# Compute mean and standard deviation of scores stratified by college
# These tables are in the notes
mean(SAT.Q ~ School, data=ut2000)
mean(SAT.V ~ School, data=ut2000)

sd(SAT.Q ~ School, data=ut2000)
sd(SAT.V ~ School, data=ut2000)

# Store the results of these computations in other variables
satqmeans = mean(SAT.Q ~ School, data=ut2000)
satqmeans
