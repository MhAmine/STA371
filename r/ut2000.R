library(mosaic)
library(lattice)
library(ggplot2)

#######  UT Class of 2000 GPA/SAT data

# Read in the data set, which includes every student who
# entered the University of Texas in the Fall of 2000

# Just use the Import Dataset button under Workspace.

# ut2000 = read.csv("ut2000.csv", header=T)


# You could also grab it off the web
#ut2000 = read.csv("http://www2.mccombs.utexas.edu/faculty/james.scott/STA371G/Data_and_Code_files/ut2000.csv", header=T)

levels(ut2000$School)


# Rename the levels to avoid labels that YELL AT YOU
ut2000$School = factor(ut2000$School, labels=c("Architecture", "Business", "Communications", "Education", "Engineering", "Fine Arts", "Liberal Arts", "Natural Science", "Nursing", "Social Work"))

# The attach command allows R to see the names of the variables in ut2000
attach(ut2000)

# What are the variables called?
names(ut2000)

# Look at the first few rows of ut2000 to get a sense of what it looks like
head(ut2000)
tail(ut2000)

# Could also pick out specific rows
ut2000[1:10,1:6]

# Compute mean and standard deviation of scores stratified by college
# These tables are in the notes
mean(SAT.Q ~ School)
mean(SAT.V ~ School)

sd(SAT.Q ~ School)
sd(SAT.V ~ School)

# Store the sample size in a variable called N
N = nrow(ut2000)

# Boxplots stratified by college
boxplot(SAT.Q ~ School, data=ut2000)
# Clearly within-group variability is bigger than between-group variability

# The qplot function in the ggplot2 package is great.
# It tends to give nice output with minimal fuss.
qplot(School, SAT.Q, data=ut2000, geom="boxplot")

# You can also change the default color theme.
theme_set(theme_bw())
qplot(School, SAT.Q, data=ut2000, geom="boxplot")



# Although you can also hand-code just about everything in an R plot.
par(mar=c(8,4,4,1))
boxplot(SAT.Q ~ School, data=ut2000,
        las=3, frame.plot=FALSE, axes=FALSE,
        ylab="SAT Math Score",
        main="SAT Math Scores for Entering Class of 2000, by College"
        )
axis(2)
axis(1,tick=FALSE, las=2, at=1:10, labels=levels(ut2000$School))






# Now get both SAT.Q and SAT.V on the same plot
par(mar=c(7.5,4,1,1))
boxplot(SAT.V ~ School, data=ut2000, at = 1:10 - 0.15, las=3, boxwex=0.25,
ylab="SAT Score", main="", col="lightblue", axes=FALSE, xlim=c(0.8,10.2), frame.plot=FALSE)
boxplot(SAT.Q ~ School, data=ut2000, at = 1:10 + 0.15, las=3, boxwex=0.25, add=TRUE,col="red", axes=FALSE)
legend("topright", pch=15, col=c("red", "lightblue"), cex=1.0, legend=c("Math", "Verbal"), bty='n')
axis(2)
axis(1,tick=FALSE, las=2, at=1:10, labels=levels(ut2000$School))

####
# Can anyone design a better plot that facilitates all the relevant comparisons?
####



par(mar=c(10,4,4,1))
boxplot(GPA ~ School, data=ut2000, las=3, ylab="Graduating GPA", main="Graduating GPA for Entering Class of 2000, by College")

par(mar=c(10,4,4,1))
boxplot(SAT.C ~ School, data=ut2000, las=3, ylab="Combined SAT Score", main="SAT Scores for Entering Class of 2000, by College")


### Now make some scatter plots

### First, GPA versus SAT Math
plot(GPA~SAT.Q, data=ut2000)

# Add a fitted line
lmQ = lm(GPA~SAT.Q,data=ut2000)
abline(lmQ)

# A slightly better version
plot(GPA~SAT.Q, data=ut2000,
     pch=19, col='grey')
abline(lmQ)

# A prettier version
par(mar=c(4,4,0,0))
plot(jitter(ut2000$SAT.Q,2), ut2000$GPA, xlab="SAT Math Score", ylab="Graduating GPA",
	main = "", cex=0.5,  pch=21, col="darkgrey", bg='lightgrey', axes=TRUE, bty='n',las=1,xlim=c(300,800))
abline(lmQ)


# SAT Verbal
plot(GPA~SAT.V, data=ut2000,
     pch=19, col='grey')
lmV = lm(GPA~SAT.V,data=ut2000)
abline(lmV)


# Now for only the College of Liberal Arts

plot(GPA~SAT.Q, data=subset(ut2000,School=="Liberal Arts"),
     pch=19, col='grey')
lmQ.LA = lm(GPA~SAT.Q,data=subset(ut2000,School=="Liberal Arts"))
abline(lmQ.LA)

plot(GPA~SAT.V, data=subset(ut2000,School=="Liberal Arts"),
     pch=19, col='grey')
lmV.LA = lm(GPA~SAT.V,data=subset(ut2000,School=="Liberal Arts"))
abline(lmV.LA)

# What if we wanted to stratify by college?

# We could color-code things
qplot(SAT.Q, GPA, data=ut2000, geom="point", color=School)

# Messy! Better is to a trellis plot, aka a lattice plot.
xyplot(GPA~SAT.Q | School)
xyplot(GPA~SAT.V | School)





#############
###### Below you will find prettier versions of some of the plots from above.
#############

### Now, GPA versus SAT Verbal
par(mar=c(4,4,0,0))
plot(jitter(ut2000$SAT.V,2), ut2000$GPA, xlab="SAT Verbal Score", ylab="Graduating GPA",
	main = "", cex=0.5,  pch=21, col="darkgrey", bg='lightgrey', axes=TRUE, bty='n',las=1,xlim=c(300,800))
	
lmV = lm(GPA~SAT.V,data=ut2000)
abline(lmV)


### Now, GPA versus SAT Combined
par(mar=c(4,4,0,0))
plot(jitter(ut2000$SAT.C,2), ut2000$GPA, pch=19, xlab="SAT Verbal Score", ylab="Graduating GPA",
	main = "", cex=0.5, col="darkgrey", axes=TRUE, bty='n',las=1,xlim=c(600,1600))
	
lmC = lm(GPA~SAT.C,data=ut2000)
abline(lmC)



### Now, GPA versus SAT Math for Liberal Arts

plot(GPA~SAT.Q, data=subset(ut2000,School=="Liberal Arts"))


# A prettier version
par(mar=c(4,4,0,0))
plot(GPA~jitter(SAT.Q,2),data=subset(ut2000, School=="Liberal Arts"), xlab="SAT Math Score", ylab="Graduating GPA",
	main = "", cex=0.5,  pch=21, col="darkgrey", bg='lightgrey', axes=TRUE, bty='n',las=1,xlim=c(300,800))
lmQ = lm(GPA~SAT.Q,data=subset(ut2000,School=="Liberal Arts"))
abline(lmQ)

par(mar=c(4,4,0,0))
plot(GPA~jitter(SAT.V,2),data=ut2000,subset=which(School=="Liberal Arts"), xlab="SAT Math Score", ylab="Graduating GPA",
	main = "", cex=0.5,  pch=21, col="darkgrey", bg='lightgrey', axes=TRUE, bty='n',las=1,xlim=c(300,800))
lmV = lm(GPA~SAT.V,data=ut2000,subset=which(School=="Liberal Arts"))
abline(lmV)




par(mar=c(4,4,0,0))
plot(ut2000$SAT.C, ut2000$GPA, pch=19, xlab="SAT Combined Score", ylab="Graduating GPA", main = "", cex=0.6, col=rgb(0,100,0,80,maxColorValue=256), axes=FALSE, bty='n')
mylm=lm(GPA~SAT.C, data=ut2000)
abline(mylm$coefficients, lwd=1.5)
axis(1, at=seq(600, 1600,by=50), las=1, cex.axis=0.7, tick=FALSE)
axis(2, at = seq(1,4, by=0.1), cex.axis=0.7, las=1, tick=FALSE)
#for(i in seq(1,4, by=0.2)) {abline(h=i, col=grey(0.95))}


par(mfrow=c(2,1), mar=c(10,4,4,1))
boxplot(GPA ~ School, data=ut2000, las=3, ylab="Graduating GPA", main="Graduating GPA for Entering Class of 2000, by College")
boxplot(SAT.C ~ School, data=ut2000, las=3, ylab="Combined SAT Score", main="SAT Scores for Entering Class of 2000, by College")
dev.off()



subset = which(ut2000$School == "Engineering")
plot(ut2000$GPA[subset] ~ ut2000$SAT.C[subset], pch=19, xlab="SAT Combined Score", ylab="Graduating GPA", main = "GPA versus SAT for UT Entering Class of 2000", cex=0.5)
subset = which(ut2000$School == "Communications")
points(ut2000$GPA[subset] ~ ut2000$SAT.C[subset], pch=19, cex=0.5, col="red")
subset = which(ut2000$School == "Business")
points(ut2000$GPA[subset] ~ ut2000$SAT.C[subset], pch=19, cex=0.5, col="blue")






