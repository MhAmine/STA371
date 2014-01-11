library(mosaic)

## Read in the Fearless Critic ratings for Austin restaurants
## in the vicinity of downtown

# Read in from .csv file

# If getting from the web
# afc = read.csv("http://www2.mccombs.utexas.edu/faculty/james.scott/STA371G/Data_and_Code_files/afc-downtown.csv", header=TRUE)

summary(afc)

hist(afc$Price)

hist(afc$FoodScore)

plot(Price~FoodScore,data=afc)

# Fit a straight line to the data by least squares
lm1 = lm(Price~FoodScore, data=afc)

# We can use the model for plug-in predictions
# Look at the coefficients and add them to the scatterplot
coef(lm1)
abline(lm1)

# We can also use the model to take the x-ness out of y
# Look at the data, fitted values, and residuals next to each other
data.frame(afc$Name, afc$Price, fitted(lm1), resid(lm1))




# Plot the data and residuals side by side
par(mfrow=c(1,2))
plot(Price~FoodScore, data=afc)
abline(lm1)
plot(resid(lm1)~FoodScore, data=afc)

# Look at actual values and residuals on the same scale
par(mfrow=c(2,1))
hist(afc$Price - mean(afc$Price),20, xlim=c(-60,85))
hist(resid(lm1),20, xlim=c(-60,85))

# Compare their standard deviations
sd(afc$Price)
sd(resid(lm1))

# Superimpose the fitted values and actual data
plot(Price~FoodScore, col='grey', data=afc)
points(lm1$fitted~FoodScore,data=afc)



#### And now all the pretty plots

par(mar=c(4,4,0,1))
plot(jitter(Price)~jitter(FoodScore), data=afc, pch=21,bg='grey', bty='n',
	xlab="Austin Fearless Critic food rating", ylab="Price (dollars/person)",
	main="", cex=1.2,
	xlim=c(1,10), axes=FALSE, ylim=c(0,130)
	)
axis(2, at=seq(0,130,by=10), las=1)
axis(1, at=1:10)
lm1 = lm(Price~FoodScore, data=afc)
abline(coef(lm1))

#identify(afc$Price~afc$FoodScore)



xstar = data.frame(FoodScore=7.5)
par(mar=c(4,4,0,1), mgp=c(2.5,1,0))
plot(jitter(Price)~jitter(FoodScore), data=afc, pch=19, col=grey(0.92), bty='n',
	xlab="Austin Fearless Critic food rating", ylab="Price (dollars/person)",
	main="", cex=1.2,
	xlim=c(1,10), axes=FALSE, ylim=c(0,80)
	)
axis(2, at=seq(0,80,by=10), las=1, pos=0.7)
axis(1, at=1:10, pos=0)
lm1 = lm(Price~FoodScore, data=afc)
abline(coef(lm1))
lines(c(7.5,7.5), c(0,predict.lm(lm1,xstar)), lty='dotted')
lines(c(7.5,0.7), c(predict.lm(lm1,xstar),predict.lm(lm1,xstar)), lty='dotted')
points(7.5,0, pch=15, cex=2.0, col='red')
points(7.5,predict.lm(lm1,xstar),cex=2.0, pch=15, col='red')
points(0.7,predict.lm(lm1,xstar),cex=2.0, pch=15, col='red')
text(7.5,5,"1) Start at the X where \nyou want to predict.", pos=4, font=2)
text(7.5,46,"2) Go vertically \nup to the line.", pos=4, font=2)
text(0.8,58,"3) Go horizontally to the Y axis.\nRead off your prediction.", pos=4, font=2)



# # afc$ComboScore = afc$FoodScore + afc$FeelScore
# par(mar=c(4,4,0,1))
# plot(jitter(Price)~jitter(ComboScore), data=afc, pch=21,bg='grey', bty='n',
	# xlab="Austin Fearless Critic food rating", ylab="Price (dollars/person)",
	# main="",
	# xlim=c(5,20), axes=FALSE, ylim=c(0,130)
	# )
# axis(2, at=seq(0,130,by=10), las=1)
# axis(1, at=5:20)
# lm2 = lm(Price~ComboScore, data=afc)
# abline(coef(lm2))

#identify(afc$Price~afc$FoodScore)









par(mar=c(4,4,0,2), mfrow=c(1,2))
plot(jitter(Price)~FoodScore, data=afc, pch=21,bg='grey',
	xlab="Food rating", ylab="Price (dollars/person)",
	main="",
	xlim=c(1,10), axes=FALSE, ylim=c(0,130)
	)
axis(2, at=seq(0,130,by=10), las=1)
axis(1, at=1:10)
lm1 = lm(Price~FoodScore, data=afc)
abline(coef(lm1))
lines(c(afc$FoodScore[3], afc$FoodScore[3]), c(afc$Price[3],fitted(lm1)[3]), lty="dotted")

points(afc$FoodScore[3],afc$Price[3], pch=21,bg='red', cex=1.2)
text(afc$FoodScore[3],afc$Price[3]-0.5,"Franklin \n BBQ", pos=1, cex=0.85)

par(mar=c(4,5,0,1))
plot(resid(lm1)~jitter(FoodScore), data=afc, pch=21,bg='grey', bty='n',
	xlab="Food rating", ylab="Residual (dollars/person)",
	main="",
	xlim=c(1,10), axes=FALSE, ylim=c(-60,60)
	)
abline(h=0)
axis(2, at = seq(-60,60,by=10),las=1)
axis(1, at=1:10)
lines(c(afc$FoodScore[3], afc$FoodScore[3]), c(0,resid(lm1)[3]), lty="dotted")
points(afc$FoodScore[3],resid(lm1)[3], pch=21,bg='red', cex=1.2)
text(afc$FoodScore[3],resid(lm1)[3]-0.5,"Franklin BBQ", pos=2, cex=0.85)
	
	
	
	
## Look by neighborhood
par(mar=c(4,9,4,1))
stripchart(FoodScore~Neighborhood,data=afc,
	vertical=FALSE, las=1,
	pch=21, bg='grey')

## What would be a better way to organize this?



myord = order(max(FoodScore~Neighborhood,data=afc)$S)
NeighbOrdered = factor(afc$Neighborhood,levels=levels(afc$Neighborhood)[myord])
par(mar=c(4,9,4,1))
stripchart(FoodScore~NeighbOrdered,data=afc,
	vertical=FALSE, las=1,
	pch=21, bg='grey')


