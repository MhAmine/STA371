library(mosaic)
library(faraway)


# Load the cars data set
#pickup=read.csv('http://www2.mccombs.utexas.edu/faculty/james.scott/STA371G/Data_and_Code_files/pickup.csv', header=TRUE)
#pickup=read.csv('pickup.csv', header=TRUE)


# How much will you have to pay for a used pickup truck?
hist(pickup$price,20, col='lightgrey', xlab="Price ($)", main="Asking price for pickup trucks on Craigslist")
price.mean = mean(pickup$price)
price.sd = sd(pickup$price)
abline(v=price.mean, col='blue', lwd=2)

# Check how many fall outside a +/- 1 standard deviation bound
sum(pickup$price < price.mean - price.sd)
sum(pickup$price > price.mean + price.sd)
nrow(pickup)


# Now introduce a predictor
plot(price~miles,data=pickup, pch=19)
lm0 = lm(price~miles,data=pickup)
abline(lm0)

# This will show several plots, only the first of which is relevant for the moment
# Pay attention to the pattern in the residuals
plot(lm0)

# Let's work with a subset where we trust a basic model
pickup2 = subset(pickup, miles < 160000)
plot(price~miles,data=pickup2, pch=19)
lm1 = lm(price~miles,data=pickup2)
abline(lm1)

# Extract the residuals
fittedprice = fitted(lm1)
residualprice = resid(lm1)
beta = coef(lm1)

# Look at a histogram of the residuals
hist(residualprice,20, col='lightgrey', xlab="Price ($)", main="Residual price after regressing on mileage")
residualsd = sd(residualprice)
abline(v=residualsd, col='blue', lwd=2)
abline(v=-residualsd, col='blue', lwd=2)



par(mfrow=c(1,1))
plot(price~miles,data=pickup2, pch=19)
abline(beta[1], beta[2])
abline(beta[1]-residualsd, beta[2], col='grey')
abline(beta[1]+residualsd, beta[2], col='grey')





## Pretty plots (for the brave)

par(mar=c(3.5,3.5,1,0), mgp=c(2.5,1,0))
hist(pickup$price, 20, xlim=c(1000,24000),
	xlab="Price ($)",
	#ylab="Frequency",
	ylab="",
	main="",
	col="lightgrey", label=TRUE,
	axes=FALSE)
#axis(2)
axis(1,at=seq(2000,24000,by=2000))

par(mar=c(3.5,3.5,1,0), mgp=c(2.5,1,0))
plot(jitter(price)~miles,data=pickup, subset=miles<160000,
	xlim=c(0,160000),
	bty='n', main="", xlab="Odometer Reading (thousands of miles)", ylab="Resale Price ($1000's)",
	cex=1.2,  pch=19, col=rgb(100,0,150,100,maxColorValue=255),
	axes=FALSE
	)
axis(1,font=2, at=seq(0, 160000,by=20000), labels=seq(0,160,by=20))
lm1 = lm(price~miles,data=pickup,subset=miles<160000)
abline(coef(lm1), col='darkgrey')
axis(2,font=2, at=seq(2000,24000,by=2000),labels=seq(2,24,by=2),las=1)







par(mar=c(3.5,3.5,1,0), mgp=c(2.5,1,0))
plot(price~miles,data=pickup,subset=miles<160000,
	bty='n', main="", xlab="Odometer Reading (thousands of miles)", ylab="Resale Price ($1000)",
	#cex=1.2,  pch=21, col="darkgreen", bg='lightgrey'
	type='n', axes=FALSE
	)
polygon(c(0, 160000, 160000, 0),
		c( 17054.1 + 2*3971.273, 17054.1 - 0.105*160000 + 2*3971.273, 17054.1 - 0.105*160000 - 2*3971.273, 17054.1 - 2*3971.273), border=NA, col=grey(0.95))
polygon(c(0, 160000, 160000, 0),
		c( 17054.1 + 1*3971.273, 17054.1 - 0.105*160000 + 1*3971.273, 17054.1 - 0.105*160000 - 1*3971.273, 17054.1 - 1*3971.273), border=NA, col=grey(0.85))
lm1 = lm(price~miles,data=pickup,subset=miles<160000)

abline(coef(lm1), col='darkgrey', lwd=2)

text(price~miles, data=pickup, subset=miles<160000, labels=make, cex=0.7, font=2)
axis(1,font=2, at=seq(0, 160000,by=20000), labels=seq(0,160,by=20))

axis(2,font=2, at=seq(2000,24000,by=2000),labels=seq(2,24,by=2),las=1)
#abline(coef(lm1) + c(sd(resid(lm1)),0), lty='dotted', col='darkgrey')
#abline(coef(lm1) - c(sd(resid(lm1)),0), lty='dotted', col='darkgrey')

