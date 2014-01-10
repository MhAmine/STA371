### Read in the orings data
orings = read.csv("http://www2.mccombs.utexas.edu/faculty/james.scott/STA371G/Data_and_Code_files/orings.csv", header=T)


lm1 = lm(Erosion~Temp,data=orings)
3.1+(-0.04)*38

plot(orings$Temp, orings$Erosion, pch=19, axes=F, xlab="Temperature at Launch", ylab='O-Ring Erosion?', cex.lab=1.3, cex.axis=1.5, main="Erosion Incidents on Previous Shuttle Flights", cex.main=1.3, xlim=c(20, 85))
axis(1, cex.axis=1.5)
axis(2, at = c(0, 1), labels=c("No", "Yes"), cex.axis=1.5)

arrows(29,0.39, 29, 0)
text(29, 0.4, "Temp at 9 AM, 1/28/86", pos=3)



### Fit a logit model

glm1 = glm(Erosion ~ Temp, data=orings, family=binomial(link="logit"))
summary(glm1)

myboot=do(1000)*glm(Erosion ~ Temp, data=resample(orings), family=binomial(link="logit"))


### Temperature at 9 AM on 1/28/1986 = 29F
mu9am = 23.87 - 0.3682 * 29

prob9am = exp(mu9am) / (1 + exp(mu9am))

### Temperature at 2 PM on 1/28/1986 = 38F
mu2pm = 23.87 - 0.3682 * 38
prob2pm = exp(mu2pm) / (1 + exp(mu2pm))


### Create a new data frame for predictions
Xnew = data.frame(Temp = seq(0,100,length=101))

mypred = predict(glm1, newdata=Xnew, type="response", se.fit = T)

### Try ?predict.glm for help

plot(Xnew$Temp, mypred$fit, type='l')
lines(Xnew$Temp, pmax(mypred$fit-2*mypred$se.fit,0), col='red')



glm2 = glm(BlowBy ~ Temp, data=orings, family=binomial(link="logit"))
summary(glm2)

mypred2 = predict(glm2, newdata=Xnew, type="response", se.fit = T)
plot(Xnew$Temp, mypred2$fit, type='l')
lines(Xnew$Temp, pmax(mypred2$fit-2*mypred$se.fit,0), col='red')


